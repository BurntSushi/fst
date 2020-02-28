use std::io;
use std::path::PathBuf;

use bstr::{BString, ByteVec};
use regex_automata::dense;

use crate::{util, Error};

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    regex: String,
    outputs: bool,
    start: Option<BString>,
    end: Option<BString>,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            regex: m
                .value_of_os("regex")
                .map(|v| v.to_string_lossy().into_owned())
                .unwrap(),
            outputs: m.is_present("outputs"),
            start: m
                .value_of_os("start")
                .map(|v| Vec::from_os_str_lossy(v).into_owned().into()),
            end: m
                .value_of_os("end")
                .map(|v| Vec::from_os_str_lossy(v).into_owned().into()),
        })
    }

    fn run(&self) -> Result<(), Error> {
        let reverse =
            std::env::var("FST_BIN_DFA_REVERSE").map_or(false, |v| v == "1");
        let minimize =
            std::env::var("FST_BIN_DFA_MINIMIZE").map_or(false, |v| v == "1");
        let fst = unsafe { util::mmap_fst(&self.input)? };
        let dense_dfa = dense::Builder::new()
            .anchored(true)
            .minimize(minimize)
            .byte_classes(true)
            .premultiply(true)
            .reverse(reverse)
            .build(&self.regex)?;
        let dfa = match dense_dfa {
            dense::DenseDFA::PremultipliedByteClass(dfa) => dfa,
            _ => unreachable!(),
        };
        let mut q = fst.search(&dfa);
        if let Some(ref start) = self.start {
            q = q.ge(start);
        }
        if let Some(ref end) = self.end {
            q = q.le(end);
        }
        util::print_stream(io::BufWriter::new(io::stdout()), self.outputs, q)
    }
}
