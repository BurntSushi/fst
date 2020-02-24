use std::io;
use std::path::PathBuf;

use bstr::{BString, ByteVec};
use fst::automaton::Levenshtein;

use crate::{util, Error};

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    query: String,
    distance: u32,
    outputs: bool,
    start: Option<BString>,
    end: Option<BString>,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            query: m
                .value_of_os("query")
                .map(|v| v.to_string_lossy().into_owned())
                .unwrap(),
            distance: m.value_of_lossy("distance").unwrap().parse()?,
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
        let fst = unsafe { util::mmap_fst(&self.input)? };
        let lev = Levenshtein::new(&self.query, self.distance)?;
        let mut q = fst.search(&lev);
        if let Some(ref start) = self.start {
            q = q.ge(start);
        }
        if let Some(ref end) = self.end {
            q = q.le(end);
        }
        util::print_stream(io::BufWriter::new(io::stdout()), self.outputs, q)
    }
}
