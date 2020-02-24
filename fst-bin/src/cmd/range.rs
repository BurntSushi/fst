use std::ffi::OsString;
use std::io;
use std::path::PathBuf;

use bstr::ByteVec;

use crate::util;
use crate::Error;

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    outputs: bool,
    start: Option<OsString>,
    end: Option<OsString>,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            outputs: m.is_present("outputs"),
            start: m.value_of_os("start").map(|v| v.to_os_string()),
            end: m.value_of_os("end").map(|v| v.to_os_string()),
        })
    }

    fn run(&self) -> Result<(), Error> {
        let fst = unsafe { util::mmap_fst(&self.input)? };
        let mut q = fst.range();
        if let Some(ref start) = self.start {
            q = q.ge(Vec::from_os_str_lossy(start));
        }
        if let Some(ref end) = self.end {
            q = q.le(Vec::from_os_str_lossy(end));
        }
        util::print_stream(io::BufWriter::new(io::stdout()), self.outputs, q)
    }
}
