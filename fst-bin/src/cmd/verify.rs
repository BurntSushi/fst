use std::path::PathBuf;

use crate::{util, Error};

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args { input: m.value_of_os("input").map(PathBuf::from).unwrap() })
    }

    fn run(&self) -> Result<(), Error> {
        let fst = unsafe { util::mmap_fst(&self.input)? };
        fst.verify()?;
        Ok(())
    }
}
