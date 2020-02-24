use std::fs;
use std::path::PathBuf;

use crate::{util, Error};

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: Vec<PathBuf>,
    output: PathBuf,
    force: bool,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m
                .values_of_os("input")
                .unwrap()
                .map(PathBuf::from)
                .collect(),
            output: m.value_of_os("output").map(PathBuf::from).unwrap(),
            force: m.is_present("force"),
        })
    }

    fn run(&self) -> Result<(), Error> {
        if !self.force && fs::metadata(&self.output).is_ok() {
            anyhow::bail!(
                "Output file already exists: {}",
                self.output.display()
            );
        }

        let wtr = util::get_buf_writer(Some(&self.output))?;
        let mut merged = fst::SetBuilder::new(wtr)?;

        let mut sets = vec![];
        for set_path in &self.input {
            let fst = unsafe { util::mmap_fst(set_path)? };
            sets.push(fst::Set::from(fst));
        }
        let union = sets.iter().collect::<fst::set::OpBuilder>().union();
        merged.extend_stream(union)?;
        merged.finish()?;
        Ok(())
    }
}
