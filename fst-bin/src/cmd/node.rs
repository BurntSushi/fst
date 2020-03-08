use std::io::Write;
use std::path::PathBuf;

use crate::{util, Error};

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    node_address: usize,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            node_address: m.value_of_lossy("node-address").unwrap().parse()?,
        })
    }

    fn run(&self) -> Result<(), Error> {
        let mut wtr = util::get_buf_writer::<&str>(None)?;
        let fst = unsafe { util::mmap_fst(&self.input)? };
        let node = fst.node(self.node_address);
        writeln!(wtr, "{:?}", node)?;
        Ok(())
    }
}
