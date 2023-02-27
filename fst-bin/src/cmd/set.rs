use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use bstr::io::BufReadExt;
use fst::SetBuilder;

use crate::merge::Merger;
use crate::util;
use crate::Error;

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: Vec<PathBuf>,
    output: PathBuf,
    force: bool,
    sorted: bool,
    fd_limit: u32,
    batch_size: u32,
    threads: Option<u32>,
    tmp_dir: Option<PathBuf>,
    keep_tmp_dir: bool,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        let threads = m.value_of_lossy("threads").unwrap().parse()?;
        Ok(Args {
            input: m
                .values_of_os("input")
                .unwrap()
                .map(PathBuf::from)
                .collect(),
            output: m.value_of_os("output").map(PathBuf::from).unwrap(),
            force: m.is_present("force"),
            sorted: m.is_present("sorted"),
            fd_limit: m.value_of_lossy("fd-limit").unwrap().parse()?,
            batch_size: m.value_of_lossy("batch-size").unwrap().parse()?,
            threads: if threads == 0 { None } else { Some(threads) },
            tmp_dir: m.value_of_os("tmp-dir").map(PathBuf::from),
            keep_tmp_dir: m.is_present("keep-tmp-dir"),
        })
    }

    fn run(&self) -> Result<(), Error> {
        if !self.force && fs::metadata(&self.output).is_ok() {
            anyhow::bail!("Output file already exists: {:?}", self.output);
        }
        if self.sorted {
            self.run_sorted()
        } else {
            self.run_unsorted()
        }
    }

    fn run_sorted(&self) -> Result<(), Error> {
        let wtr = util::get_buf_writer(Some(&self.output))?;
        let mut set = SetBuilder::new(wtr)?;
        for input in &self.input {
            let mut rdr = util::get_buf_reader(Some(input))?;
            rdr.for_byte_line(|line| {
                if line.is_empty() {
                    return Ok(false);
                }
                set.insert(line)
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
                Ok(true)
            })?;
        }
        set.finish().map_err(From::from)
    }

    fn run_unsorted(&self) -> Result<(), Error> {
        let inputs = self
            .input
            .iter()
            .map(|inp| Path::new(inp).to_path_buf())
            .collect();
        let keys = util::ConcatLines::new(inputs)
            .map(|result| result.map(|line| (line, 0)).map_err(From::from));

        let mut merger = Merger::new(keys, &self.output);
        merger = merger.fd_limit(self.fd_limit);
        merger = merger.batch_size(self.batch_size);
        merger = merger.keep_tmp_dir(self.keep_tmp_dir);
        if let Some(threads) = self.threads {
            merger = merger.threads(threads);
        }
        if let Some(ref tmp_dir) = self.tmp_dir {
            merger = merger.tmp_dir(tmp_dir);
        }
        merger.merge()
    }
}
