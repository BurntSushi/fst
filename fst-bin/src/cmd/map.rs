use std::cmp;
use std::fs;
use std::path::{Path, PathBuf};

use fst::MapBuilder;
use serde::Deserialize;

use crate::merge::Merger;
use crate::util;
use crate::Error;

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug, Deserialize)]
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
    max: bool,
    min: bool,
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
            max: m.is_present("max"),
            min: m.is_present("min"),
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
        let mut map = MapBuilder::new(wtr)?;
        for input in &self.input {
            let mut rdr = csv::ReaderBuilder::new()
                .has_headers(false)
                .from_reader(util::get_reader(Some(input))?);
            for row in rdr.deserialize() {
                let (key, val): (String, u64) = row?;
                map.insert(key, val)?;
            }
        }
        map.finish().map_err(From::from)
    }

    fn run_unsorted(&self) -> Result<(), Error> {
        let inputs = self
            .input
            .iter()
            .map(|inp| Path::new(inp).to_path_buf())
            .collect();
        let keys = util::ConcatCsv::new(inputs);

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
        if self.max {
            merger = merger.value_merger(|x, y| cmp::max(x, y));
        } else if self.min {
            merger = merger.value_merger(|x, y| cmp::min(x, y));
        } else {
            merger = merger.value_merger(|x, y| x + y);
        }
        merger.merge()
    }
}
