use std::fs;
use std::path::Path;

use docopt::Docopt;
use fst::SetBuilder;
use lines::linereader::LineReader;
use serde::Deserialize;

use crate::merge::Merger;
use crate::util;
use crate::Error;

const USAGE: &'static str = "
Creates an ordered set backed by a finite state transducer.

The input to this command should be one or more files with one key per line.

If your input is already sorted, then pass the --sorted flag to make
construction much faster.

Usage:
    fst set [options] <input>... <output>
    fst set --help

Options:
    -h, --help         Display this message.
    -f, --force        Overwrites the output if a file already exists.
    --sorted           Set this if the input data is already lexicographically
                       sorted. This will make construction much faster.
                       Note that when this is set, all of the options below are
                       ignored/not relevant.
    --fd-limit ARG     The maximum number of file descriptors to have open in
                       a single worker thread. [default: 15]
    --batch-size ARG   The number of keys to collect in each batch.
                       N.B. This is the primary factor in how much memory
                       this process uses.
                       [default: 100000]
    --threads ARG      The number of simultaneous workers to run.
                       This defaults to the number of logical CPUs reported
                       by your system.
    --tmp-dir ARG      A temporary directory used to store intermediate
                       transducers. This defaults to the default temporary
                       directory reported by your system.
    --keep-tmp-dir     Does not delete the temporary directory. Useful for
                       debugging.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_input: Vec<String>,
    arg_output: String,
    flag_force: bool,
    flag_sorted: bool,
    flag_fd_limit: u32,
    flag_batch_size: u32,
    flag_threads: Option<u32>,
    flag_tmp_dir: Option<String>,
    flag_keep_tmp_dir: bool,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(&argv).deserialize())
        .unwrap_or_else(|e| e.exit());
    if !args.flag_force && fs::metadata(&args.arg_output).is_ok() {
        fail!("Output file already exists: {}", args.arg_output);
    }
    if args.flag_sorted {
        run_sorted(&args)
    } else {
        run_unsorted(&args)
    }
}

fn run_sorted(args: &Args) -> Result<(), Error> {
    let wtr = util::get_buf_writer(Some(&args.arg_output))?;
    let mut set = SetBuilder::new(wtr)?;
    for input in &args.arg_input {
        let rdr = util::get_buf_reader(Some(input))?;
        let mut lines = LineReader::new(rdr);
        loop {
            let line = lines.read_line()?;
            if line.is_empty() {
                break;
            }
            let off = if line.len() >= 2 && line[line.len() - 2] == b'\r' {
                2
            } else {
                1
            };
            set.insert(&line[0..line.len() - off])?;
        }
    }
    set.finish().map_err(From::from)
}

fn run_unsorted(args: &Args) -> Result<(), Error> {
    let inputs = args
        .arg_input
        .iter()
        .map(|inp| Path::new(inp).to_path_buf())
        .collect();
    let keys = util::ConcatLines::new(inputs)
        .map(|result| result.map(|line| (line, 0)).map_err(From::from));

    let mut merger = Merger::new(keys, &args.arg_output);
    merger = merger.fd_limit(args.flag_fd_limit);
    merger = merger.batch_size(args.flag_batch_size);
    merger = merger.keep_tmp_dir(args.flag_keep_tmp_dir);
    if let Some(threads) = args.flag_threads {
        merger = merger.threads(threads);
    }
    if let Some(ref tmp_dir) = args.flag_tmp_dir {
        merger = merger.tmp_dir(tmp_dir);
    }
    merger.merge()
}
