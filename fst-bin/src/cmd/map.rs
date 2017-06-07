use std::cmp;
use std::fs;
use std::path::Path;

use csv;
use docopt::Docopt;
use fst::MapBuilder;

use merge::Merger;
use util;
use Error;

const USAGE: &'static str = "
Creates an ordered map backed by a finite state transducer.

The input to this command should be a CSV file with exactly two columns.
The first column should be the key and the second column should be a value
that can be interpreted as an unsigned 64 bit integer.

Usage:
    fst map [options] <input>... <output>
    fst map --help

Options:
    -h, --help         Display this message.
    -f, --force        Overwrites the output if a file already exists.
    --sorted           Set this if the input data is already lexicographically
                       sorted. This will make construction much faster.
    --max              When building an FST from unsorted data, this merges
                       output values by taking the max. The default is to sum
                       them.
    --min              When building an FST from unsorted data, this merges
                       output values by taking the min. The default is to sum
                       them.
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
    flag_max: bool,
    flag_min: bool,
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
    let wtr = try!(util::get_buf_writer(Some(&args.arg_output)));
    let mut map = try!(MapBuilder::new(wtr));
    for input in &args.arg_input {
        let rdr = try!(util::get_reader(Some(input)));
        let mut rdr = csv::ReaderBuilder::new()
            .has_headers(false)
            .from_reader(rdr);
        for row in rdr.deserialize() {
            let (key, val): (String, u64) = try!(row);
            try!(map.insert(key, val));
        }
    }
    try!(map.finish());
    Ok(())
}

fn run_unsorted(args: &Args) -> Result<(), Error> {
    let inputs =
        args.arg_input
        .iter().map(|inp| Path::new(inp).to_path_buf()).collect();
    let keys = util::ConcatCsv::new(inputs);

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
    if args.flag_max {
        merger = merger.value_merger(|x, y| cmp::max(x, y));
    } else if args.flag_min {
        merger = merger.value_merger(|x, y| cmp::min(x, y));
    } else {
        merger = merger.value_merger(|x, y| x + y);
    }
    merger.merge()
}
