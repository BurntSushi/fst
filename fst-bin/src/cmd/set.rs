use std::io::BufRead;

use docopt::Docopt;
use fst::SetBuilder;

use util;
use Error;

const USAGE: &'static str = "
Creates an ordered set backed by a finite state transducer.

Usage:
    fst set [options] <input> <output>
    fst set --help

Options:
    -h, --help       Display this message.
    --sorted         Set this if the input data is already lexicographically
                     sorted. This will make construction much faster.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_input: String,
    arg_output: String,
    flag_sorted: bool,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).decode())
                            .unwrap_or_else(|e| e.exit());
    if args.flag_sorted {
        run_sorted(&args)
    } else {
        run_unsorted(&args)
    }
}

fn run_sorted(args: &Args) -> Result<(), Error> {
    let rdr = try!(util::get_buf_reader(Some(&args.arg_input)));
    let wtr = try!(util::get_buf_writer(Some(&args.arg_output)));
    let mut set = try!(SetBuilder::new(wtr));
    for line in rdr.lines() {
        try!(set.insert(try!(line)));
    }
    set.finish().map_err(From::from)
}

fn run_unsorted(_args: &Args) -> Result<(), Error> {
    unimplemented!()
}
