use csv;
use docopt::Docopt;
use fst::MapBuilder;

use util;
use Error;

const USAGE: &'static str = "
Creates an ordered map backed by a finite state transducer.

The input to this command should be a CSV file with exactly two columns.
The first column should be the key and the second column should be a value
that can be interpreted as an unsigned 64 bit integer.

Usage:
    fst map [options] <input> <output>
    fst map --help

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
    let rdr = try!(util::get_reader(Some(&args.arg_input)));
    let mut rdr = csv::Reader::from_reader(rdr).has_headers(false);
    let wtr = try!(util::get_buf_writer(Some(&args.arg_output)));
    let mut map = try!(MapBuilder::new(wtr));
    for row in rdr.decode() {
        let (key, val): (String, u64) = try!(row);
        try!(map.insert(key, val));
    }
    try!(map.finish());
    Ok(())
}

fn run_unsorted(_args: &Args) -> Result<(), Error> {
    unimplemented!()
}
