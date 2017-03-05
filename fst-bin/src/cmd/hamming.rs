use std::io;

use docopt::Docopt;
use fst::Hamming;
use fst::raw::Fst;

use util;
use Error;

const USAGE: &'static str = "
Issues a fuzzy (Hamming) query against the given transducer.

A fuzzy query returns all search results within a particular Hamming
distance of the query given. Hamming distance is a number of substitutions
one has to perform.

Usage:
    fst hamming [options] <fst> <query>
    fst hamming --help

Options:
    -h, --help          Display this message.
    -d, --distance ARG  All terms in the fst within this distance are shown.
                        The distance is measured in the number of character
                        substitutions on the query to get the term. In this
                        case, a \"character\" is a single byte. [default: 1]
    -o, --outputs       When set, output values are shown as CSV data.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_fst: String,
    arg_query: String,
    flag_distance: u32,
    flag_outputs: bool,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).decode())
                            .unwrap_or_else(|e| e.exit());
    let fst = try!(Fst::from_path(&args.arg_fst));
    let lev = Hamming::new(&args.arg_query, args.flag_distance);
    let q = fst.search(&lev);
    util::print_stream(io::BufWriter::new(io::stdout()), args.flag_outputs, q)
}
