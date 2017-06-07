use std::io;

use docopt::Docopt;
use fst::raw::Fst;

use util;
use Error;

const USAGE: &'static str = "
Issues a range query against the given transducer.

A range query returns all search results within a particular.

If neither the start or the end of the range is specified, then all entries
in the transducer are shown.

Usage:
    fst range [options] <fst>
    fst range --help

Options:
    -h, --help          Display this message.
    -o, --outputs       When set, output values are shown as CSV data.
    -s, --start ARG     Only show results greater than or equal to this.
    -e, --end ARG       Only show results less than or equal to this.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_fst: String,
    flag_outputs: bool,
    flag_start: Option<String>,
    flag_end: Option<String>,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).deserialize())
                            .unwrap_or_else(|e| e.exit());
    let fst = try!(Fst::from_path(&args.arg_fst));
    let mut q = fst.range();
    if let Some(ref start) = args.flag_start {
        q = q.ge(start);
    }
    if let Some(ref end) = args.flag_end {
        q = q.le(end);
    }
    util::print_stream(io::BufWriter::new(io::stdout()), args.flag_outputs, q)
}
