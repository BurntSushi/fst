use std::io;

use docopt::Docopt;
use fst::automaton::Levenshtein;
use fst::raw::Fst;
use serde::Deserialize;

use crate::util;
use crate::Error;

const USAGE: &'static str = "
Issues a fuzzy query against the given transducer.

A fuzzy query returns all search results within a particular edit distance
of the query given.

WARNING: This works by building a Levenshtein automaton, which is currently
rather expensive (in time and space) with a big edit distance. This will be
improved in the future.

Usage:
    fst fuzzy [options] <fst> <query>
    fst fuzzy --help

Options:
    -h, --help          Display this message.
    -d, --distance ARG  All terms in the fst within this distance are shown.
                        The distance is measured in the number of character
                        insertions, deletions and substitutions on the query
                        to get the term. In this case, a \"character\" is a
                        single Unicode codepoint. [default: 1]
    -o, --outputs       When set, output values are shown as CSV data.
    -s, --start ARG     Only show results greater than or equal to this.
    -e, --end ARG       Only show results less than or equal to this.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_fst: String,
    arg_query: String,
    flag_distance: u32,
    flag_outputs: bool,
    flag_start: Option<String>,
    flag_end: Option<String>,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(&argv).deserialize())
        .unwrap_or_else(|e| e.exit());
    let fst = unsafe { Fst::from_path(&args.arg_fst) }?;
    let lev = Levenshtein::new(&args.arg_query, args.flag_distance)?;
    let mut q = fst.search(&lev);
    if let Some(ref start) = args.flag_start {
        q = q.ge(start);
    }
    if let Some(ref end) = args.flag_end {
        q = q.le(end);
    }
    util::print_stream(io::BufWriter::new(io::stdout()), args.flag_outputs, q)
}
