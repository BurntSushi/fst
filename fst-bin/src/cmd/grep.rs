use std::io;

use docopt::Docopt;
use fst::raw::Fst;
use regex_automata::dense;
// use regex_automata::sparse::SparseDFA;
use serde::Deserialize;

use crate::util;
use crate::Error;

const USAGE: &'static str = "
Searches a transducer with a regular expression.

WARNING: This works by building a regular expression automaton, which is
currently rather expensive (in time and space) with complex regexes. This
will be improved in the future.

Usage:
    fst grep [options] <fst> <regex>
    fst grep --help

Options:
    -h, --help          Display this message.
    -o, --outputs       When set, output values are shown as CSV data.
    -s, --start ARG     Only show results greater than or equal to this.
    -e, --end ARG       Only show results less than or equal to this.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_fst: String,
    arg_regex: String,
    flag_outputs: bool,
    flag_start: Option<String>,
    flag_end: Option<String>,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(&argv).deserialize())
        .unwrap_or_else(|e| e.exit());
    let fst = unsafe { Fst::from_path(&args.arg_fst) }?;
    let dense_dfa = dense::Builder::new()
        .anchored(true)
        .byte_classes(false)
        .premultiply(false)
        .build(&args.arg_regex)?;
    let dfa = match dense_dfa {
        dense::DenseDFA::Standard(dfa) => dfa,
        _ => unreachable!(),
    };
    // let dfa = match dense_dfa.to_sparse()? {
    // SparseDFA::Standard(dfa) => dfa,
    // _ => unreachable!(),
    // };
    let mut q = fst.search(&dfa);
    if let Some(ref start) = args.flag_start {
        q = q.ge(start);
    }
    if let Some(ref end) = args.flag_end {
        q = q.le(end);
    }
    util::print_stream(io::BufWriter::new(io::stdout()), args.flag_outputs, q)
}
