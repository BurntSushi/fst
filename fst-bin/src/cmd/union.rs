use std::fs;

use docopt::Docopt;
use fst;

use util;
use Error;

const USAGE: &'static str = "
Unions all of the transducer inputs into a single transducer.

Any output values are dropped. Stated differently, the output transducer is
always a set.

Usage:
    fst union [options] <input>... <output>
    fst union --help

Options:
    -h, --help       Show this help message.
    -f, --force      Overwrites the output if a file already exists.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_input: Vec<String>,
    arg_output: String,
    flag_force: bool,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).decode())
                            .unwrap_or_else(|e| e.exit());
    if !args.flag_force && fs::metadata(&args.arg_output).is_ok() {
        return fail!("Output file already exists: {}", args.arg_output);
    }

    let wtr = try!(util::get_buf_writer(Some(&args.arg_output)));
    let mut merged = try!(fst::SetBuilder::new(wtr));

    let mut sets = vec![];
    for set_path in &args.arg_input {
        sets.push(try!(fst::Set::from_path(set_path)));
    }
    let union = sets.iter().collect::<fst::set::OpBuilder>().union();
    try!(merged.extend_stream(union));
    try!(merged.finish());
    Ok(())
}
