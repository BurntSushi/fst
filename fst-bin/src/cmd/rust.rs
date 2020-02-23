use std::io::{Read, Write};

use docopt::Docopt;
use serde::Deserialize;

use crate::util;
use crate::Error;

const USAGE: &'static str = "
Emit Rust source code for the given FST.

This reads the FST given and emits it as Rust source code with one
constant defined:

    {NAME}_BYTES

And a `lazy_static!` ref for:

    {NAME}

Where {NAME} is taken from the name given as an argument.

The latter definition corresponds to calling `Fst::from_bytes({NAME}_BYTES)`.
This makes it possible to trivially use pre-built FSTs in your program.

Usage:
    fst rust [options] <fst> <name>
    fst node --help

Options:
    -h, --help       Show this help message.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_fst: String,
    arg_name: String,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(&argv).deserialize())
        .unwrap_or_else(|e| e.exit());
    let mut wtr = util::get_buf_writer::<&str>(None)?;
    let mut rdr = util::get_buf_reader(Some(&args.arg_fst))?;

    let mut bytes = vec![];
    rdr.read_to_end(&mut bytes)?;

    w!(wtr, "lazy_static! {{");
    w!(wtr, "    pub static ref {}: ::fst::raw::Fst = ", args.arg_name);
    w!(wtr, "        ::fst::raw::Fst::new({}_BYTES).unwrap();", args.arg_name);
    w!(wtr, "}}\n");

    w!(wtr, "const {}_BYTES: &'static [u8] = b\"\\", args.arg_name);
    let mut column = 0;
    for b in bytes {
        let escaped = if (b as char).is_whitespace() {
            format!("\\x{:02x}", b)
        } else {
            util::escape_input(b)
        };
        if column + escaped.len() >= 79 {
            column = 0;
            write!(wtr, "\\\n")?;
        }
        column += escaped.len();
        write!(wtr, "{}", escaped)?;
    }
    w!(wtr, "\\\n\";");

    Ok(())
}
