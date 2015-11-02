use docopt::Docopt;
use fst::raw::Fst;

use util;
use Error;

const USAGE: &'static str = "
Shows a single node from the transducer.

The input to this command is the node's address. An address may be found either
from debugging a transducer in code, or from the output of the 'fst csv'
command.

If the address does not point to a valid node, then the executable may panic or
abort without ceremony.

Usage:
    fst node [options] <fst> <node-address>
    fst node --help

Options:
    -h, --help       Show this help message.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_fst: String,
    arg_node_address: usize,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).decode())
                            .unwrap_or_else(|e| e.exit());
    let mut wtr = try!(util::get_buf_writer::<&str>(None));
    let fst = try!(Fst::from_file_path(&args.arg_fst));
    let node = fst.node(args.arg_node_address);
    w!(wtr, "{:?}", node);
    Ok(())
}
