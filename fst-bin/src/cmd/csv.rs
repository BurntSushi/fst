use bit_set::BitSet;
use csv;
use docopt::Docopt;
use fst::raw as fst;

use crate::util;
use crate::Error;

const USAGE: &'static str = "
Emit information in CSV format about the transducer.

If <output> is not set, then CSV data is emitted to stdout.

Usage:
    fst csv edges <input> [<output>]
    fst csv nodes <input> [<output>]
    fst csv --help

Options:
    -h, --help       Display this message.
";

#[derive(Debug, Deserialize)]
struct Args {
    cmd_edges: bool,
    cmd_nodes: bool,
    arg_input: String,
    arg_output: Option<String>,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).deserialize())
                            .unwrap_or_else(|e| e.exit());

    let wtr = util::get_writer(args.arg_output.as_ref())?;
    let mut wtr = csv::Writer::from_writer(wtr);

    let fst = unsafe { fst::Fst::from_path(args.arg_input) }?;
    let mut set = BitSet::with_capacity(fst.len());

    if args.cmd_edges {
        wtr.serialize(("addr_in", "addr_out", "input", "output"))?;
        let mut stack = vec![fst.root().addr()];
        set.insert(fst.root().addr());
        while let Some(addr) = stack.pop() {
            for t in fst.node(addr).transitions() {
                if !set.contains(t.addr) {
                    stack.push(t.addr);
                    set.insert(t.addr);
                }
                wtr.serialize((
                    addr, t.addr, t.inp as char, t.out.value(),
                ))?;
            }
        }
    } else {
        wtr.serialize((
            "addr", "state", "size",
            "transitions", "final", "final_output",
        ))?;
        let mut stack = vec![fst.root().addr()];
        set.insert(fst.root().addr());
        while let Some(addr) = stack.pop() {
            let node = fst.node(addr);
            for t in node.transitions() {
                if !set.contains(t.addr) {
                    stack.push(t.addr);
                    set.insert(t.addr);
                }
            }
            let row = &[
                node.addr().to_string(),
                node.state().to_string(),
                node.as_slice().len().to_string(),
                node.len().to_string(),
                node.is_final().to_string(),
                node.final_output().value().to_string(),
            ];
            wtr.write_record(row.iter())?;
        }
    }
    wtr.flush().map_err(From::from)
}
