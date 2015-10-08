use std::io::Write;

use bit_set::BitSet;
use csv;
use docopt::Docopt;
use fst::fst;

use util;
use Error;

const USAGE: &'static str = "
Usage: fst csv edges <input> [<output>]
       fst csv nodes <input> [<output>]
";

#[derive(Debug, RustcDecodable)]
struct Args {
    cmd_edges: bool,
    cmd_nodes: bool,
    arg_input: String,
    arg_output: Option<String>,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).decode())
                            .unwrap_or_else(|e| e.exit());

    let wtr = try!(util::get_writer(args.arg_output));
    let mut wtr = csv::Writer::from_writer(wtr);

    let fst = try!(fst::Fst::from_file_path(args.arg_input));
    let mut set = BitSet::with_capacity(fst.as_slice().len());

    if args.cmd_edges {
        try!(wtr.encode(("addr_in", "addr_out", "input", "output")));
        let mut stack = vec![fst.root().addr()];
        while let Some(addr) = stack.pop() {
            if set.contains(&(addr as usize)) {
                continue;
            }
            set.insert(addr as usize);
            for t in fst.node(addr).transitions() {
                stack.push(t.addr);
                try!(wtr.encode((
                    addr, t.addr, t.inp as char, t.out.value(),
                )));
            }
        }
    } else {
        try!(wtr.encode((
            "addr", "state", "size",
            "transitions", "final", "final_output",
        )));
        let mut stack = vec![fst.root().addr()];
        while let Some(addr) = stack.pop() {
            if set.contains(&(addr as usize)) {
                continue;
            }
            set.insert(addr as usize);
            let node = fst.node(addr);
            for t in node.transitions() {
                stack.push(t.addr);
            }
            try!(wtr.encode((
                node.addr(), node.state(),
                node.as_slice().len(), node.len(),
                node.is_final(), node.final_output().value(),
            )));
        }
    }
    try!(wtr.flush());
    Ok(())
}
