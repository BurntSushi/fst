use std::io::Write;

use bit_set::BitSet;
use docopt::Docopt;
use fst::raw as fst;

use util;
use Error;

const USAGE: &'static str = "
Emit this transducer in the \"dot\" format.

If <output> is not set, then the \"dot\" description is emitted to stdout.

Generally, usage of this command should look like this:

    $ fst dot your-transducer.fst | dot -Tpng > your-transducer.png
    $ $YOUR_FAVORITE_IMAGE_VIEWER your-transducer.png

Note that `dot` is a command line utility that is part of graphviz.

If your transducer contains output values, then they are shown as labels on
transitions. Zero output values are omitted.

Usage:
    fst dot <input> [<output>]
    fst dot --help

Options:
    -h, --help       Show this help message.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_input: String,
    arg_output: Option<String>,
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).decode())
                            .unwrap_or_else(|e| e.exit());

    let mut wtr = try!(util::get_buf_writer(args.arg_output.as_ref()));
    let fst = try!(fst::Fst::from_file_path(args.arg_input));
    let mut set = BitSet::with_capacity(fst.len());

    let mut stack = vec![fst.root().addr()];

    writeln!(wtr, r#"
digraph automaton {{
    labelloc="l";
    labeljust="l";
    rankdir="LR";
"#).unwrap();
    while let Some(addr) = stack.pop() {
        if set.contains(&addr) {
            continue;
        }
        set.insert(addr);

        let node = fst.node(addr);
        if node.is_final() {
            writeln!(wtr, "    {} [label=\"\",peripheries=2];", addr).unwrap();
        } else {
            writeln!(wtr, "    {} [label=\"\"];", addr).unwrap();
        }
        for t in node.transitions() {
            stack.push(t.addr);
            let inp = (t.inp as char).to_string();
            let out = if t.out.value() == 0 { "".to_owned() } else {
                format!("/{}", t.out.value().to_string())
            };
            writeln!(wtr, "    {} -> {} [label=\"{}{}\"];",
                     addr, t.addr, inp, out).unwrap();
        }
    }
    writeln!(wtr, "}}").unwrap();
    try!(wtr.flush());
    Ok(())
}
