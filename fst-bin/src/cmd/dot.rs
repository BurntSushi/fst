use std::io::Write;

use bit_set::BitSet;
use docopt::Docopt;
use fst::raw as fst;

use util;
use Error;

const USAGE: &'static str = "
Usage: fst dot <input> [<output>]
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

    let mut wtr = try!(util::get_buf_writer(args.arg_output));
    let fst = try!(fst::Fst::from_file_path(args.arg_input));
    let mut set = BitSet::with_capacity(fst.as_slice().len());

    let mut stack = vec![fst.root().addr()];

    writeln!(wtr, r#"
digraph automaton {{
    label=<<FONT POINT-SIZE="20">minimal acyclic DFA</FONT>>;
    labelloc="l";
    labeljust="l";
    rankdir="LR";
"#).unwrap();
    while let Some(addr) = stack.pop() {
        if set.contains(&(addr as usize)) {
            continue;
        }
        set.insert(addr as usize);

        let node = fst.node(addr);
        if node.is_final() {
            writeln!(wtr, "    {} [peripheries=2];", addr).unwrap();
        } else {
            writeln!(wtr, "    {};", addr).unwrap();
        }
        for t in node.transitions() {
            stack.push(t.addr);
            let inp = (t.inp as char).to_string();
            writeln!(wtr, "    {} -> {} [label=\"{}\"];",
                     addr, t.addr, inp).unwrap();
        }
    }
    writeln!(wtr, "}}").unwrap();
    try!(wtr.flush());
    Ok(())
}
