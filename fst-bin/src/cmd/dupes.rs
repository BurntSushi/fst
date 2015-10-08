use std::collections::HashMap;
use std::io::Write;

use bit_set::BitSet;
use docopt::Docopt;
use fst::fst;

use util;
use Error;

const USAGE: &'static str = "
Usage: fst dupes <input> [<output>]
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_input: String,
    arg_output: Option<String>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct FullNode {
    is_final: bool,
    final_output: fst::Output,
    trans: Vec<fst::Transition>,
}

impl FullNode {
    fn from_node(node: &fst::Node) -> FullNode {
        FullNode {
            is_final: node.is_final(),
            final_output: node.final_output(),
            trans: node.transitions().collect(),
        }
    }
}

pub fn run(argv: Vec<String>) -> Result<(), Error> {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.argv(&argv).decode())
                            .unwrap_or_else(|e| e.exit());

    let mut wtr = try!(util::get_buf_writer(args.arg_output));
    let fst = try!(fst::Fst::from_file_path(args.arg_input));
    let mut set = BitSet::with_capacity(fst.as_slice().len());
    let mut node_counts = HashMap::with_capacity(10_000);

    let mut stack = vec![fst.root().addr()];
    while let Some(addr) = stack.pop() {
        if set.contains(&(addr as usize)) {
            continue;
        }
        set.insert(addr as usize);

        let full_node = FullNode::from_node(&fst.node(addr));
        for t in &full_node.trans {
            stack.push(t.addr);
        }
        *node_counts.entry(full_node).or_insert(0) += 1;
    }

    let total = node_counts.values().fold(0, |n, c| n + c);
    let unique = node_counts.len();
    let mut counts: Vec<(FullNode, i32)> =
        node_counts.into_iter().filter(|&(_, c)| c > 1).collect();
    counts.sort_by(|&(_, ref c1), &(_, ref c2)| c1.cmp(c2).reverse());

    w!(wtr, "Total nodes:           {}", total);
    w!(wtr, "Unique nodes:          {}", unique);
    w!(wtr, "Nodes with duplicates: {}", counts.len());
    w!(wtr, "----------------------------------");

    for &(ref fnode, count) in counts.iter().take(10) {
        w!(wtr, "Duplicated {} times", count);
        w!(wtr, "{:#?}", fnode);
        w!(wtr, "----------------------------------");
    }

    try!(wtr.flush());
    Ok(())
}
