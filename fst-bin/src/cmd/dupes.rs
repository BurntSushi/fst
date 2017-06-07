use std::collections::HashMap;
use std::io::Write;

use bit_set::BitSet;
use docopt::Docopt;
use fst::raw as fst;

use util;
use Error;

const USAGE: &'static str = "
A simple way to show duplicate nodes.

This is meant to be a diagnostic tool to view duplicate nodes in the
transducer. Every duplicate node represents a missed opportunity for more
compression. A minimal transducer should have precisely zero duplicate nodes.

WARNING: This stores all nodes in the transducer in memory, decompressed. This
may be expensive in both time and space depending on the size of your
transducer.

If <output> is omitted, then diagnostic data is emitted to stdout.

Usage:
    fst dupes [options] <input> [<output>]
    fst dupes --help

Options:
    -h, --help       Display this message.
    --limit ARG      Show this many duplicate nodes. [default: 10]
    --min ARG        Only show duplicate nodes with this many
                     reoccurrences. [default: 20]
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_input: String,
    arg_output: Option<String>,
    flag_limit: usize,
    flag_min: i32,
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
                            .and_then(|d| d.argv(&argv).deserialize())
                            .unwrap_or_else(|e| e.exit());

    let mut wtr = try!(util::get_buf_writer(args.arg_output.as_ref()));
    let fst = try!(fst::Fst::from_path(args.arg_input));
    let mut set = BitSet::with_capacity(fst.len());
    let mut node_counts = HashMap::with_capacity(10_000);

    let mut stack = vec![fst.root().addr()];
    while let Some(addr) = stack.pop() {
        if set.contains(addr) {
            continue;
        }
        set.insert(addr);

        let full_node = FullNode::from_node(&fst.node(addr));
        for t in &full_node.trans {
            stack.push(t.addr);
        }
        *node_counts.entry(full_node).or_insert(0) += 1;
    }

    let total = node_counts.values().fold(0, |n, c| n + c);
    let unique = node_counts.len();
    let min = args.flag_min;
    let mut counts: Vec<(FullNode, i32)> =
        node_counts.into_iter().filter(|&(_, c)| c > min).collect();
    counts.sort_by(|&(_, ref c1), &(_, ref c2)| c1.cmp(c2).reverse());

    w!(wtr, "Total nodes:           {}", total);
    w!(wtr, "Unique nodes:          {}", unique);
    w!(wtr, "Nodes with duplicates: {}", counts.len());
    w!(wtr, "----------------------------------");

    for &(ref fnode, count) in counts.iter().take(args.flag_limit) {
        w!(wtr, "Duplicated {} times", count);
        w!(wtr, "{:#?}", fnode);
        w!(wtr, "----------------------------------");
    }

    try!(wtr.flush());
    Ok(())
}
