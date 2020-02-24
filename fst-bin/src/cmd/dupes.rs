use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;

use bit_set::BitSet;

use crate::{util, Error};

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct FullNode {
    is_final: bool,
    final_output: fst::raw::Output,
    trans: Vec<fst::raw::Transition>,
}

impl FullNode {
    fn from_node(node: &fst::raw::Node) -> FullNode {
        FullNode {
            is_final: node.is_final(),
            final_output: node.final_output(),
            trans: node.transitions().collect(),
        }
    }
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    output: Option<PathBuf>,
    limit: usize,
    min: i32,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            output: m.value_of_os("output").map(PathBuf::from),
            limit: m.value_of_lossy("limit").unwrap().parse()?,
            min: m.value_of_lossy("min").unwrap().parse()?,
        })
    }

    fn run(&self) -> Result<(), Error> {
        let mut wtr = util::get_buf_writer(self.output.as_ref())?;
        let fst = unsafe { util::mmap_fst(&self.input)? };
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
        let mut counts: Vec<(FullNode, i32)> =
            node_counts.into_iter().filter(|&(_, c)| c > self.min).collect();
        counts.sort_by(|&(_, ref c1), &(_, ref c2)| c1.cmp(c2).reverse());

        w!(wtr, "Total nodes:           {}", total);
        w!(wtr, "Unique nodes:          {}", unique);
        w!(wtr, "Nodes with duplicates: {}", counts.len());
        w!(wtr, "----------------------------------");

        for &(ref fnode, count) in counts.iter().take(self.limit) {
            w!(wtr, "Duplicated {} times", count);
            w!(wtr, "{:#?}", fnode);
            w!(wtr, "----------------------------------");
        }

        wtr.flush()?;
        Ok(())
    }
}
