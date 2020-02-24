use std::path::PathBuf;

use bit_set::BitSet;
use csv;

use crate::util;
use crate::Error;

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    output: Option<PathBuf>,
    which: Which,
}

#[derive(Debug)]
enum Which {
    Edges,
    Nodes,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        let (which, m) = match m.subcommand() {
            ("edges", Some(m)) => (Which::Edges, m),
            ("nodes", Some(m)) => (Which::Nodes, m),
            (unknown, _) => {
                anyhow::bail!("unrecognized csv sub-command: {}", unknown)
            }
        };
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            output: m.value_of_os("output").map(PathBuf::from),
            which,
        })
    }

    fn run(&self) -> Result<(), Error> {
        let wtr = util::get_writer(self.output.as_ref())?;
        let mut wtr = csv::Writer::from_writer(wtr);

        let fst = unsafe { util::mmap_fst(&self.input)? };
        let mut set = BitSet::with_capacity(fst.len());

        match self.which {
            Which::Edges => {
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
                            addr,
                            t.addr,
                            t.inp as char,
                            t.out.value(),
                        ))?;
                    }
                }
            }
            Which::Nodes => {
                wtr.serialize((
                    "addr",
                    "state",
                    "size",
                    "transitions",
                    "final",
                    "final_output",
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
        }
        wtr.flush().map_err(From::from)
    }
}
