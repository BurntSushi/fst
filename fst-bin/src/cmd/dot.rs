use std::io::Write;
use std::path::PathBuf;

use bit_set::BitSet;

use crate::util;
use crate::Error;

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    output: Option<PathBuf>,
    state_names: bool,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            output: m.value_of_os("output").map(PathBuf::from),
            state_names: m.is_present("state-names"),
        })
    }

    fn run(&self) -> Result<(), Error> {
        let mut wtr = util::get_buf_writer(self.output.as_ref())?;
        let fst = unsafe { util::mmap_fst(&self.input)? };
        let mut set = BitSet::with_capacity(fst.len());

        let mut stack = vec![fst.root().addr()];

        writeln!(
            wtr,
            r#"
    digraph automaton {{
        labelloc="l";
        labeljust="l";
        rankdir="LR";
    "#
        )
        .unwrap();
        let mut state_num = 0;
        while let Some(addr) = stack.pop() {
            if set.contains(addr) {
                continue;
            }
            set.insert(addr);

            let node = fst.node(addr);
            writeln!(wtr, "{}", self.dot_state(&node, state_num, addr))
                .unwrap();
            for t in node.transitions() {
                stack.push(t.addr);
                let out = if t.out.value() == 0 {
                    "".to_owned()
                } else {
                    format!("/{}", t.out.value().to_string())
                };
                writeln!(
                    wtr,
                    "    {} -> {} [label=\"{}{}\"];",
                    addr,
                    t.addr,
                    util::escape_input(t.inp),
                    out
                )
                .unwrap();
            }
            state_num += 1;
        }
        writeln!(wtr, "}}").unwrap();
        wtr.flush()?;
        Ok(())
    }

    fn dot_state(
        &self,
        node: &fst::raw::Node,
        i: usize,
        addr: fst::raw::CompiledAddr,
    ) -> String {
        let label =
            if self.state_names { i.to_string() } else { "".to_owned() };
        if node.is_final() {
            format!("    {} [label=\"{}\",peripheries=2];", addr, label)
        } else {
            format!("    {} [label=\"{}\"];", addr, label)
        }
    }
}
