#![allow(dead_code, unused_mut, unused_variables)]

extern crate byteorder;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

use std::cmp;
use std::io;

use byteorder::{ReadBytesExt, LittleEndian};

pub use build::Builder;
use node::Node;

mod build;
mod ioutil;
mod node;
mod pack;
#[cfg(test)] mod tests;

const VERSION: u64 = 1;
const FINAL_EMPTY_STATE: CompiledAddr = 0;
const NONE_STATE: CompiledAddr = 1;

pub type CompiledAddr = u64;

pub struct Fst<B> {
    data: B,
    root_addr: CompiledAddr,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct BuilderNode {
    is_final: bool,
    final_output: Output,
    trans: Vec<BuilderTransition>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
struct BuilderTransition {
    addr: CompiledAddr,
    inp: u8,
    out: Output,
}

impl<B: AsRef<[u8]>> Fst<B> {
    pub fn new(data: B) -> Fst<B> {
        let root_addr = {
            let mut last = &data.as_ref()[data.as_ref().len() - 8..];
            last.read_u64::<LittleEndian>().unwrap()
        };
        Fst {
            data: data,
            root_addr: root_addr,
        }
    }

    pub fn into_inner(self) -> B {
        self.data
    }

    pub fn reader(&self) -> FstReader<B> {
        FstReader {
            fst: self,
            inp: Vec::with_capacity(64),
            empty_output: self.empty_output(),
            stack: vec![FstReaderState {
                addr: self.root_addr,
                trans: 0,
                out: Output::none(),
            }],
        }
    }

    fn root(&self) -> Node {
        self.node(self.root_addr)
    }

    fn node(&self, addr: CompiledAddr) -> Node {
        Node::new(addr, &self.data.as_ref())
    }

    fn empty_output(&self) -> Option<Output> {
        let root = self.root();
        if root.is_final() {
            Some(root.final_output())
        } else {
            None
        }
    }

    #[doc(hidden)]
    pub fn csv<W: io::Write>(&self, mut wtr: W) -> io::Result<()> {
        use std::collections::HashSet;

        fn word(b: u8) -> String {
            match b {
                b'"' => "\"\"\"\"".into(),
                b',' => "\",\"".into(),
                b => (b as char).to_string(),
            }
        }

        try!(writeln!(wtr, "word,in,out"));
        let mut stack = vec![self.root_addr];
        let mut seen = HashSet::new();
        while let Some(addr) = stack.pop() {
            if seen.contains(&addr) {
                continue;
            }
            seen.insert(addr);
            for (b, _, to_addr) in self.node(addr).transitions() {
                stack.push(to_addr);
                try!(writeln!(wtr, "{},{},{}", word(b), addr, to_addr));
            }
        }
        Ok(())
    }

    #[doc(hidden)]
    pub fn dot(&self) -> String {
        use std::collections::HashSet;
        use std::fmt::Write;

        let mut out = String::new();
        macro_rules! w {
            ($w:expr, $($tt:tt)*) => { {write!($w, $($tt)*)}.unwrap() }
        }

        fn word(b: u8) -> String {
            if b == b'\'' {
                "\"'\"".into()
            } else {
                (b as char).to_string()
            }
        }

        w!(out, r#"
digraph automaton {{
    label=<<FONT POINT-SIZE="20">minimal acyclic DFA</FONT>>;
    labelloc="l";
    labeljust="l";
    rankdir="LR";
"#);
        let mut stack = vec![self.root_addr];
        let mut seen = HashSet::new();
        while let Some(addr) = stack.pop() {
            let node = self.node(addr);
            if node.is_final() {
                w!(out, "    {} [peripheries=2];\n", addr);
            } else {
                w!(out, "    {};\n", addr);
            }
            for (b, _, to_addr) in node.transitions() {
                let edge = (addr, to_addr, b);
                if !seen.contains(&edge) {
                    seen.insert(edge);
                    w!(out, "    {} -> {} [label={}];\n",
                       addr, to_addr, word(b));
                }
                stack.push(to_addr);
            }
        }
        w!(out, "}}");
        out
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Output(Option<u64>);

impl Output {
    fn new(v: u64) -> Output {
        assert!(v < ::std::u64::MAX);
        Output(Some(v))
    }

    fn none() -> Output {
        Output(None)
    }

    fn encode(self) -> u64 {
        match self.0 {
            None => 0,
            Some(v) => v + 1,
        }
    }

    fn decode(v: u64) -> Output {
        if v == 0 {
            Output::none()
        } else {
            Output::new(v - 1)
        }
    }

    fn is_some(self) -> bool {
        match self.0 {
            Some(_) => true,
            None => false,
        }
    }

    fn is_none(self) -> bool {
        !self.is_some()
    }

    fn prefix(self, o: Output) -> Output {
        match (self.0, o.0) {
            (None, _) | (_, None) => Output(None),
            (Some(x), Some(y)) => Output(Some(cmp::min(x, y))),
        }
    }

    fn cat(self, o: Output) -> Output {
        match (self.0, o.0) {
            (None, x) | (x, None) => Output(x),
            (Some(x), Some(y)) => Output(Some(x + y)),
        }
    }

    fn sub(self, o: Output) -> Output {
        match (self.0, o.0) {
            (x, None) => Output(x),
            (None, Some(_)) => panic!("BUG: can't subtract from empty output"),
            (Some(x), Some(y)) if x == y => Output(None),
            (Some(x), Some(y)) => {
                assert!(x > y);
                Output(Some(x - y))
            }
        }
    }
}

pub struct FstReader<'a, B: 'a> {
    fst: &'a Fst<B>,
    inp: Vec<u8>,
    empty_output: Option<Output>,
    stack: Vec<FstReaderState>,
}

#[derive(Clone, Copy, Debug)]
struct FstReaderState {
    addr: CompiledAddr,
    trans: usize,
    out: Output,
}

impl<'a, B: AsRef<[u8]>> FstReader<'a, B> {
    pub fn next(&mut self) -> Option<(&[u8], Output)> {
        if let Some(out) = self.empty_output.take() {
            return Some((&[], out));
        }
        while let Some(state) = self.stack.pop() {
            let node = self.fst.node(state.addr);
            if state.trans >= node.len() {
                if node.addr() != self.fst.root_addr {
                    self.inp.pop().unwrap();
                }
                continue;
            }
            let (inp, out, addr) = node.transition(state.trans);
            self.inp.push(inp);
            let out = state.out.cat(out);
            self.stack.push(FstReaderState {
                addr: state.addr,
                trans: state.trans + 1,
                out: state.out,
            });
            self.stack.push(FstReaderState {
                addr: addr,
                trans: 0,
                out: out,
            });
            let next_node = self.fst.node(addr);
            if next_node.is_final() {
                return Some((&self.inp, out.cat(next_node.final_output())));
            }
        }
        None
    }
}
