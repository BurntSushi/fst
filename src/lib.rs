#![allow(dead_code, unused_mut, unused_variables)]

extern crate byteorder;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

use std::cmp;
use std::io;

use byteorder::{ReadBytesExt, LittleEndian};

pub use build::Builder;
pub use error::{Error, Result};
pub use node::Node;

mod build;
mod error;
mod ioutil;
mod node;
mod pack;
mod registry;
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
    trans: Vec<Transition>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Transition {
    pub inp: u8,
    pub out: Output,
    pub addr: CompiledAddr,
}

impl<B: AsRef<[u8]>> Fst<B> {
    pub fn new(data: B) -> Result<Fst<B>> {
        if data.as_ref().len() < 16 {
            return Err(Error::Format);
        }
        // The read_u64 unwraps below are OK because they can never fail.
        // They can only fail when there is an IO error or if there is an
        // unexpected EOF. However, we are reading from a byte slice (no
        // IO errors possible) and we've confirmed the byte slice is at least
        // 16 bytes (no unexpected EOF).
        let version = data.as_ref().read_u64::<LittleEndian>().unwrap();
        if version != VERSION {
            return Err(Error::Version {
                expected: VERSION,
                got: version,
            });
        }
        let root_addr = {
            let mut last = &data.as_ref()[data.as_ref().len() - 8..];
            last.read_u64::<LittleEndian>().unwrap()
        };
        Ok(Fst {
            data: data,
            root_addr: root_addr,
        })
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

    pub fn as_slice(&self) -> &[u8] {
        self.data.as_ref()
    }

    pub fn root(&self) -> Node {
        self.node(self.root_addr)
    }

    pub fn node(&self, addr: CompiledAddr) -> Node {
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
            for t in node.transitions() {
                let edge = (addr, t.addr, t.inp);
                if !seen.contains(&edge) {
                    seen.insert(edge);
                    w!(out, "    {} -> {} [label={}];\n",
                       addr, t.addr, word(t.inp));
                }
                stack.push(t.addr);
            }
        }
        w!(out, "}}");
        out
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Output(u64);

impl Output {
    fn some(v: u64) -> Option<Output> {
        if v == ::std::u64::MAX {
            None
        } else {
            Some(Output(v + 1))
        }
    }

    fn none() -> Output {
        Output(0)
    }

    pub fn into_option(self) -> Option<u64> {
        if self.is_none() { None } else { Some(self.0 - 1) }
    }

    fn encode(self) -> u64 {
        self.0
    }

    fn decode(v: u64) -> Output {
        Output(v)
    }

    fn is_some(self) -> bool {
        self.0 > 0
    }

    fn is_none(self) -> bool {
        !self.is_some()
    }

    fn prefix(self, o: Output) -> Output {
        Output(cmp::min(self.0, o.0))
    }

    fn cat(self, o: Output) -> Output {
        Output(self.0 + o.0)
    }

    fn sub(self, o: Output) -> Output {
        Output(self.0.checked_sub(o.0)
                     .expect("BUG: underflow subtraction not allowed"))
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
            let trans = node.transition(state.trans);
            self.inp.push(trans.inp);
            let out = state.out.cat(trans.out);
            self.stack.push(FstReaderState {
                addr: state.addr,
                trans: state.trans + 1,
                out: state.out,
            });
            self.stack.push(FstReaderState {
                addr: trans.addr,
                trans: 0,
                out: out,
            });
            let next_node = self.fst.node(trans.addr);
            if next_node.is_final() {
                return Some((&self.inp, out.cat(next_node.final_output())));
            }
        }
        None
    }
}
