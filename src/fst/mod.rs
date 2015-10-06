use std::cmp;
use std::path::Path;

use byteorder::{ReadBytesExt, LittleEndian};
use memmap::{Mmap, Protection};

use error::{Error, Result};
pub use self::build::Builder;
pub use self::merge::{union_ignore_outputs, union_with_outputs};
pub use self::node::{Node, Transitions};
use self::node::node_new;

mod build;
mod common_inputs;
mod counting_writer;
mod merge;
mod node;
mod pack;
mod registry;
#[cfg(test)] mod tests;

const VERSION: u64 = 1;
const NONE_STATE: CompiledAddr = 1;

pub type CompiledAddr = u64;

pub struct Fst<B> {
    data: B,
    root_addr: CompiledAddr,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Transition {
    pub inp: u8,
    pub out: Output,
    pub addr: CompiledAddr,
}

impl Fst<MmapSlice> {
    pub fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Fst<MmapSlice>> {
        Fst::new(MmapSlice(try!(Mmap::open_path(path, Protection::Read))))
    }
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
                out: Output::zero(),
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
        node_new(addr, &self.data.as_ref())
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

#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Output(u64);

impl Output {
    fn new(v: u64) -> Output {
        Output(v)
    }

    fn zero() -> Output {
        Output(0)
    }

    pub fn value(self) -> u64 {
        self.0
    }

    fn encode(self) -> u64 {
        self.0
    }

    fn decode(v: u64) -> Output {
        Output(v)
    }

    pub fn is_zero(self) -> bool {
        self.0 == 0
    }

    pub fn prefix(self, o: Output) -> Output {
        Output(cmp::min(self.0, o.0))
    }

    pub fn cat(self, o: Output) -> Output {
        Output(self.0 + o.0)
    }

    pub fn sub(self, o: Output) -> Output {
        Output(self.0.checked_sub(o.0)
                     .expect("BUG: underflow subtraction not allowed"))
    }
}

pub struct MmapSlice(Mmap);

impl AsRef<[u8]> for MmapSlice {
    fn as_ref(&self) -> &[u8] {
        // I find it slightly difficult to articulate an argument for safety
        // here. My understanding is that `as_slice` is unsafe because there
        // could be some other *process* modifying the underlying data?
        // In particular, the mmap is opened in shared mode, so if some other
        // process modifies the underlying data, is it observable from this
        // slice? And if so, does that imply unsafety?
        //
        // If this is *not* safe to do, then what is the alternative?
        unsafe { self.0.as_slice() }
    }
}
