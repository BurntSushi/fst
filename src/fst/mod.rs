use std::cmp;
use std::fmt;
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
mod registry_minimal;
#[cfg(test)] mod tests;

const VERSION: u64 = 1;
const NONE_STATE: CompiledAddr = 1;

pub type CompiledAddr = u64;

pub struct Fst {
    data: FstData,
    root_addr: CompiledAddr,
}

impl Fst {
    pub fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        Fst::new(FstData::Mmap(try!(Mmap::open_path(path, Protection::Read))))
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self> {
        Fst::new(FstData::Owned(bytes))
    }
}

impl Fst {
    fn new(data: FstData) -> Result<Self> {
        if data.as_slice().len() < 16 {
            return Err(Error::Format);
        }
        // The read_u64 unwraps below are OK because they can never fail.
        // They can only fail when there is an IO error or if there is an
        // unexpected EOF. However, we are reading from a byte slice (no
        // IO errors possible) and we've confirmed the byte slice is at least
        // 16 bytes (no unexpected EOF).
        let version = data.as_slice().read_u64::<LittleEndian>().unwrap();
        if version != VERSION {
            return Err(Error::Version {
                expected: VERSION,
                got: version,
            });
        }
        let root_addr = {
            let mut last = &data.as_slice()[data.as_slice().len() - 8..];
            last.read_u64::<LittleEndian>().unwrap()
        };
        Ok(Fst {
            data: data,
            root_addr: root_addr,
        })
    }

    pub fn reader(&self) -> FstReader {
        let root = self.root();
        let empty_output =
            if root.is_final() {
                Some(root.final_output())
            } else {
                None
            };
        FstReader {
            fst: self,
            inp: Vec::with_capacity(64),
            empty_output: empty_output,
            stack: vec![FstReaderState {
                addr: self.root_addr,
                trans: 0,
                out: Output::zero(),
            }],
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        self.data.as_slice()
    }

    pub fn root(&self) -> Node {
        self.node(self.root_addr)
    }

    pub fn node(&self, addr: CompiledAddr) -> Node {
        node_new(addr, &self.data.as_slice())
    }

    pub fn find<B: AsRef<[u8]>>(&self, key: B) -> Option<Output> {
        let mut node = self.root();
        let mut out = Output::zero();
        for &b in key.as_ref() {
            node = match node.find_input(b) {
                None => return None,
                Some(i) => {
                    let t = node.transition(i);
                    out = out.cat(t.out);
                    self.node(t.addr)
                }
            }
        }
        if !node.is_final() {
            None
        } else {
            Some(out.cat(node.final_output()))
        }
    }
}

pub struct FstReader<'f> {
    fst: &'f Fst,
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

impl<'f> FstReader<'f> {
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

enum FstData {
    Owned(Vec<u8>),
    Mmap(Mmap),
}

impl FstData {
    fn as_slice(&self) -> &[u8] {
        match *self {
            FstData::Owned(ref v) => &**v,
            // I find it slightly difficult to articulate an argument for
            // safety here. My understanding is that `as_slice` is unsafe
            // because there could be some other *process* modifying the
            // underlying data? In particular, the mmap is opened in shared
            // mode, so if some other process modifies the underlying data,
            // is it observable from this slice? And if so, does that imply
            // unsafety?
            //
            // If this is *not* safe to do, then what is the alternative?
            FstData::Mmap(ref v) => unsafe { v.as_slice() }
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Transition {
    pub inp: u8,
    pub out: Output,
    pub addr: CompiledAddr,
}

impl Default for Transition {
    fn default() -> Self {
        Transition {
            inp: 0,
            out: Output::zero(),
            addr: NONE_STATE,
        }
    }
}

impl fmt::Debug for Transition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.out.is_zero() {
            write!(f, "{} -> {}", self.inp as char, self.addr)
        } else {
            write!(f, "({}, {}) -> {}",
                   self.inp as char, self.out.value(), self.addr)
        }
    }
}
