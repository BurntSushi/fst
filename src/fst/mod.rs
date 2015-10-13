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

    pub fn as_slice(&self) -> &[u8] {
        self.data.as_slice()
    }

    pub fn root(&self) -> Node {
        self.node(self.root_addr)
    }

    pub fn node(&self, addr: CompiledAddr) -> Node {
        node_new(addr, &self.data.as_slice())
    }

    pub fn empty_final_output(&self) -> Option<Output> {
        let root = self.root();
        if root.is_final() {
            Some(root.final_output())
        } else {
            None
        }
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

    pub fn reader(&self) -> FstReader {
        self.range::<&[u8]>(Bound::Unbounded, Bound::Unbounded)
    }

    pub fn range<T>(
        &self,
        min: Bound<T>,
        max: Bound<T>,
    ) -> FstReader
    where T: AsRef<[u8]> {
        FstReader::new(self, min, max)
    }
}

pub enum Bound<T> {
    Included(T),
    Excluded(T),
    Unbounded,
}

impl<T: AsRef<[u8]>> Bound<T> {
    fn includes_empty(&self) -> bool {
        match *self {
            Bound::Included(ref v) => v.as_ref() == &[],
            Bound::Excluded(_) => false,
            Bound::Unbounded => true,
        }
    }

    fn into_bytes(self) -> Bound<Vec<u8>> {
        match self {
            Bound::Included(v) => Bound::Included(v.as_ref().to_owned()),
            Bound::Excluded(v) => Bound::Excluded(v.as_ref().to_owned()),
            Bound::Unbounded => Bound::Unbounded,
        }
    }

    fn exceeded_by(&self, inp: &[u8]) -> bool {
        match *self {
            Bound::Included(ref v) => inp > v.as_ref(),
            Bound::Excluded(ref v) => inp >= v.as_ref(),
            Bound::Unbounded => false,
        }
    }
}

pub struct FstReader<'f> {
    fst: &'f Fst,
    inp: Vec<u8>,
    empty_output: Option<Output>,
    stack: Vec<FstReaderState>,
    end_at: Bound<Vec<u8>>,
}

#[derive(Clone, Copy, Debug)]
struct FstReaderState {
    addr: CompiledAddr,
    trans: usize,
    out: Output,
}

impl<'f> FstReader<'f> {
    fn new<T>(
        fst: &'f Fst,
        min: Bound<T>,
        max: Bound<T>,
    ) -> Self
    where T: AsRef<[u8]> {
        let mut rdr = FstReader {
            fst: fst,
            inp: Vec::with_capacity(16),
            empty_output: None,
            stack: vec![],
            end_at: max.into_bytes(),
        };
        rdr.seek_min(min);
        rdr
    }

    pub fn next(&mut self) -> Option<(&[u8], Output)> {
        if let Some(out) = self.empty_output.take() {
            if self.end_at.exceeded_by(&[]) {
                self.stack.clear();
                return None;
            }
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
            self.inp.push(trans.inp);
            if self.end_at.exceeded_by(&self.inp) {
                return None;
            }
            let next_node = self.fst.node(trans.addr);
            if next_node.is_final() {
                return Some((&self.inp, out.cat(next_node.final_output())));
            }
        }
        None
    }

    fn seek_min<T: AsRef<[u8]>>(&mut self, min: Bound<T>) {
        let (key, inclusive) = match min {
            Bound::Excluded(ref min) if min.as_ref().is_empty() => {
                self.stack = vec![FstReaderState {
                    addr: self.fst.root_addr,
                    trans: 0,
                    out: Output::zero(),
                }];
                return;
            }
            Bound::Excluded(ref min) => {
                (min.as_ref(), false)
            }
            Bound::Included(ref min) if !min.as_ref().is_empty() => {
                (min.as_ref(), true)
            }
            _ => {
                self.empty_output = self.fst.empty_final_output();
                self.stack = vec![FstReaderState {
                    addr: self.fst.root_addr,
                    trans: 0,
                    out: Output::zero(),
                }];
                return;
            }
        };
        // At this point, we need to find the starting location of `min` in
        // the FST. However, as we search, we need to maintain a stack of
        // reader states so that the reader can pick up where we left off.
        // N.B. We do not necessarily need to stop in a final state, unlike
        // the one-off `find` method. For the example, the given bound might
        // not actually exist in the FST.
        let mut node = self.fst.root();
        let mut out = Output::zero();
        for &b in key {
            match node.find_input(b) {
                Some(i) => {
                    let t = node.transition(i);
                    self.stack.push(FstReaderState {
                        addr: node.addr(),
                        trans: i+1,
                        out: out,
                    });
                    out = out.cat(t.out);
                    self.inp.push(b);
                    node = self.fst.node(t.addr);
                }
                None => {
                    // This is a little tricky. We're in this case if the
                    // given bound is not a prefix of any key in the FST.
                    // Since this is a minimum bound, we need to find the
                    // first transition in this node that proceeds the current
                    // input byte.
                    self.stack.push(FstReaderState {
                        addr: node.addr(),
                        trans: node.transitions()
                                   .position(|t| t.inp > b)
                                   .unwrap_or(node.len()),
                        out: out,
                    });
                    return;
                }
            }
        }
        if !self.stack.is_empty() {
            let last = self.stack.len() - 1;
            if inclusive {
                self.stack[last].trans -= 1;
                self.inp.pop();
            } else {
                let state = self.stack[last];
                let node = self.fst.node(state.addr);
                self.stack.push(FstReaderState {
                    addr: node.transition_addr(state.trans - 1),
                    trans: 0,
                    out: out,
                });
            }
        }
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
