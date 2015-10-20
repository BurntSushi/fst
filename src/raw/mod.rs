use std::cmp;
use std::fmt;
use std::path::Path;

use byteorder::{ReadBytesExt, LittleEndian};
use memmap::{Mmap, Protection};

use automaton::{Automaton, AlwaysMatch};
use error::{Error, Result};
use stream::{IntoStream, Stream};

pub use self::build::Builder;
pub use self::node::{Node, Transitions};
use self::node::node_new;
pub use self::ops::{
    FstOutput, StreamOp,
    StreamIntersection, StreamUnion,
    StreamDifference, StreamSymmetricDifference,
};

mod build;
mod common_inputs;
mod counting_writer;
mod node;
mod ops;
mod pack;
mod registry;
mod registry_minimal;
#[cfg(test)] mod tests;

pub const VERSION: u64 = 1;
pub const NONE_ADDRESS: CompiledAddr = 1;

/// FstType is a convention used to indicate the type of the underlying
/// transducer.
///
/// This crate reserves the range 0-255 (inclusive) but currently leaves the
/// meaning of 0-255 unspecified.
pub type FstType = u64;

/// CompiledAddr is the type used to address nodes in a finite state
/// transducer.
pub type CompiledAddr = usize;

pub struct Fst {
    data: FstData,
    root_addr: CompiledAddr,
    ty: FstType,
    len: usize,
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
        if data.as_slice().len() < 32 {
            return Err(Error::Format);
        }
        // The read_u64 unwraps below are OK because they can never fail.
        // They can only fail when there is an IO error or if there is an
        // unexpected EOF. However, we are reading from a byte slice (no
        // IO errors possible) and we've confirmed the byte slice is at least
        // 24 bytes (no unexpected EOF).
        let version = data.as_slice().read_u64::<LittleEndian>().unwrap();
        if version != VERSION {
            return Err(Error::Version {
                expected: VERSION,
                got: version,
            });
        }
        let ty = (&data.as_slice()[8..]).read_u64::<LittleEndian>().unwrap();
        let root_addr = {
            let mut last = &data.as_slice()[data.as_slice().len() - 8..];
            last.read_u64::<LittleEndian>().unwrap()
        };
        let len = {
            let mut last2 = &data.as_slice()[data.as_slice().len() - 16..];
            last2.read_u64::<LittleEndian>().unwrap()
        };
        Ok(Fst {
            data: data,
            root_addr: u64_to_usize(root_addr),
            ty: ty,
            len: u64_to_usize(len),
        })
    }

    pub fn stream(&self) -> FstStream {
        FstStreamBuilder::new(self, AlwaysMatch).into_stream()
    }

    pub fn range(&self) -> FstStreamBuilder {
        FstStreamBuilder::new(self, AlwaysMatch)
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

    pub fn search<A: Automaton>(&self, aut: A) -> FstStreamBuilder<A> {
        FstStreamBuilder::new(self, aut)
    }

    pub fn fst_type(&self) -> FstType {
        self.ty
    }

    pub fn root(&self) -> Node {
        self.node(self.root_addr)
    }

    pub fn node(&self, addr: CompiledAddr) -> Node {
        node_new(addr, &self.data.as_slice())
    }

    pub fn as_slice(&self) -> &[u8] {
        self.data.as_slice()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn empty_final_output(&self) -> Option<Output> {
        let root = self.root();
        if root.is_final() {
            Some(root.final_output())
        } else {
            None
        }
    }
}

impl<'a, 'f> IntoStream<'a> for &'f Fst {
    type Item = (&'a [u8], Output);
    type Into = FstStream<'f>;

    fn into_stream(self) -> Self::Into {
        FstStreamBuilder::new(self, AlwaysMatch).into_stream()
    }
}

pub struct FstStreamBuilder<'a, A=AlwaysMatch> {
    fst: &'a Fst,
    aut: A,
    min: Bound,
    max: Bound,
}

impl<'f, A: Automaton> FstStreamBuilder<'f, A> {
    fn new(fst: &'f Fst, aut: A) -> Self {
        FstStreamBuilder {
            fst: fst,
            aut: aut,
            min: Bound::Unbounded,
            max: Bound::Unbounded,
        }
    }

    pub fn ge<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.min = Bound::Included(bound.as_ref().to_owned());
        self
    }

    pub fn gt<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.min = Bound::Excluded(bound.as_ref().to_owned());
        self
    }

    pub fn le<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.max = Bound::Included(bound.as_ref().to_owned());
        self
    }

    pub fn lt<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.max = Bound::Excluded(bound.as_ref().to_owned());
        self
    }
}

impl<'a, 'f, A: Automaton> IntoStream<'a> for FstStreamBuilder<'f, A> {
    type Item = (&'a [u8], Output);
    type Into = FstStream<'f, A>;

    fn into_stream(self) -> FstStream<'f, A> {
        FstStream::new(self.fst, self.aut, self.min, self.max)
    }
}

#[derive(Debug)]
enum Bound {
    Included(Vec<u8>),
    Excluded(Vec<u8>),
    Unbounded,
}

impl Bound {
    fn includes_empty(&self) -> bool {
        match *self {
            Bound::Included(ref v) => v == &[],
            Bound::Excluded(_) => false,
            Bound::Unbounded => true,
        }
    }

    fn exceeded_by(&self, inp: &[u8]) -> bool {
        match *self {
            Bound::Included(ref v) => inp > v,
            Bound::Excluded(ref v) => inp >= v,
            Bound::Unbounded => false,
        }
    }
}

pub struct FstStream<'f, A=AlwaysMatch> where A: Automaton {
    fst: &'f Fst,
    aut: A,
    inp: Vec<u8>,
    empty_output: Option<Output>,
    stack: Vec<FstStreamState<A::State>>,
    end_at: Bound,
}

#[derive(Clone, Copy, Debug)]
struct FstStreamState<S> {
    addr: CompiledAddr,
    trans: usize,
    out: Output,
    aut_state: Option<S>,
}

impl<'f, A: Automaton> FstStream<'f, A> {
    fn new(fst: &'f Fst, aut: A, min: Bound, max: Bound) -> Self {
        let mut rdr = FstStream {
            fst: fst,
            aut: aut,
            inp: Vec::with_capacity(16),
            empty_output: None,
            stack: vec![],
            end_at: max,
        };
        rdr.seek_min(min);
        rdr
    }

    fn seek_min(&mut self, min: Bound) {
        let (key, inclusive) = match min {
            Bound::Excluded(ref min) if min.is_empty() => {
                self.stack = vec![FstStreamState {
                    addr: self.fst.root_addr,
                    trans: 0,
                    out: Output::zero(),
                    aut_state: Some(self.aut.start()),
                }];
                return;
            }
            Bound::Excluded(ref min) => {
                (min, false)
            }
            Bound::Included(ref min) if !min.is_empty() => {
                (min, true)
            }
            _ => {
                self.empty_output = self.fst.empty_final_output();
                self.stack = vec![FstStreamState {
                    addr: self.fst.root_addr,
                    trans: 0,
                    out: Output::zero(),
                    aut_state: Some(self.aut.start()),
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
        let mut aut_state = Some(self.aut.start());
        for &b in key {
            match node.find_input(b) {
                Some(i) => {
                    let t = node.transition(i);
                    self.stack.push(FstStreamState {
                        addr: node.addr(),
                        trans: i+1,
                        out: out,
                        aut_state: aut_state,
                    });
                    aut_state = aut_state.and_then(|s| self.aut.accept(s, b));
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
                    self.stack.push(FstStreamState {
                        addr: node.addr(),
                        trans: node.transitions()
                                   .position(|t| t.inp > b)
                                   .unwrap_or(node.len()),
                        out: out,
                        aut_state: aut_state,
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
                self.stack.push(FstStreamState {
                    addr: node.transition_addr(state.trans - 1),
                    trans: 0,
                    out: out,
                    aut_state: aut_state,
                });
            }
        }
    }
}

impl<'f, 'a, A: Automaton> Stream<'a> for FstStream<'f, A> {
    type Item = (&'a [u8], Output);

    fn next(&'a mut self) -> Option<Self::Item> {
        if let Some(out) = self.empty_output.take() {
            if self.end_at.exceeded_by(&[]) {
                self.stack.clear();
                return None;
            }
            if self.aut.is_match(self.aut.start()) {
                return Some((&[], out));
            }
        }
        while let Some(state) = self.stack.pop() {
            let node = self.fst.node(state.addr);
            if state.trans >= node.len() || state.aut_state.is_none() {
                if node.addr() != self.fst.root_addr {
                    self.inp.pop().unwrap();
                }
                continue;
            }
            let trans = node.transition(state.trans);
            let out = state.out.cat(trans.out);
            self.stack.push(FstStreamState {
                addr: state.addr,
                trans: state.trans + 1,
                out: state.out,
                aut_state: state.aut_state,
            });
            let next_state =
                match self.aut.accept(state.aut_state.unwrap(), trans.inp) {
                    None => continue,
                    Some(state) => state,
                };
            self.inp.push(trans.inp);
            self.stack.push(FstStreamState {
                addr: trans.addr,
                trans: 0,
                out: out,
                aut_state: Some(next_state),
            });
            if self.end_at.exceeded_by(&self.inp) {
                return None;
            }
            let next_node = self.fst.node(trans.addr);
            if next_node.is_final() && self.aut.is_match(next_state) {
                return Some((&self.inp, out.cat(next_node.final_output())));
            }
        }
        None
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Output(u64);

impl Output {
    pub fn new(v: u64) -> Output {
        Output(v)
    }

    pub fn zero() -> Output {
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
            addr: NONE_ADDRESS,
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

#[inline]
#[cfg(target_pointer_width = "64")]
fn u64_to_usize(n: u64) -> usize {
    n as usize
}

#[inline]
#[cfg(not(target_pointer_width = "64"))]
fn u64_to_usize(n: u64) -> usize {
    if n > ::std::usize::MAX as u64 {
        panic!("\
Cannot convert node address {} to a pointer sized variable. If this FST
is very large and was generated on a system with a larger pointer size
than this system, then it is not possible to read this FST on this
system.", n);
    }
    n as usize
}
