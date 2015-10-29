/*!
Operations on raw finite state transducers.

This sub-module exposes the guts of a finite state transducer. Many parts of
it, such as construction and traversal, are mirrored in the `set` and `map`
sub-modules. Other parts of it, such as direct access to nodes and transitions
in the transducer, do not have any analog.

# Overview of types

`Fst` is a read only interface to pre-constructed finite state transducers.
`Node` is a read only interface to a single node in a transducer. `Builder` is
used to create new finite state transducers. (Once a transducer is created, it
can never be modified.) `Stream` is a stream of all inputs and outputs in a
transducer. `StreamBuilder` builds range queries. `OpBuilder` collects streams
and executes set operations like `union` or `intersection` on them with the
option of specifying a merge strategy for output values.

Most of the rest of the types are streams from set operations.
*/
use std::cmp;
use std::fmt;
use std::path::Path;

use byteorder::{ReadBytesExt, LittleEndian};
use memmap::{Mmap, Protection};

use automaton::{Automaton, AlwaysMatch};
use error::Result;
use stream::{IntoStreamer, Streamer};

pub use self::build::Builder;
pub use self::error::Error;
pub use self::node::{Node, Transitions};
use self::node::node_new;
pub use self::ops::{
    IndexedValue, OpBuilder,
    Intersection, Union, Difference, SymmetricDifference,
};

mod build;
mod common_inputs;
mod counting_writer;
mod error;
mod node;
mod ops;
mod pack;
mod registry;
#[cfg(test)] mod tests;

/// The API version of this crate.
///
/// This version number is written to every finite state transducer created by
/// this crate. When a finite state transducer is read, its version number is
/// checked against this value.
///
/// Currently, any version mismatch results in an error. Fixing this requires
/// regenerating the finite state transducer or switching to a version of this
/// crate that is compatible with the serialized transducer. This particular
/// behavior may be relaxed in future versions.
pub const VERSION: u64 = 1;

/// A sentinel value used to indicate an invalid state.
///
/// This is never the address of a node in a serialized transducer.
const NONE_ADDRESS: CompiledAddr = 1;

/// FstType is a convention used to indicate the type of the underlying
/// transducer.
///
/// This crate reserves the range 0-255 (inclusive) but currently leaves the
/// meaning of 0-255 unspecified.
pub type FstType = u64;

/// CompiledAddr is the type used to address nodes in a finite state
/// transducer.
///
/// It is most useful as a pointer to nodes. It can be used in the `Fst::node`
/// method to resolve the pointer.
pub type CompiledAddr = usize;

/// An acyclic deterministic finite state transducer.
///
/// # How does it work?
///
/// The short answer: it's just like a prefix trie, which compresses keys
/// based only on their prefixes, except that a automaton/transducer also
/// compresses suffixes.
///
/// The longer answer is that keys in an automaton are stored only in the
/// transitions from one state to another. A key can be acquired by tracing
/// a path from the root of the automaton to any match state. The inputs along
/// each transition are concatenated. Once a match state is reached, the
/// concatenation of inputs up until that point corresponds to a single key.
///
/// But why is it called a transducer instead of an automaton? A finite state
/// transducer is just like a finite state automaton, except that it has output
/// transitions in addition to input transitions. Namely, the value associated
/// with any particular key is determined by summing the outputs along every
/// input transition that leads to the key's corresponding match state.
///
/// This is best demonstrated with a couple images. First, let's ignore the
/// "transducer" aspect and focus on a plain automaton.
///
/// Consider that your keys are abbreviations of some of the months in the
/// Gregorian calendar:
///
/// ```ignore
/// jan
/// feb
/// mar
/// apr
/// may
/// jun
/// jul
/// ```
///
/// The corresponding automaton that stores all of these as keys looks like
/// this:
///
/// ![finite state automaton](http://burntsushi.net/stuff/months-set.png)
///
/// Notice here how the prefix and suffix of `jan` and `jun` are shared.
/// Similarly, the prefixes of `jun` and `jul` are shared and the suffixes
/// of `mar` and `may` are shared.
///
/// All of the keys from this automaton can be enumerated in lexicographic
/// order by following every transition from each node in lexicographic
/// order. Since it is acyclic, the procedure will terminate.
///
/// A key can be found by tracing it through the transitions in the automaton.
/// For example, the key `aug` is known not be in the automaton by only
/// visiting the first root state (because there is no `a` transition).
/// For another example, the key `jax` is known not to be in the set only after
/// moving through the transitions for `j` and `a`. Namely, after those
/// transitions are followed, there are no transitions for `x`.
///
/// Notice here that looking up a key is proportional the length of the key
/// itself. Namely, lookup time is not affected by the number of keys in the
/// automaton!
///
/// Additionally, notice that the automaton exploits the fact that many keys
/// share common prefixes and suffixes. For example, `jun` and `jul` are
/// represented with no more states than would be required to represent either
/// one on its own. Instead, the only change is a single extra transition. This
/// is a form of compression and is key to how the automatons produced by this
/// crate are so small.
///
/// Let's move on to finite state transducers. Consider the same set of keys
/// as above, but let's assign their numeric month values:
///
/// ```ignore
/// jan,1
/// feb,2
/// mar,3
/// apr,4
/// may,5
/// jun,6
/// jul,7
/// ```
///
/// The corresponding transducer looks very similar to the automaton above,
/// except outputs have been added to some of the transitions:
///
/// ![finite state transducer](http://burntsushi.net/stuff/months-map.png)
///
/// All of the operations with a transducer are the same as described above
/// for automatons. Additionally, the same compression techniques are used:
/// common prefixes and suffixes in keys are exploited.
///
/// The key difference is that some transitions have been given an output.
/// As one follows input transitions, one must sum the outputs as they
/// are seen. (A transition with no output represents the additive identity,
/// or `0` in this case.) For example, when looking up `feb`, the transition
/// `f` has output `2`, the transition `e` has output `0`, and the transition
/// `b` also has output `0`. The sum of these is `2`, which is exactly the
/// value we associated with `feb`.
///
/// For another more interesting example, consider `jul`. The `j` transition
/// has output `1`, the `u` transition has output `5` and the `l` transition
/// has output `1`. Summing these together gets us `7`, which is again the
/// correct value associated with `jul`. Notice that if we instead looked up
/// the `jun` key, then the `n` transition would be followed instead of the
/// `l` transition, which has no output. Therefore, the `jun` key equals
/// `1+5+0=6`.
///
/// The trick to transducers is that there exists a unique path through the
/// transducer for every key, and its outputs are stored appropriately along
/// this path such that the correct value is returned when they are all summed
/// together. This process also enables the data that makes up each value to be
/// shared across many values in the transducer in exactly the same way that
/// keys are shared. This is yet another form of compression!
///
/// # Does this crate produce minimal transducers?
///
/// For any non-trivial sized set of keys, it is unlikely that this crate will
/// produce a minimal transducer. As far as this author knows, guaranteeing a
/// minimal transducer requires working memory proportional to the number of
/// keys. This can be quite costly and is anathema to the main design goal of
/// this crate: provide the ability to work with gigantic sets of strings with
/// constant memory overhead.
///
/// Instead, construction of a finite state transducer uses a cache of
/// states. More frequently used states are cached and reused, which provides
/// reasonably good compression ratios. (No comprehensive benchmarks exist to
/// back up this claim.)
pub struct Fst {
    data: FstData,
    root_addr: CompiledAddr,
    ty: FstType,
    len: usize,
}

impl Fst {
    /// Opens a transducer stored at the given file path via a memory map.
    ///
    /// The fst must have been written with a compatible finite state
    /// transducer builder (`Builder` qualifies). If the format is invalid or
    /// if there is a mismatch between the API version of this library and the
    /// fst, then an error is returned.
    pub fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        Fst::new(FstData::Mmap(try!(Mmap::open_path(path, Protection::Read))))
    }

    /// Creates a transducer from its representation as a raw byte sequence.
    ///
    /// Note that this operation is very cheap (no allocations and no copies).
    ///
    /// The fst must have been written with a compatible finite state
    /// transducer builder (`Builder` qualifies). If the format is invalid or
    /// if there is a mismatch between the API version of this library and the
    /// fst, then an error is returned.
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self> {
        Fst::new(FstData::Owned(bytes))
    }
}

impl Fst {
    fn new(data: FstData) -> Result<Self> {
        if data.as_slice().len() < 32 {
            return Err(Error::Format.into());
        }
        // The read_u64 unwraps below are OK because they can never fail.
        // They can only fail when there is an IO error or if there is an
        // unexpected EOF. However, we are reading from a byte slice (no
        // IO errors possible) and we've confirmed the byte slice is at least
        // N bytes (no unexpected EOF).
        let version = data.as_slice().read_u64::<LittleEndian>().unwrap();
        if version != VERSION {
            return Err(Error::Version {
                expected: VERSION,
                got: version,
            }.into());
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

    /// Retrieves the value associated with a key.
    ///
    /// If the key does not exist, then `None` is returned.
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

    /// Return a lexicographically ordered stream of all key-value pairs in
    /// this fst.
    pub fn stream(&self) -> Stream {
        StreamBuilder::new(self, AlwaysMatch).into_stream()
    }

    /// Return a builder for range queries.
    ///
    /// A range query returns a subset of key-value pairs in this fst in a
    /// range given in lexicographic order.
    pub fn range(&self) -> StreamBuilder {
        StreamBuilder::new(self, AlwaysMatch)
    }

    /// Executes an automaton on the keys of this map.
    pub fn search<A: Automaton>(&self, aut: A) -> StreamBuilder<A> {
        StreamBuilder::new(self, aut)
    }

    /// Returns the number of keys in this fst.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if and only if this fst has no keys.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Creates a new fst operation with this fst added to it.
    ///
    /// The `OpBuilder` type can be used to add additional fst streams
    /// and perform set operations like union, intersection, difference and
    /// symmetric difference on the keys of the fst. These set operations also
    /// allow one to specify how conflicting values are merged in the stream.
    pub fn op(&self) -> OpBuilder {
        OpBuilder::new().add(self)
    }

    /// Returns true if and only if the `self` fst is disjoint with the fst
    /// `stream`.
    ///
    /// `stream` must be a lexicographically ordered sequence of byte strings
    /// with associated values.
    pub fn is_disjoint<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStreamer<'a, Into=S, Item=(&'a [u8], Output)>,
                  S: 'f + for<'a> Streamer<'a, Item=(&'a [u8], Output)> {
        self.op().add(stream).intersection().next().is_none()
    }

    /// Returns true if and only if the `self` fst is a subset of the fst
    /// `stream`.
    ///
    /// `stream` must be a lexicographically ordered sequence of byte strings
    /// with associated values.
    pub fn is_subset<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStreamer<'a, Into=S, Item=(&'a [u8], Output)>,
                  S: 'f + for<'a> Streamer<'a, Item=(&'a [u8], Output)> {
        let mut op = self.op().add(stream).intersection();
        let mut count = 0;
        while let Some(_) = op.next() {
            count += 1;
        }
        count == self.len()
    }

    /// Returns true if and only if the `self` fst is a superset of the fst
    /// `stream`.
    ///
    /// `stream` must be a lexicographically ordered sequence of byte strings
    /// with associated values.
    pub fn is_superset<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStreamer<'a, Into=S, Item=(&'a [u8], Output)>,
                  S: 'f + for<'a> Streamer<'a, Item=(&'a [u8], Output)> {
        let mut op = self.op().add(stream).union();
        let mut count = 0;
        while let Some(_) = op.next() {
            count += 1;
        }
        count == self.len()
    }

    /// Returns the underlying type of this fst.
    ///
    /// FstType is a convention used to indicate the type of the underlying
    /// transducer.
    ///
    /// This crate reserves the range 0-255 (inclusive) but currently leaves
    /// the meaning of 0-255 unspecified.
    pub fn fst_type(&self) -> FstType {
        self.ty
    }

    /// Returns the root node of this fst.
    pub fn root(&self) -> Node {
        self.node(self.root_addr)
    }

    /// Returns the node at the given address.
    ///
    /// Node addresses can be obtained by reading transitions on `Node` values.
    pub fn node(&self, addr: CompiledAddr) -> Node {
        node_new(addr, &self.data.as_slice())
    }

    /// Return the raw byte representation of the underlying fst.
    pub fn as_slice(&self) -> &[u8] {
        self.data.as_slice()
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

impl<'a, 'f> IntoStreamer<'a> for &'f Fst {
    type Item = (&'a [u8], Output);
    type Into = Stream<'f>;

    fn into_stream(self) -> Self::Into {
        StreamBuilder::new(self, AlwaysMatch).into_stream()
    }
}

/// A builder for constructing range queries on streams.
///
/// Once all bounds are set, one should call `into_stream` to get a
/// `Stream`.
///
/// Bounds are not additive. That is, if `ge` is called twice on the same
/// builder, then the second setting wins.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'f` lifetime parameter refers to the lifetime of the underlying fst.
pub struct StreamBuilder<'f, A=AlwaysMatch> {
    fst: &'f Fst,
    aut: A,
    min: Bound,
    max: Bound,
}

impl<'f, A: Automaton> StreamBuilder<'f, A> {
    fn new(fst: &'f Fst, aut: A) -> Self {
        StreamBuilder {
            fst: fst,
            aut: aut,
            min: Bound::Unbounded,
            max: Bound::Unbounded,
        }
    }

    /// Specify a greater-than-or-equal-to bound.
    pub fn ge<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.min = Bound::Included(bound.as_ref().to_owned());
        self
    }

    /// Specify a greater-than bound.
    pub fn gt<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.min = Bound::Excluded(bound.as_ref().to_owned());
        self
    }

    /// Specify a less-than-or-equal-to bound.
    pub fn le<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.max = Bound::Included(bound.as_ref().to_owned());
        self
    }

    /// Specify a less-than bound.
    pub fn lt<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        self.max = Bound::Excluded(bound.as_ref().to_owned());
        self
    }
}

impl<'a, 'f, A: Automaton> IntoStreamer<'a> for StreamBuilder<'f, A> {
    type Item = (&'a [u8], Output);
    type Into = Stream<'f, A>;

    fn into_stream(self) -> Stream<'f, A> {
        Stream::new(self.fst, self.aut, self.min, self.max)
    }
}

#[derive(Debug)]
enum Bound {
    Included(Vec<u8>),
    Excluded(Vec<u8>),
    Unbounded,
}

impl Bound {
    fn exceeded_by(&self, inp: &[u8]) -> bool {
        match *self {
            Bound::Included(ref v) => inp > v,
            Bound::Excluded(ref v) => inp >= v,
            Bound::Unbounded => false,
        }
    }
}

/// A lexicographically ordered stream of key-value pairs from an fst.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'f` lifetime parameter refers to the lifetime of the underlying fst.
pub struct Stream<'f, A=AlwaysMatch> where A: Automaton {
    fst: &'f Fst,
    aut: A,
    inp: Vec<u8>,
    empty_output: Option<Output>,
    stack: Vec<StreamState<A::State>>,
    end_at: Bound,
}

#[derive(Clone, Debug)]
struct StreamState<S> {
    addr: CompiledAddr,
    trans: usize,
    out: Output,
    aut_state: S,
}

impl<'f, A: Automaton> Stream<'f, A> {
    fn new(fst: &'f Fst, aut: A, min: Bound, max: Bound) -> Self {
        let mut rdr = Stream {
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

    /// Seeks the underlying stream such that the next key to be read is the
    /// smallest key in the underlying fst that satisfies the given minimum
    /// bound.
    ///
    /// This theoretically should be straight-forward, but we need to make
    /// sure our stack is correct, which includes accounting for automaton
    /// states.
    fn seek_min(&mut self, min: Bound) {
        let (key, inclusive) = match min {
            Bound::Excluded(ref min) if min.is_empty() => {
                self.stack = vec![StreamState {
                    addr: self.fst.root_addr,
                    trans: 0,
                    out: Output::zero(),
                    aut_state: self.aut.start(),
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
                self.stack = vec![StreamState {
                    addr: self.fst.root_addr,
                    trans: 0,
                    out: Output::zero(),
                    aut_state: self.aut.start(),
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
        let mut aut_state = self.aut.start();
        for &b in key {
            match node.find_input(b) {
                Some(i) => {
                    let t = node.transition(i);
                    let next_state = self.aut.accept(&aut_state, b);
                    aut_state = self.aut.accept(&aut_state, b);
                    self.stack.push(StreamState {
                        addr: node.addr(),
                        trans: i+1,
                        out: out,
                        aut_state: next_state,
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
                    self.stack.push(StreamState {
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
                let addr = self.stack[last].addr;
                let trans = self.stack[last].trans;
                let node = self.fst.node(addr);
                self.stack.push(StreamState {
                    addr: node.transition(trans - 1).addr,
                    trans: 0,
                    out: out,
                    aut_state: aut_state,
                });
            }
        }
    }

    /// Convert this stream into a vector of byte strings and outputs.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_vec(mut self) -> Vec<(Vec<u8>, Output)> {
        let mut vs = vec![];
        while let Some((k, v)) = self.next() {
            vs.push((k.to_vec(), v));
        }
        vs
    }

    /// Convert this stream into a vector of byte strings.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_keys(mut self) -> Vec<Vec<u8>> {
        let mut vs = vec![];
        while let Some((k, _)) = self.next() {
            vs.push(k.to_vec());
        }
        vs
    }

    /// Convert this stream into a vector of outputs.
    pub fn into_values(mut self) -> Vec<Output> {
        let mut vs = vec![];
        while let Some((_, v)) = self.next() {
            vs.push(v);
        }
        vs
    }
}

impl<'f, 'a, A: Automaton> Streamer<'a> for Stream<'f, A> {
    type Item = (&'a [u8], Output);

    fn next(&'a mut self) -> Option<Self::Item> {
        if let Some(out) = self.empty_output.take() {
            if self.end_at.exceeded_by(&[]) {
                self.stack.clear();
                return None;
            }
            if self.aut.is_match(&self.aut.start()) {
                return Some((&[], out));
            }
        }
        while let Some(state) = self.stack.pop() {
            let node = self.fst.node(state.addr);
            if state.trans >= node.len()
                    || !self.aut.can_match(&state.aut_state) {
                if node.addr() != self.fst.root_addr {
                    self.inp.pop().unwrap();
                }
                continue;
            }
            let trans = node.transition(state.trans);
            let out = state.out.cat(trans.out);
            let next_state = self.aut.accept(&state.aut_state, trans.inp);
            let is_match = self.aut.is_match(&next_state);
            self.inp.push(trans.inp);
            self.stack.push(StreamState {
                addr: state.addr,
                trans: state.trans + 1,
                out: state.out,
                aut_state: state.aut_state,
            });
            self.stack.push(StreamState {
                addr: trans.addr,
                trans: 0,
                out: out,
                aut_state: next_state,
            });
            if self.end_at.exceeded_by(&self.inp) {
                // We are done, forever.
                self.stack.clear();
                return None;
            }
            let next_node = self.fst.node(trans.addr);
            if next_node.is_final() && is_match {
                return Some((&self.inp, out.cat(next_node.final_output())));
            }
        }
        None
    }
}

/// An output is a value that is associated with a key in a finite state
/// transducer.
///
/// Note that outputs must satisfy an algebra. Namely, it must have an additive
/// identity and the following binary operations defined: `prefix`,
/// `concatenation` and `subtraction`. `prefix` and `concatenation` are
/// commutative while `subtraction` is not. `subtraction` is only defined on
/// pairs of operands where the first operand is greater than or equal to the
/// second operand.
///
/// Currently, output values must be `u64`. However, in theory, an output value
/// can be anything that satisfies the above algebra. Future versions of this
/// crate may make outputs generic on this algebra.
#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Output(u64);

impl Output {
    /// Create a new output from a `u64`.
    pub fn new(v: u64) -> Output {
        Output(v)
    }

    /// Create a zero output.
    pub fn zero() -> Output {
        Output(0)
    }

    /// Retrieve the value inside this output.
    pub fn value(self) -> u64 {
        self.0
    }

    /// Returns true if this is a zero output.
    pub fn is_zero(self) -> bool {
        self.0 == 0
    }

    /// Returns the prefix of this output and `o`.
    pub fn prefix(self, o: Output) -> Output {
        Output(cmp::min(self.0, o.0))
    }

    /// Returns the concatenation of this output and `o`.
    pub fn cat(self, o: Output) -> Output {
        Output(self.0 + o.0)
    }

    /// Returns the subtraction of `o` from this output.
    ///
    /// This function panics if `self > o`.
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

/// A transition from one note to another.
#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Transition {
    /// The byte input associated with this transition.
    pub inp: u8,
    /// The output associated with this transition.
    pub out: Output,
    /// The address of the node that this transition points to.
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
