use std::cmp;
use std::fmt;
use std::io;
use std::ops::Range;

use byteorder::WriteBytesExt;
use memchr::memchr;

use raw::{EMPTY_ADDRESS, CompiledAddr, Output, Transition, u64_to_usize};
use raw::build::BuilderNode;
use raw::common_inputs::{COMMON_INPUTS, COMMON_INPUTS_INV};
use raw::pack::{pack_size, pack_uint, pack_uint_in, unpack_uint};

/// Node represents a single state in a finite state transducer.
///
/// Nodes are very cheap to construct. Notably, they satisfy the `Copy` trait.
#[derive(Clone, Copy)]
pub struct Node<'f> {
    data: &'f [u8],
    state: State,
}

impl<'f> fmt::Debug for Node<'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "NODE@{}", self.start_addr()));
        try!(writeln!(f, "  end_addr: {}", self.end_addr()));
        try!(writeln!(f, "  size: {} bytes", self.as_slice().len()));
        try!(writeln!(f, "  state: {:?}", self.state));
        try!(writeln!(f, "  is_final: {}", self.is_final()));
        try!(writeln!(f, "  final_output: {:?}", self.final_output()));
        try!(writeln!(f, "  # transitions: {}", self.len()));
        try!(writeln!(f, "  transitions:"));
        for t in self.transitions() {
            try!(writeln!(f, "    {:?}", t));
        }
        Ok(())
    }
}

/// Creates a new note at the address given.
///
/// `data` should be a slice to an entire FST.
///
/// This is a free function so that we can export it to parent modules, but
/// not to consumers of this crate.
pub fn node_new(addr: CompiledAddr, data: &[u8]) -> Node {
    if addr == EMPTY_ADDRESS {
        Node {
            data: &[],
            state: State::EmptyFinal,
        }
    } else {
        Node {
            data: &data[..addr + 1],
            state: State::from_u8(data[addr]),
        }
    }
}

impl<'f> Node<'f> {
    /// Returns an iterator over all transitions in this node in lexicographic
    /// order.
    pub fn transitions<'n>(&'n self) -> Transitions<'f, 'n> {
        Transitions { node: self, range: 0..self.len() as u8 }
    }

    /// Returns the transition at index `i`.
    pub fn transition(&self, i: usize) -> Transition {
        Transition {
            inp: self.input(i),
            out: self.output(i),
            addr: self.transition_addr(i),
        }
    }

    /// Returns the input byte at transition `i`.
    fn input(&self, i: usize) -> u8 {
        self.state.input(self, i)
    }

    /// Returns the output value at transition `i`.
    fn output(&self, i: usize) -> Output {
        self.state.output(self, i)
    }

    /// Returns the transition address pointed to by transition `i`.
    fn transition_addr(&self, i: usize) -> CompiledAddr {
        self.state.trans_addr(self, i)
    }

    /// Finds the `i`th transition corresponding to the given input byte.
    ///
    /// If no transition for this byte exists, then `None` is returned.
    pub fn find_input(&self, b: u8) -> Option<usize> {
        self.state.find_input(self, b)
    }

    /// If this node is final and has a terminal output value, then it is
    /// returned. Otherwise, a zero output is returned.
    pub fn final_output(&self) -> Output {
        self.state.final_output(self)
    }

    /// Returns true if and only if this node corresponds to a final or "match"
    /// state in the finite state transducer.
    pub fn is_final(&self) -> bool {
        self.state.is_final()
    }

    /// Returns the number of transitions in this node.
    ///
    /// The maximum number of transitions is 256.
    pub fn len(&self) -> usize {
        self.state.ntrans(self)
    }

    /// Returns true if and only if this node has zero transitions.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Return the address of this node.
    pub fn addr(&self) -> CompiledAddr {
        self.start_addr() as CompiledAddr
    }

    fn start_addr(&self) -> usize {
        if self.data.len() == 0 {
            EMPTY_ADDRESS
        } else {
            self.data.len() - 1
        }
    }

    fn end_addr(&self) -> usize {
        self.state.end_addr(self)
    }

    #[doc(hidden)]
    pub fn as_slice(&self) -> &[u8] {
        &self.data[self.end_addr()..]
    }

    #[doc(hidden)]
    pub fn state(&self) -> &'static str {
        use self::State::*;
        match self.state {
            OneTransNext(_) => "OTN",
            OneTrans(_) => "OT",
            AnyTrans(_) => "AT",
            EmptyFinal => "EF",
        }
    }
}

impl BuilderNode {
    pub fn compile_to<W: io::Write>(
        &self, wtr: W, last_addr: CompiledAddr, addr: CompiledAddr,
    ) -> io::Result<()> {
        assert!(self.trans.len() <= ::std::u8::MAX as usize);
        State::compile(wtr, last_addr, addr, self)
    }
}

#[derive(Clone, Copy, Debug)]
enum State {
    OneTransNext(StateOneTransNext),
    OneTrans(StateOneTrans),
    AnyTrans(StateAnyTrans),
    EmptyFinal,
}

// one trans flag (1), next flag (1), common input
#[derive(Clone, Copy, Debug)]
struct StateOneTransNext(u8);
// one trans flag (1), next flag (0), common input
#[derive(Clone, Copy, Debug)]
struct StateOneTrans(u8);
// one trans flag (0), final flag, # transitions
#[derive(Clone, Copy, Debug)]
struct StateAnyTrans(u8);

impl State {
    #[inline]
    fn from_u8(v: u8) -> State {
        use self::State::*;
        match (v & 0b11_000000) >> 6 {
            0b11 => OneTransNext(StateOneTransNext(v)),
            0b10 => OneTrans(StateOneTrans(v)),
            _ => AnyTrans(StateAnyTrans(v)),
        }
    }

    fn compile<W: io::Write>(
        wtr: W,
        last_addr: CompiledAddr,
        addr: CompiledAddr,
        node: &BuilderNode,
    ) -> io::Result<()> {
        assert!(node.trans.len() <= ::std::u8::MAX as usize);
        if node.trans.is_empty()
            && node.is_final
            && node.final_output.is_zero() {
            return Ok(());
        } else if node.trans.len() != 1 || node.is_final {
            StateAnyTrans::compile(wtr, addr, node)
        } else {
            if !node.is_final
                && node.trans[0].addr == last_addr
                && node.trans[0].out.is_zero() {
                StateOneTransNext::compile(wtr, addr, node.trans[0].inp)
            } else {
                StateOneTrans::compile(wtr, addr, node.trans[0])
            }
        }
    }

    fn is_final(&self) -> bool {
        use self::State::*;
        match *self {
            OneTransNext(_) | OneTrans(_) => false,
            AnyTrans(s) => s.is_final_state(),
            EmptyFinal => true,
        }
    }

    fn ntrans(&self, node: &Node) -> usize {
        use self::State::*;
        match *self {
            OneTransNext(_)
            | OneTrans(_) => 1,
            AnyTrans(s) => s.ntrans(node),
            EmptyFinal => 0,
        }
    }

    fn input(&self, node: &Node, i: usize) -> u8 {
        use self::State::*;
        match *self {
            OneTransNext(s) => { assert!(i == 0); s.input(node) }
            OneTrans(s) => { assert!(i == 0); s.input(node) }
            AnyTrans(s) => s.input(node, i),
            EmptyFinal => panic!("out of bounds"),
        }
    }

    fn find_input(&self, node: &Node, b: u8) -> Option<usize> {
        use self::State::*;
        match *self {
            OneTransNext(s) if s.input(node) == b => Some(0),
            OneTransNext(_) => None,
            OneTrans(s) if s.input(node) == b => Some(0),
            OneTrans(_) => None,
            AnyTrans(s) => s.find_input(node, b),
            EmptyFinal => None,
        }
    }

    fn output(&self, node: &Node, i: usize) -> Output {
        use self::State::*;
        match *self {
            OneTransNext(_) => { assert!(i == 0); Output::zero() }
            OneTrans(s) => { assert!(i == 0); s.output(node) }
            AnyTrans(s) => s.output(node, i),
            EmptyFinal => Output::zero(),
        }
    }

    fn final_output(&self, node: &Node) -> Output {
        use self::State::*;
        match *self {
            OneTransNext(_)
            | OneTrans(_) => Output::zero(),
            AnyTrans(s) => s.final_output(node),
            EmptyFinal => Output::zero(),
        }
    }

    fn trans_addr(&self, node: &Node, i: usize) -> CompiledAddr {
        use self::State::*;
        match *self {
            OneTransNext(s) => { assert!(i == 0); s.trans_addr(node) }
            OneTrans(s) => { assert!(i == 0); s.trans_addr(node) }
            AnyTrans(s) => s.trans_addr(node, i),
            EmptyFinal => panic!("out of bounds"),
        }
    }

    fn end_addr(&self, node: &Node) -> usize {
        use self::State::*;
        match *self {
            OneTransNext(s) => s.end_addr(node),
            OneTrans(s) => s.end_addr(node),
            AnyTrans(s) => s.end_addr(node),
            EmptyFinal => EMPTY_ADDRESS,
        }
    }
}

impl StateOneTransNext {
    fn compile<W: io::Write>(
        mut wtr: W, _: CompiledAddr, input: u8,
    ) -> io::Result<()> {
        let mut state = StateOneTransNext::new();
        state.set_common_input(input);
        if state.common_input().is_none() {
            try!(wtr.write_u8(input));
        }
        wtr.write_u8(state.0).map_err(From::from)
    }

    #[inline]
    fn new() -> Self {
        StateOneTransNext(0b11_000000)
    }

    #[inline]
    fn set_common_input(&mut self, input: u8) {
        self.0 = (self.0 & 0b11_000000) | common_idx(input, 0b111111);
    }

    #[inline]
    fn common_input(&self) -> Option<u8> {
        common_input(self.0 & 0b00_111111)
    }

    fn input_len(&self) -> usize {
        if self.common_input().is_none() { 1 } else { 0 }
    }

    fn input(&self, node: &Node) -> u8 {
        if let Some(inp) = self.common_input() {
            inp
        } else {
            node.data[node.start_addr() - 1]
        }
    }

    fn trans_addr(&self, node: &Node) -> CompiledAddr {
        self.end_addr(node) as CompiledAddr - 1
    }

    fn end_addr(&self, node: &Node) -> usize {
        node.start_addr() - self.input_len()
    }
}

impl StateOneTrans {
    fn compile<W: io::Write>(
        mut wtr: W, addr: CompiledAddr, trans: Transition,
    ) -> io::Result<()> {
        let out = trans.out.value();
        let output_pack_size = if out == 0 {
            0
        } else {
            try!(pack_uint(&mut wtr, out))
        };
        let trans_pack_size = try!(pack_delta(&mut wtr, addr, trans.addr));

        let mut pack_sizes = PackSizes::new();
        pack_sizes.set_output_pack_size(output_pack_size);
        pack_sizes.set_transition_pack_size(trans_pack_size);
        try!(wtr.write_u8(pack_sizes.encode()));

        let mut state = StateOneTrans::new();
        state.set_common_input(trans.inp);
        if state.common_input().is_none() {
            try!(wtr.write_u8(trans.inp));
        }
        wtr.write_u8(state.0).map_err(From::from)
    }

    #[inline]
    fn new() -> Self {
        StateOneTrans(0b10_000000)
    }

    #[inline]
    fn set_common_input(&mut self, input: u8) {
        self.0 = (self.0 & 0b10_000000) | common_idx(input, 0b111111);
    }

    #[inline]
    fn common_input(&self) -> Option<u8> {
        common_input(self.0 & 0b00_111111)
    }

    fn input_len(&self) -> usize {
        if self.common_input().is_none() { 1 } else { 0 }
    }

    fn sizes(&self, node: &Node) -> PackSizes {
        let i = node.start_addr() - self.input_len() - 1;
        PackSizes::decode(node.data[i])
    }

    fn input(&self, node: &Node) -> u8 {
        if let Some(inp) = self.common_input() {
            inp
        } else {
            node.data[node.start_addr() - 1]
        }
    }

    fn output(&self, node: &Node) -> Output {
        let osize = self.sizes(node).output_pack_size();
        if osize == 0 {
            return Output::zero();
        }
        let tsize = self.sizes(node).transition_pack_size();
        let i = node.start_addr()
                - self.input_len()
                - 1 // pack size
                - tsize - osize;
        Output::new(unpack_uint(&node.data[i..], osize as u8).unwrap())
    }

    fn trans_addr(&self, node: &Node) -> CompiledAddr {
        let tsize = self.sizes(node).transition_pack_size();
        let i = node.start_addr()
                - self.input_len()
                - 1 // pack size
                - tsize;
        unpack_delta(&node.data[i..], tsize, self.end_addr(node))
    }

    fn end_addr(&self, node: &Node) -> usize {
        let tsize = self.sizes(node).transition_pack_size();
        let osize = self.sizes(node).output_pack_size();
        node.start_addr()
        - self.input_len()
        - 1 // pack size
        - tsize - osize
    }
}

impl StateAnyTrans {
    fn compile<W: io::Write>(
        mut wtr: W, addr: CompiledAddr, node: &BuilderNode,
    ) -> io::Result<()> {
        assert!(node.trans.len() <= ::std::u8::MAX as usize);

        let mut tsize = 0;
        let mut osize = pack_size(node.final_output.value());
        let mut any_outs = !node.final_output.is_zero();
        for t in &node.trans {
            tsize = cmp::max(tsize, pack_delta_size(addr, t.addr));
            osize = cmp::max(osize, pack_size(t.out.value()));
            any_outs = any_outs || !t.out.is_zero();
        }

        let mut pack_sizes = PackSizes::new();
        if any_outs {
            pack_sizes.set_output_pack_size(osize);
        } else {
            pack_sizes.set_output_pack_size(0);
        }
        pack_sizes.set_transition_pack_size(tsize);

        let mut state = StateAnyTrans::new();
        state.set_final_state(node.is_final);
        state.set_state_ntrans(node.trans.len() as u8);

        if any_outs {
            if node.is_final {
                try!(pack_uint_in(&mut wtr, node.final_output.value(), osize));
            }
            for t in node.trans.iter().rev() {
                try!(pack_uint_in(&mut wtr, t.out.value(), osize));
            }
        }
        for t in node.trans.iter().rev() {
            try!(pack_delta_in(&mut wtr, addr, t.addr, tsize));
        }
        for t in node.trans.iter().rev() {
            try!(wtr.write_u8(t.inp));
        }
        try!(wtr.write_u8(pack_sizes.encode()));
        if state.state_ntrans().is_none() {
            try!(wtr.write_u8(node.trans.len() as u8));
        }
        wtr.write_u8(state.0).map_err(From::from)
    }

    #[inline]
    fn new() -> Self {
        StateAnyTrans(0b00_000000)
    }

    #[inline]
    fn set_final_state(&mut self, yes: bool) {
        if yes {
            self.0 |= 0b01_000000;
        }
    }

    #[inline]
    fn is_final_state(&self) -> bool {
        self.0 & 0b01_000000 == 0b01_000000
    }

    #[inline]
    fn set_state_ntrans(&mut self, n: u8) {
        if n <= 0b00_111111 {
            self.0 = (self.0 & 0b11_000000) | n;
        }
    }

    #[inline]
    fn state_ntrans(&self) -> Option<u8> {
        let n = self.0 & 0b00_111111;
        if n == 0 {
            None
        } else {
            Some(n)
        }
    }

    fn sizes(&self, node: &Node) -> PackSizes {
        let i = node.start_addr() - self.ntrans_len() - 1;
        PackSizes::decode(node.data[i])
    }

    fn ntrans_len(&self) -> usize {
        if self.state_ntrans().is_none() { 1 } else { 0 }
    }

    fn ntrans(&self, node: &Node) -> usize {
        if let Some(n) = self.state_ntrans() {
            n as usize
        } else {
            node.data[node.start_addr() - 1] as usize
        }
    }

    fn input(&self, node: &Node, i: usize) -> u8 {
        let at = node.start_addr()
                 - self.ntrans_len()
                 - 1 // pack size
                 - i
                 - 1; // the input byte
        node.data[at]
    }

    fn find_input(&self, node: &Node, b: u8) -> Option<usize> {
        let ntrans = self.ntrans(node);
        let start = node.start_addr()
                    - self.ntrans_len()
                    - 1 // pack size
                    - ntrans; // inputs
        let end = start + ntrans;
        memchr(b, &node.data[start..end]).map(|i| ntrans - i - 1)
    }

    fn output(&self, node: &Node, i: usize) -> Output {
        let osize = self.sizes(node).output_pack_size();
        if osize == 0 {
            return Output::zero();
        }
        let tsize = self.sizes(node).transition_pack_size();
        let ntrans = self.ntrans(node);
        let at = node.start_addr()
                 - self.ntrans_len()
                 - 1 // pack size
                 - ntrans // inputs
                 - (ntrans * tsize) // transition addresses
                 - (i * osize) // the previous outputs
                 - osize; // the desired output value
        Output::new(unpack_uint(&node.data[at..], osize as u8).unwrap())
    }

    fn final_output(&self, node: &Node) -> Output {
        let osize = self.sizes(node).output_pack_size();
        if !self.is_final_state() || osize == 0 {
            return Output::zero();
        }
        let tsize = self.sizes(node).transition_pack_size();
        let ntrans = self.ntrans(node);
        let at = node.start_addr()
                 - self.ntrans_len()
                 - 1 // pack size
                 - ntrans // inputs
                 - (ntrans * tsize) // transition addresses
                 - (ntrans * osize) // output values
                 - osize; // the desired output value
        Output::new(unpack_uint(&node.data[at..], osize as u8).unwrap())
    }

    fn trans_addr(&self, node: &Node, i: usize) -> CompiledAddr {
        assert!(i < self.ntrans(node));
        let tsize = self.sizes(node).transition_pack_size();
        let ntrans = self.ntrans(node);
        let at = node.start_addr()
                 - self.ntrans_len()
                 - 1 // pack size
                 - ntrans // inputs
                 - (i * tsize) // the previous transition addresses
                 - tsize; // the desired transition address
        unpack_delta(&node.data[at..], tsize, self.end_addr(node))
    }

    fn end_addr(&self, node: &Node) -> usize {
        let osize = self.sizes(node).output_pack_size();
        let final_osize = if !self.is_final_state() {
            0
        } else {
            osize
        };
        let tsize = self.sizes(node).transition_pack_size();
        let ntrans = self.ntrans(node);
        node.start_addr()
        - self.ntrans_len()
        - 1 // pack size
        - ntrans // inputs
        - (ntrans * tsize) // transition addresses
        - (ntrans * osize) // output values
        - final_osize // final output
    }
}

// high 4 bits is transition address packed size.
// low 4 bits is output value packed size.
//
// `0` is a legal value which means there are no transitions/outputs.
#[derive(Clone, Copy, Debug)]
struct PackSizes(u8);

impl PackSizes {
    #[inline]
    fn new() -> Self {
        PackSizes(0)
    }

    fn decode(v: u8) -> Self {
        PackSizes(v)
    }

    fn encode(&self) -> u8 {
        self.0
    }

    fn set_transition_pack_size(&mut self, size: u8) {
        assert!(size <= 8);
        self.0 = (self.0 & 0b0000_1111) | (size << 4);
    }

    fn transition_pack_size(&self) -> usize {
        ((self.0 & 0b1111_0000) >> 4) as usize
    }

    fn set_output_pack_size(&mut self, size: u8) {
        assert!(size <= 8);
        self.0 = (self.0 & 0b1111_0000) | size;
    }

    fn output_pack_size(&self) -> usize {
        (self.0 & 0b0000_1111) as usize
    }
}

/// An iterator over all transitions in a node.
///
/// `'f` is the lifetime of the underlying fst and `'n` is the lifetime of
/// the underlying `Node`.
pub struct Transitions<'f: 'n, 'n> {
    node: &'n Node<'f>,
    range: Range<u8>,
}

impl<'f, 'n> Iterator for Transitions<'f, 'n> {
    type Item = Transition;

    fn next(&mut self) -> Option<Transition> {
        self.range.next().map(|i| self.node.transition(i as usize))
    }
}

/// common_idx translate a byte to an index in the COMMON_INPUTS_INV array.
///
/// I wonder if it would be prudent to store this mapping in the FST itself.
/// The advantage of doing so would mean that common inputs would reflect the
/// specific data in the FST. The problem of course is that this table has to
/// be computed up front, which is pretty much at odds with the streaming
/// nature of the builder.
///
/// Nevertheless, the *caller* may have a priori knowledge that could be
/// supplied to the builder manually, which could then be embedded in the FST.
fn common_idx(input: u8, max: u8) -> u8 {
    let val = ((COMMON_INPUTS[input as usize] as u32 + 1) % 256) as u8;
    if val > max {
        0
    } else {
        val
    }
}

/// common_input translates a common input index stored in a serialized FST
/// to the corresponding byte.
fn common_input(idx: u8) -> Option<u8> {
    if idx == 0 {
        None
    } else {
        Some(COMMON_INPUTS_INV[(idx - 1) as usize])
    }
}

fn pack_delta<W: io::Write>(
    wtr: W,
    node_addr: CompiledAddr,
    trans_addr: CompiledAddr,
) -> io::Result<u8> {
    let nbytes = pack_delta_size(node_addr, trans_addr);
    try!(pack_delta_in(wtr, node_addr, trans_addr, nbytes));
    Ok(nbytes)
}

fn pack_delta_in<W: io::Write>(
    wtr: W,
    node_addr: CompiledAddr,
    trans_addr: CompiledAddr,
    nbytes: u8,
) -> io::Result<()> {
    let delta_addr = if trans_addr == EMPTY_ADDRESS {
        EMPTY_ADDRESS
    } else {
        node_addr - trans_addr
    };
    pack_uint_in(wtr, delta_addr as u64, nbytes)
}

fn pack_delta_size(node_addr: CompiledAddr, trans_addr: CompiledAddr) -> u8 {
    let delta_addr = if trans_addr == EMPTY_ADDRESS {
        EMPTY_ADDRESS
    } else {
        node_addr - trans_addr
    };
    pack_size(delta_addr as u64)
}

fn unpack_delta<R: io::Read>(
    rdr: R,
    trans_pack_size: usize,
    node_addr: usize,
) -> CompiledAddr {
    let delta = unpack_uint(rdr, trans_pack_size as u8).unwrap();
    let delta_addr = u64_to_usize(delta);
    if delta_addr == EMPTY_ADDRESS {
        EMPTY_ADDRESS
    } else {
        node_addr - delta_addr
    }
}

#[cfg(test)]
mod tests {
    use quickcheck::{TestResult, quickcheck};

    use raw::{Builder, Transition, CompiledAddr, Fst, Output};
    use raw::build::BuilderNode;
    use raw::node::{Node, node_new};
    use stream::Streamer;

    const NEVER_LAST: CompiledAddr = ::std::u64::MAX as CompiledAddr;

    #[test]
    fn prop_emits_inputs() {
        fn p(mut bs: Vec<Vec<u8>>) -> TestResult {
            bs.sort();
            bs.dedup();

            let mut bfst = Builder::memory();
            for word in &bs {
                bfst.add(word).unwrap();
            }
            let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
            let mut rdr = fst.stream();
            let mut words = vec![];
            while let Some(w) = rdr.next() {
                words.push(w.0.to_owned());
            }
            TestResult::from_bool(bs == words)
        }
        quickcheck(p as fn(Vec<Vec<u8>>) -> TestResult)
    }

    fn nodes_equal(compiled: &Node, uncompiled: &BuilderNode) -> bool {
        println!("{:?}", compiled);
        assert_eq!(compiled.is_final(), uncompiled.is_final);
        assert_eq!(compiled.len(), uncompiled.trans.len());
        assert_eq!(compiled.final_output(), uncompiled.final_output);
        for (ct, ut)
         in compiled.transitions().zip(uncompiled.trans.iter().cloned()) {
            assert_eq!(ct.inp, ut.inp);
            assert_eq!(ct.out, ut.out);
            assert_eq!(ct.addr, ut.addr);
        }
        true
    }

    fn compile(node: &BuilderNode) -> (CompiledAddr, Vec<u8>) {
        let mut buf = vec![0; 24];
        node.compile_to(&mut buf, NEVER_LAST, 24).unwrap();
        (buf.len() as CompiledAddr - 1, buf)
    }

    fn roundtrip(bnode: &BuilderNode) -> bool {
        let (addr, bytes) = compile(bnode);
        let node = node_new(addr, &bytes);
        nodes_equal(&node, &bnode)
    }

    fn trans(addr: CompiledAddr, inp: u8) -> Transition {
        Transition { inp: inp, out: Output::zero(), addr: addr }
    }

    #[test]
    fn bin_no_trans() {
        let bnode = BuilderNode {
            is_final: false,
            final_output: Output::zero(),
            trans: vec![],
        };
        let (addr, buf) = compile(&bnode);
        let node = node_new(addr, &buf);
        assert_eq!(node.as_slice().len(), 3);
        roundtrip(&bnode);
    }

    #[test]
    fn bin_one_trans_common() {
        let bnode = BuilderNode {
            is_final: false,
            final_output: Output::zero(),
            trans: vec![trans(20, b'a')],
        };
        let (addr, buf) = compile(&bnode);
        let node = node_new(addr, &buf);
        assert_eq!(node.as_slice().len(), 3);
        roundtrip(&bnode);
    }

    #[test]
    fn bin_one_trans_not_common() {
        let bnode = BuilderNode {
            is_final: false,
            final_output: Output::zero(),
            trans: vec![trans(2, b'\xff')],
        };
        let (addr, buf) = compile(&bnode);
        let node = node_new(addr, &buf);
        assert_eq!(node.as_slice().len(), 4);
        roundtrip(&bnode);
    }

    #[test]
    fn bin_many_trans() {
        let bnode = BuilderNode {
            is_final: false,
            final_output: Output::zero(),
            trans: vec![
                trans(2, b'a'), trans(3, b'b'),
                trans(4, b'c'), trans(5, b'd'),
                trans(6, b'e'), trans(7, b'f'),
            ],
        };
        let (addr, buf) = compile(&bnode);
        let node = node_new(addr, &buf);
        assert_eq!(node.as_slice().len(), 14);
        roundtrip(&bnode);
    }
}
