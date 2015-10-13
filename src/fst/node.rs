use std::cmp;
use std::fmt;
use std::io;
use std::ops::Range;

use byteorder::WriteBytesExt;
use memchr::memchr;

use fst::{CompiledAddr, Output, Transition};
use fst::build::BuilderNode;
use fst::common_inputs::{COMMON_INPUTS, COMMON_INPUTS_INV};
use fst::pack::{pack_size, pack_uint, pack_uint_in, unpack_uint};

#[derive(Clone, Copy)]
pub struct Node<'b> {
    data: &'b [u8],
    state: State,
}

impl<'b> fmt::Debug for Node<'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "NODE@{}", self.start_addr()));
        try!(writeln!(f, "  end_addr: {}", self.end_addr()));
        try!(writeln!(f, "  state: {:?}", self.state));
        try!(writeln!(f, "  is_final: {}", self.is_final()));
        try!(writeln!(f, "  final_output: {:?}", self.final_output()));
        try!(writeln!(f, "  transitions:"));
        for t in self.transitions() {
            try!(writeln!(f, "{:?}", t));
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
    Node {
        data: &data[..addr as usize + 1],
        state: State::from_u8(data[addr as usize]),
    }
}

impl<'b> Node<'b> {
    pub fn addr(&self) -> CompiledAddr {
        self.start_addr() as CompiledAddr
    }

    pub fn transitions<'n>(&'n self) -> Transitions<'b, 'n> {
        Transitions { node: self, range: 0..self.len() as u8 }
    }

    pub fn transition(&self, i: usize) -> Transition {
        Transition {
            inp: self.input(i),
            out: self.output(i),
            addr: self.transition_addr(i),
        }
    }

    pub fn input(&self, i: usize) -> u8 {
        self.state.input(self, i)
    }

    pub fn output(&self, i: usize) -> Output {
        self.state.output(self, i)
    }

    pub fn transition_addr(&self, i: usize) -> CompiledAddr {
        self.state.trans_addr(self, i)
    }

    pub fn final_output(&self) -> Output {
        self.state.final_output(self)
    }

    pub fn is_final(&self) -> bool {
        self.state.is_final()
    }

    pub fn find_input(&self, b: u8) -> Option<usize> {
        self.state.find_input(self, b)
    }

    pub fn len(&self) -> usize {
        self.state.ntrans(self)
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn start_addr(&self) -> usize {
        self.data.len() - 1
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
            OneTransNextOutput(_) => "OneTransNextOutput",
            OneTransNext(_) => "OneTransNext",
            OneTransFinal(_) => "OneTransFinal",
            OneTrans(_) => "OneTrans",
            AnyTrans(_) => "AnyTrans",
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
    OneTransNextOutput(StateOneTransNextOutput),
    OneTransNext(StateOneTransNext),
    OneTransFinal(StateOneTransFinal),
    OneTrans(StateOneTrans),
    AnyTrans(StateAnyTrans),
}

// one trans flag (1), next flag (1), output flag (1), output pack size
#[derive(Clone, Copy, Debug)]
struct StateOneTransNextOutput(u8);
// one trans flag (1), next flag (1), output flag (0), common input
#[derive(Clone, Copy, Debug)]
struct StateOneTransNext(u8);
// one trans flag (1), next flag (0), final flag (1), common input
#[derive(Clone, Copy, Debug)]
struct StateOneTransFinal(u8);
// one trans flag (1), next flag (0), final flag (0), common input
#[derive(Clone, Copy, Debug)]
struct StateOneTrans(u8);
// one trans flag (0), final flag, # transitions
#[derive(Clone, Copy, Debug)]
struct StateAnyTrans(u8);

impl State {
    #[inline]
    fn from_u8(v: u8) -> State {
        use self::State::*;
        match (v & 0b111_00000) >> 5 {
            0b111 => OneTransNextOutput(StateOneTransNextOutput(v)),
            0b110 => OneTransNext(StateOneTransNext(v)),
            0b101 => OneTransFinal(StateOneTransFinal(v)),
            0b100 => OneTrans(StateOneTrans(v)),
            _ => AnyTrans(StateAnyTrans(v)),
        }
    }

    fn compile<W: io::Write>(
        wtr: W, last_addr: CompiledAddr, addr: CompiledAddr,
        node: &BuilderNode,
    ) -> io::Result<()> {
        assert!(node.trans.len() <= ::std::u8::MAX as usize);
        if node.trans.len() != 1 {
            StateAnyTrans::compile(wtr, addr, node)
        } else {
            if !node.is_final && node.trans[0].addr == last_addr {
                if node.trans[0].out.is_zero() {
                    StateOneTransNext::compile(
                        wtr, addr, node.trans[0].inp)
                } else {
                    StateOneTransNextOutput::compile(
                        wtr, addr, node.trans[0].inp, node.trans[0].out)
                }
            } else {
                if node.is_final {
                    StateOneTransFinal::compile(
                        wtr, addr, node.final_output, node.trans[0])
                } else {
                    StateOneTrans::compile(wtr, addr, node.trans[0])
                }
            }
        }
    }

    fn is_final(&self) -> bool {
        use self::State::*;
        match *self {
            OneTransNextOutput(_) | OneTransNext(_) | OneTrans(_) => false,
            OneTransFinal(_) => true,
            AnyTrans(s) => s.is_final_state(),
        }
    }

    fn ntrans(&self, node: &Node) -> usize {
        use self::State::*;
        match *self {
            OneTransNextOutput(_)
            | OneTransNext(_)
            | OneTransFinal(_)
            | OneTrans(_) => 1,
            AnyTrans(s) => s.ntrans(node),
        }
    }

    fn input(&self, node: &Node, i: usize) -> u8 {
        use self::State::*;
        match *self {
            OneTransNextOutput(s) => { assert!(i == 0); s.input(node) }
            OneTransNext(s) => { assert!(i == 0); s.input(node) }
            OneTransFinal(s) => { assert!(i == 0); s.input(node) }
            OneTrans(s) => { assert!(i == 0); s.input(node) }
            AnyTrans(s) => s.input(node, i),
        }
    }

    fn find_input(&self, node: &Node, b: u8) -> Option<usize> {
        use self::State::*;
        match *self {
            OneTransNextOutput(s) if s.input(node) == b => Some(0),
            OneTransNextOutput(_) => None,
            OneTransNext(s) if s.input(node) == b => Some(0),
            OneTransNext(_) => None,
            OneTransFinal(s) if s.input(node) == b => Some(0),
            OneTransFinal(_) => None,
            OneTrans(s) if s.input(node) == b => Some(0),
            OneTrans(_) => None,
            AnyTrans(s) => s.find_input(node, b),
        }
    }

    fn output(&self, node: &Node, i: usize) -> Output {
        use self::State::*;
        match *self {
            OneTransNextOutput(s) => { assert!(i == 0); s.output(node) }
            OneTransNext(s) => { assert!(i == 0); Output::zero() }
            OneTransFinal(s) => { assert!(i == 0); s.output(node) }
            OneTrans(s) => { assert!(i == 0); s.output(node) }
            AnyTrans(s) => s.output(node, i),
        }
    }

    fn final_output(&self, node: &Node) -> Output {
        use self::State::*;
        match *self {
            OneTransNextOutput(_)
            | OneTransNext(_)
            | OneTrans(_) => Output::zero(),
            OneTransFinal(s) => s.final_output(node),
            AnyTrans(s) => s.final_output(node),
        }
    }

    fn trans_addr(&self, node: &Node, i: usize) -> CompiledAddr {
        use self::State::*;
        match *self {
            OneTransNextOutput(s) => { assert!(i == 0); s.trans_addr(node) }
            OneTransNext(s) => { assert!(i == 0); s.trans_addr(node) }
            OneTransFinal(s) => { assert!(i == 0); s.trans_addr(node) }
            OneTrans(s) => { assert!(i == 0); s.trans_addr(node) }
            AnyTrans(s) => s.trans_addr(node, i),
        }
    }

    fn end_addr(&self, node: &Node) -> usize {
        use self::State::*;
        match *self {
            OneTransNextOutput(s) => s.end_addr(node),
            OneTransNext(s) => s.end_addr(node),
            OneTransFinal(s) => s.end_addr(node),
            OneTrans(s) => s.end_addr(node),
            AnyTrans(s) => s.end_addr(node),
        }
    }
}

impl StateOneTransNextOutput {
    fn compile<W: io::Write>(
        mut wtr: W, addr: CompiledAddr, input: u8, output: Output,
    ) -> io::Result<()> {
        let mut state = StateOneTransNextOutput::new();
        let pack_size = try!(pack_uint(&mut wtr, output.encode()));
        try!(wtr.write_u8(input));
        state.set_output_pack_size(pack_size);
        wtr.write_u8(state.0).map_err(From::from)
    }

    #[inline]
    fn new() -> Self {
        StateOneTransNextOutput(0b111_00000)
    }

    #[inline]
    fn set_output_pack_size(&mut self, size: u8) {
        assert!(size >= 1 && size <= 8);
        self.0 = (self.0 & 0b111_00000) | size;
    }

    #[inline]
    fn output_pack_size(&self) -> u8 {
        self.0 & 0b000_11111
    }

    fn input(&self, node: &Node) -> u8 {
        node.data[node.start_addr() - 1]
    }

    fn output(&self, node: &Node) -> Output {
        let osize = self.output_pack_size() as usize;
        let i = node.start_addr()
                - 1 // input
                - osize;
        Output::decode(unpack_uint(&node.data[i..], osize as u8).unwrap())
    }

    fn trans_addr(&self, node: &Node) -> CompiledAddr {
        self.end_addr(node) as CompiledAddr - 1
    }

    fn end_addr(&self, node: &Node) -> usize {
        node.start_addr() - 1 - (self.output_pack_size() as usize)
    }
}

impl StateOneTransNext {
    fn compile<W: io::Write>(
        mut wtr: W, addr: CompiledAddr, input: u8,
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
        StateOneTransNext(0b110_00000)
    }

    #[inline]
    fn set_common_input(&mut self, input: u8) {
        self.0 = (self.0 & 0b110_00000) | common_idx(input, 0b11111);
    }

    #[inline]
    fn common_input(&self) -> Option<u8> {
        common_input(self.0 & 0b000_11111)
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

impl StateOneTransFinal {
    fn compile<W: io::Write>(
        mut wtr: W,
        addr: CompiledAddr,
        final_output: Output,
        trans: Transition,
    ) -> io::Result<()> {
        let final_output = final_output.encode();
        let trans_output = trans.out.encode();
        let osize = pack_size(cmp::max(final_output, trans_output));
        try!(pack_uint_in(&mut wtr, final_output, osize));
        try!(pack_uint_in(&mut wtr, trans_output, osize));

        let delta_addr = addr - trans.addr;
        let tsize = try!(pack_uint(&mut wtr, delta_addr));

        let mut pack_sizes = PackSizes::new();
        pack_sizes.set_output_pack_size(osize);
        pack_sizes.set_transition_pack_size(tsize);
        try!(wtr.write_u8(pack_sizes.encode()));

        let mut state = StateOneTransFinal::new();
        state.set_common_input(trans.inp);
        if state.common_input().is_none() {
            try!(wtr.write_u8(trans.inp));
        }
        wtr.write_u8(state.0).map_err(From::from)
    }

    #[inline]
    fn new() -> Self {
        StateOneTransFinal(0b101_00000)
    }

    #[inline]
    fn set_common_input(&mut self, input: u8) {
        self.0 = (self.0 & 0b101_00000) | common_idx(input, 0b11111);
    }

    #[inline]
    fn common_input(&self) -> Option<u8> {
        common_input(self.0 & 0b000_11111)
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
        let tsize = self.sizes(node).transition_pack_size();
        let osize = self.sizes(node).output_pack_size();
        let i = node.start_addr()
                - self.input_len()
                - 1 // pack size
                - tsize - osize;
        Output::decode(unpack_uint(&node.data[i..], osize as u8).unwrap())
    }

    fn final_output(&self, node: &Node) -> Output {
        let tsize = self.sizes(node).transition_pack_size();
        let osize = self.sizes(node).output_pack_size();
        let i = node.start_addr()
                - self.input_len()
                - 1 // pack size
                - tsize - osize - osize;
        Output::decode(unpack_uint(&node.data[i..], osize as u8).unwrap())
    }

    fn trans_addr(&self, node: &Node) -> CompiledAddr {
        let tsize = self.sizes(node).transition_pack_size();
        let osize = self.sizes(node).output_pack_size();
        let i = node.start_addr()
                - self.input_len()
                - 1 // pack size
                - tsize;
        let delta = unpack_uint(&node.data[i..], tsize as u8).unwrap();
        self.end_addr(node) as CompiledAddr - delta
    }

    fn end_addr(&self, node: &Node) -> usize {
        let tsize = self.sizes(node).transition_pack_size();
        let osize = self.sizes(node).output_pack_size();
        node.start_addr()
        - self.input_len()
        - 1 // pack size
        - tsize - osize - osize
    }
}

impl StateOneTrans {
    fn compile<W: io::Write>(
        mut wtr: W, addr: CompiledAddr, trans: Transition,
    ) -> io::Result<()> {
        let out = trans.out.encode();
        let output_pack_size = try!(pack_uint(&mut wtr, out));

        let delta_addr = addr - trans.addr;
        let trans_pack_size = try!(pack_uint(&mut wtr, delta_addr));

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
        StateOneTrans(0b100_00000)
    }

    #[inline]
    fn set_common_input(&mut self, input: u8) {
        self.0 = (self.0 & 0b100_00000) | common_idx(input, 0b11111);
    }

    #[inline]
    fn common_input(&self) -> Option<u8> {
        common_input(self.0 & 0b000_11111)
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
        let tsize = self.sizes(node).transition_pack_size();
        let osize = self.sizes(node).output_pack_size();
        let i = node.start_addr()
                - self.input_len()
                - 1 // pack size
                - tsize - osize;
        Output::decode(unpack_uint(&node.data[i..], osize as u8).unwrap())
    }

    fn trans_addr(&self, node: &Node) -> CompiledAddr {
        let tsize = self.sizes(node).transition_pack_size();
        let osize = self.sizes(node).output_pack_size();
        let i = node.start_addr()
                - self.input_len()
                - 1 // pack size
                - tsize;
        let delta = unpack_uint(&node.data[i..], tsize as u8).unwrap();
        self.end_addr(node) as CompiledAddr - delta
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
        let mut osize = pack_size(node.final_output.encode());
        let mut any_outs = !node.final_output.is_zero();
        for t in &node.trans {
            tsize = cmp::max(tsize, pack_size(addr - t.addr));
            osize = cmp::max(osize, pack_size(t.out.encode()));
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
            try!(pack_uint_in(&mut wtr, node.final_output.encode(), osize));
            for t in node.trans.iter().rev() {
                try!(pack_uint_in(&mut wtr, t.out.encode(), osize));
            }
        }
        for t in node.trans.iter().rev() {
            try!(pack_uint_in(&mut wtr, addr - t.addr, tsize));
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
    fn set_state_ntrans(&mut self, mut n: u8) {
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
        Output::decode(unpack_uint(&node.data[at..], osize as u8).unwrap())
    }

    fn final_output(&self, node: &Node) -> Output {
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
                 - (ntrans * osize) // output values
                 - osize; // the desired output value
        Output::decode(unpack_uint(&node.data[at..], osize as u8).unwrap())
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
        let delta = unpack_uint(&node.data[at..], tsize as u8).unwrap();
        self.end_addr(node) as CompiledAddr - delta
    }

    fn end_addr(&self, node: &Node) -> usize {
        let osize = self.sizes(node).output_pack_size();
        let tsize = self.sizes(node).transition_pack_size();
        let ntrans = self.ntrans(node);
        node.start_addr()
        - self.ntrans_len()
        - 1 // pack size
        - ntrans // inputs
        - (ntrans * tsize) // transition addresses
        - (ntrans * osize) // output values
        - osize // final output
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

pub struct Transitions<'b: 'n, 'n> {
    node: &'n Node<'b>,
    range: Range<u8>,
}

impl<'b, 'n> Iterator for Transitions<'b, 'n> {
    type Item = Transition;

    fn next(&mut self) -> Option<Transition> {
        self.range.next().map(|i| self.node.transition(i as usize))
    }
}

fn common_idx(input: u8, max: u8) -> u8 {
    let val = ((COMMON_INPUTS[input as usize] as u32 + 1) % 256) as u8;
    if val > max {
        0
    } else {
        val
    }
}

fn common_input(idx: u8) -> Option<u8> {
    if idx == 0 {
        None
    } else {
        Some(COMMON_INPUTS_INV[(idx - 1) as usize])
    }
}

#[cfg(test)]
mod tests {
    use quickcheck::{TestResult, quickcheck};

    use fst::{Builder, Transition, CompiledAddr, Fst, Output};
    use fst::build::BuilderNode;
    use fst::node::{Node, node_new};

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
            let mut rdr = fst.reader();
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
        assert_eq!(node.as_slice().len(), 4);
        roundtrip(&bnode);
    }

    #[test]
    fn bin_one_trans_not_common() {
        let bnode = BuilderNode {
            is_final: false,
            final_output: Output::zero(),
            trans: vec![trans(2, b'~')],
        };
        let (addr, buf) = compile(&bnode);
        let node = node_new(addr, &buf);
        assert_eq!(node.as_slice().len(), 5);
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
