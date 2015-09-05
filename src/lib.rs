#![allow(dead_code)]

extern crate byteorder;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

use std::cmp;
use std::io;
use std::ops::Range;

use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};

use pack::Packer;

mod build;
mod ioutil;
mod pack;
#[cfg(test)] mod tests;

pub use self::build::Builder;

const VERSION: u64 = 1;
const FINAL_EMPTY_STATE: CompiledAddr = 0;
const NONE_STATE: CompiledAddr = 1;

const COMMON_INPUTS: [u8; 256] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 0
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 16
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 32
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 48
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 64
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 80
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,      // 96
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 0, 0, 0, 0, 0, // 112
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 128
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 144
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 160
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 176
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 192
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 208
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 224
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            // 240
];

const COMMON_INPUTS_INV: [u8; 26] = [
    b'a', b'b', b'c', b'd', b'e',
    b'f', b'g', b'h', b'i', b'j',
    b'k', b'l', b'm', b'n', b'o',
    b'p', b'q', b'r', b's', b't',
    b'u', b'v', b'w', b'x', b'y',
    b'z',
];

pub struct Fst<B> {
    data: B,
    root_addr: CompiledAddr,
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
        let data = self.data.as_ref();
        if data[data.len() - 8 - 8 - 1] == 0 {
            return None;
        }
        let mut out_bytes = &data[data.len() - 8 - 8..];
        Some(Output::decode(out_bytes.read_u64::<LittleEndian>().unwrap()))
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

type CompiledAddr = u64;

#[derive(Clone, Debug)]
struct Node<'b> {
    data: &'b [u8],
    state: NodeState,
}

impl<'b> Node<'b> {
    fn new(addr: CompiledAddr, data: &'b [u8]) -> Node<'b> {
        Node {
            data: &data[..addr as usize + 1],
            state: NodeState::from_u8(data[addr as usize]),
        }
    }

    pub fn addr(&self) -> CompiledAddr {
        self.start_addr() as CompiledAddr
    }

    pub fn transitions<'n>(&'n self) -> Transitions<'b, 'n> {
        Transitions { node: self, range: 0..self.len() as u8 }
    }

    pub fn transition(&self, i: usize) -> (u8, Output, CompiledAddr) {
        assert!(i < self.len());
        (self.input(i), self.output(i), self.transition_addr(i))
    }

    pub fn input(&self, i: usize) -> u8 {
        self.state.common_input()
                  .unwrap_or_else(|| self.data[self.pos_inputs() - i])
    }

    pub fn output(&self, i: usize) -> Output {
        let opsize = self.output_pack_size();
        if opsize == 0 || self.state.is_next() {
            return Output::none();
        }
        let at = self.pos_outputs() - (i * opsize);
        let packer = Packer::for_num_bytes(opsize as u8);
        let out_encoded = packer.read(&self.data[at - opsize + 1..]).unwrap();
        Output::decode(out_encoded)
    }

    pub fn transition_addr(&self, i: usize) -> CompiledAddr {
        let delta = if self.state.is_next() {
            assert!(i == 0);
            1
        } else {
            let tpsize = self.transition_pack_size();
            let at = self.pos_transitions() - (i * tpsize);
            let packer = Packer::for_num_bytes(tpsize as u8);
            packer.read(&self.data[at - tpsize + 1..]).unwrap()
        };
        self.end_addr() as u64 - delta
    }

    pub fn is_final(&self) -> bool {
        self.is_empty_final() || self.state.is_final_state()
    }

    fn is_empty_final(&self) -> bool {
        self.start_addr() as CompiledAddr == FINAL_EMPTY_STATE
    }

    pub fn len(&self) -> usize {
        if self.is_empty_final() {
            0
        } else {
            self.state
                .ntrans()
                .unwrap_or_else(|| self.data[self.pos_ntrans()]) as usize
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn transition_pack_size(&self) -> usize {
        if self.is_empty() {
            0
        } else {
            self.state
                .transition_pack_size()
                .unwrap_or_else(|| {
                    let sizes = self.data[self.pos_pack_size()];
                    sizes & 0b00_000_111
                }) as usize
        }
    }

    fn output_pack_size(&self) -> usize {
        if self.is_empty() {
            0
        } else {
            self.state
                .transition_pack_size()
                .map(|_| 0)
                .unwrap_or_else(|| {
                    let sizes = self.data[self.pos_pack_size()];
                    sizes >> 3
                }) as usize
        }
    }

    fn start_addr(&self) -> usize {
        self.data.len() - 1
    }

    fn end_addr(&self) -> usize {
        if self.is_empty_final() || self.state.is_next() {
            return self.start_addr();
        } else if self.is_empty() {
            return self.start_addr() - 1;
        }
        self.start_addr()
        - self.state.ntrans().map(|_| 0).unwrap_or(1)
        - self.state.transition_pack_size().map(|_| 0).unwrap_or(1)
        - self.state.common_input().map(|_| 0).unwrap_or_else(|| self.len())
        - (self.len() * self.transition_pack_size())
        - (self.len() * self.output_pack_size())
    }

    fn pos_ntrans(&self) -> usize {
        self.start_addr() - 1
    }

    fn pos_pack_size(&self) -> usize {
        self.start_addr() - self.state.ntrans().map(|_| 0).unwrap_or(1) - 1
    }

    fn pos_inputs(&self) -> usize {
        self.start_addr()
        - self.state.ntrans().map(|_| 0).unwrap_or(1)
        - self.state.transition_pack_size().map(|_| 0).unwrap_or(1)
        - 1
    }

    fn pos_transitions(&self) -> usize {
        self.start_addr()
        - self.state.ntrans().map(|_| 0).unwrap_or(1)
        - self.state.transition_pack_size().map(|_| 0).unwrap_or(1)
        - self.state.common_input().map(|_| 0).unwrap_or_else(|| self.len())
        - 1
    }

    fn pos_outputs(&self) -> usize {
        self.start_addr()
        - self.state.ntrans().map(|_| 0).unwrap_or(1)
        - self.state.transition_pack_size().map(|_| 0).unwrap_or(1)
        - self.state.common_input().map(|_| 0).unwrap_or_else(|| self.len())
        - (self.len() * self.transition_pack_size())
        - 1
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct BuilderNode {
    is_final: bool,
    trans: Vec<BuilderTransition>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
struct BuilderTransition {
    addr: CompiledAddr,
    inp: u8,
    out: Output,
}

impl BuilderNode {
    fn compile_to<W>(
        &self,
        mut wtr: W,
        last_addr: CompiledAddr,
        addr: CompiledAddr,
    ) -> io::Result<()>
    where W: io::Write {
        assert!(self.trans.len() <= ::std::u8::MAX as usize);
        let mut state = NodeState::new();
        state.set_final_state(self.is_final);

        if self.trans.is_empty() {
            try!(wtr.write_u8(0)); // # transitions
            try!(wtr.write_u8(state.into_u8()));
            return Ok(());
        }

        let (mut min_addr, mut max_addr) = (::std::u64::MAX, 0);
        let (mut min_out, mut max_out) = (::std::u64::MAX, 0);
        for t in &self.trans {
            min_addr = cmp::min(min_addr, addr - t.addr);
            max_addr = cmp::max(max_addr, addr - t.addr);
            if !t.out.is_none() {
                min_out = cmp::min(min_out, t.out.encode());
                max_out = cmp::max(max_out, t.out.encode());
            }
        }
        let trans_packer = Packer::for_range(min_addr, max_addr);
        let out_packer = Packer::for_range(min_out, max_out);
        if out_packer.bytes_needed() == 0 {
            assert!(trans_packer.bytes_needed() > 0);
            state.set_transition_pack_size(trans_packer.bytes_needed());
        }
        state.set_ntrans(self.trans.len() as u8);
        state.set_common_input(self.trans[0].inp);
        if out_packer.bytes_needed() == 0
           && state.common_input().is_some()
           && self.trans[0].addr == last_addr {
            state.set_next(true);
            try!(wtr.write_u8(state.into_u8()));
            return Ok(());
        }

        if out_packer.bytes_needed() > 0 {
            for t in self.trans.iter().rev() {
                try!(out_packer.write(&mut wtr, t.out.encode()));
            }
        }
        for t in self.trans.iter().rev() {
            try!(trans_packer.write(&mut wtr, addr - t.addr));
        }
        if state.common_input().is_none() {
            for t in self.trans.iter().rev() {
                try!(wtr.write_u8(t.inp));
            }
        }
        if state.transition_pack_size().is_none() {
            let pack_sizes = (out_packer.bytes_needed() << 3)
                             | trans_packer.bytes_needed();
            try!(wtr.write_u8(pack_sizes));
        }
        if state.ntrans().is_none() {
            try!(wtr.write_u8(self.trans.len() as u8));
        }
        try!(wtr.write_u8(state.into_u8()));
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
struct NodeState(u8);

impl NodeState {
    #[inline]
    fn new() -> NodeState {
        NodeState(0)
    }

    #[inline]
    fn from_u8(v: u8) -> NodeState {
        NodeState(v)
    }

    #[inline]
    fn into_u8(self) -> u8 {
        self.0
    }

    #[inline]
    fn set_final_state(&mut self, yes: bool) {
        if yes {
            self.0 |= 0b1_00_00000;
        }
    }

    #[inline]
    fn is_final_state(&self) -> bool {
        self.0 & 0b1_00_00000 == 0b1_00_00000
    }

    #[inline]
    fn set_common_input(&mut self, input: u8) {
        if self.ntrans() == Some(1) {
            let val = COMMON_INPUTS[input as usize];
            if val == 0 {
                return;
            }
            assert!(val >= 1 && val <= 26);
            self.0 = (self.0 & 0b1_11_00000) | val;
        }
    }

    #[inline]
    fn common_input(&self) -> Option<u8> {
        let val = self.0 & 0b0_00_11111;
        if val == 0 || val > 26 {
            None
        } else {
            Some(COMMON_INPUTS_INV[val as usize - 1])
        }
    }

    #[inline]
    fn set_transition_pack_size(&mut self, size: u8) {
        if size <= 2 {
            self.0 |= size << 5;
        }
    }

    #[inline]
    fn transition_pack_size(&self) -> Option<u8> {
        let val = (self.0 & 0b0_11_00000) >> 5;
        if val >= 1 && val <= 2 {
            Some(val)
        } else {
            None
        }
    }

    #[inline]
    fn set_next(&mut self, yes: bool) {
        if yes {
            self.0 |= 0b0_11_00000;
        }
    }

    #[inline]
    fn is_next(&self) -> bool {
        self.0 & 0b0_11_00000 == 0b0_11_00000
    }

    #[inline]
    fn set_ntrans(&mut self, n: u8) {
        if n >= 1 && n <= 5 && self.common_input().is_none() {
            self.0 |= 26 + n;
        }
    }

    #[inline]
    fn ntrans(&self) -> Option<u8> {
        match self.0 & 0b0_00_11111 {
            1...26 => Some(1),
            val @ 27...31 => Some(val - 26),
            _ => None,
        }
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

    fn is_none(self) -> bool {
        match self.0 {
            None => true,
            _ => false,
        }
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

struct Transitions<'b: 'n, 'n> {
    node: &'n Node<'b>,
    range: Range<u8>,
}

impl<'b, 'n> Iterator for Transitions<'b, 'n> {
    type Item = (u8, Output, CompiledAddr);

    fn next(&mut self) -> Option<(u8, Output, CompiledAddr)> {
        self.range.next().map(|i| self.node.transition(i as usize))
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
            if self.fst.node(addr).is_final() {
                return Some((&self.inp, out));
            }
        }
        None
    }
}
