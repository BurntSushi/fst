use std::io::{self, Write};

use byteorder::{WriteBytesExt, LittleEndian};

use error::{Error, Result};
use fst::{VERSION, NONE_STATE, CompiledAddr, Output, Transition};
use fst::counting_writer::CountingWriter;
use fst::registry::{Registry, RegistryEntry};
// use fst::registry_any::{Registry, RegistryEntry};
// use fst::registry_minimal::{Registry, RegistryEntry};

pub struct Builder<W> {
    /// The FST raw data is written directly to `wtr`.
    ///
    /// No internal buffering is done.
    wtr: CountingWriter<W>,
    /// The stack of unfinished nodes.
    ///
    /// An unfinished node is a node that could potentially have a new
    /// transition added to it when a new word is added to the dictionary.
    unfinished: UnfinishedNodes,
    /// A map of finished nodes.
    ///
    /// A finished node is one that has been compiled and written to `wtr`.
    /// After this point, the node is considered immutable and will never
    /// Achange.
    registry: Registry,
    /// The last word added.
    ///
    /// This is used to enforce the invariant that words are added in sorted
    /// order.
    last: Option<Vec<u8>>,
    /// The address of the last compiled node.
    ///
    /// This is used to optimize states with one transition that point
    /// to the previously compiled node. (The previously compiled node in
    /// this case actually corresponds to the next state for the transition,
    /// since states are compiled in reverse.)
    last_addr: CompiledAddr,
}

#[derive(Debug)]
struct UnfinishedNodes {
    stack: Vec<BuilderNodeUnfinished>,
}

#[derive(Debug)]
struct BuilderNodeUnfinished {
    node: BuilderNode,
    last: Option<LastTransition>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct BuilderNode {
    pub is_final: bool,
    pub final_output: Output,
    pub trans: Vec<Transition>,
}

#[derive(Debug)]
struct LastTransition {
    inp: u8,
    out: Output,
}

impl Builder<io::Cursor<Vec<u8>>> {
    pub fn memory() -> Builder<Vec<u8>> {
        Builder::from_bytes(Vec::with_capacity(10 * (1 << 10)))
    }

    pub fn from_bytes(buf: Vec<u8>) -> Builder<Vec<u8>> {
        Builder::new(buf).unwrap()
    }
}

impl<W: io::Write> Builder<W> {
    pub fn new(wtr: W) -> Result<Builder<W>> {
        let mut wtr = CountingWriter::new(wtr);
        // Don't allow any nodes to have address 0-7. We use these to encode
        // the API version.
        try!(wtr.write_u64::<LittleEndian>(VERSION));
        Ok(Builder {
            wtr: wtr,
            unfinished: UnfinishedNodes::new(),
            registry: Registry::new(10_000, 5),
            last: None,
            last_addr: NONE_STATE,
        })
    }

    pub fn finish(self) -> Result<()> {
        try!(self.into_inner());
        Ok(())
    }

    pub fn into_inner(mut self) -> Result<W> {
        try!(self.compile_from(0));
        let mut root_node = self.unfinished.pop_root();
        let root_addr = try!(self.compile(&root_node));
        try!(self.wtr.write_u64::<LittleEndian>(root_addr));
        try!(self.wtr.flush());
        Ok(self.wtr.into_inner())
    }

    pub fn add<B>(&mut self, bs: B) -> Result<()>
            where B: AsRef<[u8]> {
        try!(self.check_last_key(bs.as_ref(), false));
        self.insert_output(bs, Output::zero())
    }

    pub fn insert<B>(&mut self, bs: B, val: u64) -> Result<()>
            where B: AsRef<[u8]> {
        try!(self.check_last_key(bs.as_ref(), true));
        self.insert_output(bs, Output::new(val))
    }

    fn insert_output<B>(&mut self, bs: B, out: Output) -> Result<()>
            where B: AsRef<[u8]> {
        let bs = bs.as_ref();
        if bs.is_empty() {
            self.unfinished.set_root_output(out);
            return Ok(());
        }
        let (prefix_len, out) =
            self.unfinished.find_common_prefix_and_set_output(bs, out);
        if prefix_len == bs.len() {
            // If the prefix found consumes the entire set of bytes, then
            // the prefix *equals* the bytes given. This means it is a
            // duplicate value with no output. So we can give up here.
            //
            // If the below assert fails, then that means we let a duplicate
            // value through even when inserting outputs.
            assert!(out.is_zero());
            return Ok(());
        }
        try!(self.compile_from(prefix_len));
        self.unfinished.add_suffix(&bs[prefix_len..], out);
        Ok(())
    }

    fn compile_from(&mut self, istate: usize) -> Result<()> {
        let mut addr = NONE_STATE;
        while istate + 1 < self.unfinished.len() {
            let node =
                if addr == NONE_STATE {
                    self.unfinished.pop_empty()
                } else {
                    self.unfinished.pop_freeze(addr)
                };
            addr = try!(self.compile(&node));
            assert!(addr != NONE_STATE);
        }
        self.unfinished.top_last_freeze(addr);
        Ok(())
    }

    fn compile(&mut self, node: &BuilderNode) -> Result<CompiledAddr> {
        Ok(match self.registry.entry(&node) {
            RegistryEntry::Rejected => {
                let start_addr = self.wtr.count() as CompiledAddr;
                try!(node.compile_to(
                    &mut self.wtr, self.last_addr, start_addr));
                self.last_addr = self.wtr.count() as CompiledAddr - 1;
                self.last_addr
            }
            RegistryEntry::NotFound(mut cell) => {
                let start_addr = self.wtr.count() as CompiledAddr;
                try!(node.compile_to(
                    &mut self.wtr, self.last_addr, start_addr));
                self.last_addr = self.wtr.count() as CompiledAddr - 1;
                cell.insert(self.last_addr);
                self.last_addr
            }
            RegistryEntry::Found(addr) => addr,
        })
    }

    fn check_last_key(&mut self, bs: &[u8], check_dupe: bool) -> Result<()> {
        if let Some(ref mut last) = self.last {
            if check_dupe && bs == &**last {
                return Err(Error::DuplicateKey { got: bs.to_vec() });
            }
            if bs < &**last {
                return Err(Error::OutOfOrder {
                    previous: last.to_vec(),
                    got: bs.to_vec(),
                });
            }
            last.clear();
            for &b in bs {
                last.push(b);
            }
        } else {
            self.last = Some(bs.to_vec());
        }
        Ok(())
    }
}

impl UnfinishedNodes {
    fn new() -> UnfinishedNodes {
        let mut unfinished = UnfinishedNodes { stack: Vec::with_capacity(64) };
        unfinished.push_empty(false);
        unfinished
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn push_empty(&mut self, is_final: bool) {
        self.stack.push(BuilderNodeUnfinished {
            node: BuilderNode {
                is_final: is_final,
                final_output: Output::zero(),
                trans: vec![],
            },
            last: None,
        });
    }

    fn pop_root(&mut self) -> BuilderNode {
        assert!(self.stack.len() == 1);
        assert!(self.stack[0].last.is_none());
        self.stack.pop().unwrap().node
    }

    fn pop_freeze(&mut self, addr: CompiledAddr) -> BuilderNode {
        let mut unfinished = self.stack.pop().unwrap();
        unfinished.last_compiled(addr);
        unfinished.node
    }

    fn pop_empty(&mut self) -> BuilderNode {
        let mut unfinished = self.stack.pop().unwrap();
        assert!(unfinished.last.is_none());
        unfinished.node
    }

    fn set_root_output(&mut self, out: Output) {
        self.stack[0].node.is_final = true;
        self.stack[0].node.final_output = out;
    }

    fn top_last_freeze(&mut self, addr: CompiledAddr) {
        let last = self.stack.len().checked_sub(1).unwrap();
        self.stack[last].last_compiled(addr);
    }

    fn add_suffix(&mut self, bs: &[u8], out: Output) {
        if bs.is_empty() {
            return;
        }
        let last = self.stack.len().checked_sub(1).unwrap();
        assert!(self.stack[last].last.is_none());
        self.stack[last].last = Some(LastTransition { inp: bs[0], out: out });
        for &b in &bs[1..] {
            self.stack.push(BuilderNodeUnfinished {
                node: BuilderNode {
                    is_final: false,
                    final_output: Output::zero(),
                    trans: vec![],
                },
                last: Some(LastTransition { inp: b, out: Output::zero() }),
            });
        }
        self.push_empty(true);
    }

    fn find_common_prefix_and_set_output(
        &mut self,
        mut bs: &[u8],
        mut out: Output,
    ) -> (usize, Output) {
        let mut i = 0;
        while !bs.is_empty() {
            let add_prefix = match self.stack[i].last.as_mut() {
                Some(ref mut t) if t.inp == bs[0] => {
                    bs = &bs[1..];
                    i += 1;
                    let common_pre = t.out.prefix(out);
                    let add_prefix = t.out.sub(common_pre);
                    out = out.sub(common_pre);
                    t.out = common_pre;
                    add_prefix
                }
                _ => break,
            };
            self.stack[i].add_output_prefix(add_prefix);
        }
        (i, out)
    }
}

impl BuilderNodeUnfinished {
    fn last_compiled(&mut self, addr: CompiledAddr) {
        if let Some(trans) = self.last.take() {
            self.node.trans.push(Transition {
                inp: trans.inp,
                out: trans.out,
                addr: addr,
            });
        }
    }

    fn add_output_prefix(&mut self, prefix: Output) {
        if self.node.is_final {
            self.node.final_output = prefix.cat(self.node.final_output);
        }
        for t in &mut self.node.trans {
            t.out = prefix.cat(t.out);
        }
        if let Some(ref mut t) = self.last {
            t.out = prefix.cat(t.out);
        }
    }
}
