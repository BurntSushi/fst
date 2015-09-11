use std::error::Error;
use std::io::{self, Write};
use std::ptr;

use byteorder::{WriteBytesExt, LittleEndian};

use ioutil::CountingWriter;
use {
    VERSION, NONE_STATE,
    BuilderNode, BuilderTransition, CompiledAddr, Output,
};

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
    /// After this point, it is considered immutable and will never change.
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

#[derive(Debug)]
struct LastTransition {
    inp: u8,
    out: Output,
}

impl Builder<io::Cursor<Vec<u8>>> {
    pub fn memory() -> Builder<Vec<u8>> {
        Builder::from_bytes(Vec::with_capacity(1024))
    }

    pub fn from_bytes(buf: Vec<u8>) -> Builder<Vec<u8>> {
        Builder::new(buf).unwrap()
    }
}

impl<W: io::Write> Builder<W> {
    pub fn new(wtr: W) -> io::Result<Builder<W>> {
        let mut wtr = CountingWriter::new(wtr);
        // Don't allow any nodes to have address 0-7. We use these as
        // special markers. e.g., `0` means "final state with no transitions."
        // We also use the first 8 bytes to encode the API version.
        try!(wtr.write_u64::<LittleEndian>(VERSION));
        let b = Builder {
            wtr: wtr,
            unfinished: UnfinishedNodes::new(),
            registry: Registry::new(50_000, 5),
            last: None,
            last_addr: NONE_STATE,
        };
        Ok(b)
    }

    pub fn into_inner(mut self) -> io::Result<W> {
        try!(self.compile_from(0));
        let mut root_node = self.unfinished.pop_root();
        let root_addr = try!(self.compile(&root_node));
        try!(self.wtr.write_u64::<LittleEndian>(root_addr));
        try!(self.wtr.flush());
        Ok(self.wtr.into_inner())
    }

    pub fn add<B>(&mut self, bs: B) -> io::Result<()>
            where B: AsRef<[u8]> {
        self.insert_output(bs, Output::none())
    }

    pub fn insert<B>(&mut self, bs: B, val: u64) -> io::Result<()>
            where B: AsRef<[u8]> {
        self.insert_output(bs, Output::new(val))
    }

    fn insert_output<B>(&mut self, bs: B, out: Output) -> io::Result<()>
            where B: AsRef<[u8]> {
        let bs = bs.as_ref();
        try!(self.check_last_key(bs));
        if bs.is_empty() {
            self.unfinished.set_root_output(out);
            return Ok(());
        }
        let (prefix_len, out) =
            self.unfinished.find_common_prefix_and_set_output(bs, out);
        assert!(prefix_len != bs.len());
        try!(self.compile_from(prefix_len));
        self.unfinished.add_suffix(&bs[prefix_len..], out);
        Ok(())
    }

    fn compile_from(&mut self, istate: usize) -> io::Result<()> {
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

    fn compile(&mut self, node: &BuilderNode) -> io::Result<CompiledAddr> {
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
                cell.set_compiled_address(self.last_addr);
                self.last_addr
            }
            RegistryEntry::Found(addr) => addr,
        })
    }

    fn check_last_key(&mut self, bs: &[u8]) -> io::Result<()> {
        if let Some(ref mut last) = self.last {
            if bs == &**last {
                return Err(ioerr(
                    format!("Cannot insert duplicate key: {:?}", bs)));
            }
            if bs < &**last {
                return Err(ioerr(
                    format!("'{:?}' is < '{:?}', entries must be added in \
                             lexicographic order.", bs, last)));
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
                final_output: Output::none(),
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
                    final_output: Output::none(),
                    trans: vec![],
                },
                last: Some(LastTransition { inp: b, out: Output::none() }),
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
            self.node.trans.push(BuilderTransition {
                addr: addr,
                inp: trans.inp,
                out: trans.out,
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

fn ioerr<E: Into<Box<Error + Send + Sync>>>(e: E) -> io::Error {
    io::Error::new(io::ErrorKind::Other, e)
}

struct Registry {
    table: Vec<RegistryCell>,
    table_size: usize, // number of rows
    lru_size: usize, // number of columns
}

struct RegistryLru<'a> {
    cells: &'a mut [RegistryCell],
}

#[derive(Clone, Copy, Debug)]
struct RegistryCell {
    addr: CompiledAddr,
    node: RegistryNode,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct RegistryNode {
    is_final: bool,
    final_output: Output,
    trans: [CompiledAddr; 2],
    inputs: [u8; 2],
    outputs: [Output; 2],
}

enum RegistryEntry<'a> {
    Found(CompiledAddr),
    NotFound(&'a mut RegistryCell),
    Rejected,
}

impl Registry {
    fn new(table_size: usize, lru_size: usize) -> Registry {
        let empty_cell = RegistryCell::none();
        let ncells = table_size.checked_mul(lru_size).unwrap();
        Registry {
            table: vec![empty_cell; ncells],
            table_size: table_size,
            lru_size: lru_size,
        }
    }

    fn entry<'a>(&'a mut self, bnode: &BuilderNode) -> RegistryEntry<'a> {
        if self.table.is_empty() {
            return RegistryEntry::Rejected;
        }
        let node = match RegistryNode::from_builder_node(bnode) {
            None => return RegistryEntry::Rejected,
            Some(node) => node,
        };
        let bucket = self.hash(&node);
        let start = self.lru_size * bucket;
        let end = start + self.lru_size;
        RegistryLru { cells: &mut self.table[start..end] }.entry(node)
    }

    fn hash(&self, node: &RegistryNode) -> usize {
        // The overhead of std's SipHasher is just so not worth it.
        // (In my unscientific tests, this dumb hash function has no
        // observable impact on compression ratio, but results in a perf
        // boost of ~38%.)
        let mut n: usize = 0;
        n = n.wrapping_add(2 * node.is_final as usize);
        n = n.wrapping_add(7 * node.trans[0] as usize);
        n = n.wrapping_add(11 * node.trans[1] as usize);
        n = n.wrapping_add(3 * node.inputs[0] as usize);
        n = n.wrapping_add(5 * node.inputs[1] as usize);
        n = n.wrapping_add(13 * node.outputs[0].encode() as usize);
        n = n.wrapping_add(17 * node.outputs[1].encode() as usize);
        n = n.wrapping_add(19 * node.final_output.encode() as usize);
        n % self.table_size
    }
}

impl<'a> RegistryLru<'a> {
    fn entry(mut self, node: RegistryNode) -> RegistryEntry<'a> {
        if let Some(i) = self.cells.iter().position(|c| c.node == node) {
            let addr = self.cells[i].addr;
            self.promote(i);
            RegistryEntry::Found(addr)
        } else {
            let last = self.cells.len() - 1;
            self.cells[last].node = node;
            self.promote(last);
            RegistryEntry::NotFound(&mut self.cells[0])
        }
    }

    fn promote(&mut self, i: usize) {
        assert!(i < self.cells.len());
        let cell = self.cells[i];
        let p = self.cells.as_mut_ptr();
        unsafe { ptr::copy(p, p.offset(1), i); }
        self.cells[0] = cell;
    }
}

impl RegistryCell {
    fn none() -> RegistryCell {
        RegistryCell {
            addr: NONE_STATE,
            node: RegistryNode::none(),
        }
    }

    fn is_none(&self) -> bool {
        self.addr == NONE_STATE
    }

    fn set_compiled_address(&mut self, addr: CompiledAddr) {
        self.addr = addr;
    }
}

impl RegistryNode {
    fn from_builder_node(node: &BuilderNode) -> Option<RegistryNode> {
        match node.trans.len() {
            1 => Some(RegistryNode {
                is_final: node.is_final,
                final_output: node.final_output,
                trans: [node.trans[0].addr, 0],
                inputs: [node.trans[0].inp, 0],
                outputs: [node.trans[0].out, Output::none()],
            }),
            2 => Some(RegistryNode {
                is_final: node.is_final,
                final_output: node.final_output,
                trans: [node.trans[0].addr, node.trans[1].addr],
                inputs: [node.trans[0].inp, node.trans[1].inp],
                outputs: [node.trans[0].out, node.trans[1].out],
            }),
            _ => None,
        }
    }

    fn none() -> RegistryNode {
        RegistryNode {
            is_final: false,
            final_output: Output::none(),
            trans: [NONE_STATE, NONE_STATE],
            inputs: [0, 0],
            outputs: [Output::none(), Output::none()],
        }
    }

    fn is_none(&self) -> bool {
        self.trans == [NONE_STATE, NONE_STATE]
    }
}
