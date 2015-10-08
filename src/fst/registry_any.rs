use std::fmt;
use std::hash::{Hash, Hasher, SipHasher};
use std::ptr;

use fst::{NONE_STATE, CompiledAddr, Output, Transition};
use fst::build::BuilderNode;

#[derive(Debug)]
pub struct Registry {
    table: Vec<RegistryCell>,
    table_size: usize, // number of rows
    lru_size: usize, // number of columns
}

#[derive(Debug)]
struct RegistryLru<'a> {
    cells: &'a mut [RegistryCell],
}

#[derive(Clone, Copy, Debug)]
pub struct RegistryCell {
    addr: CompiledAddr,
    node: RegistryNode,
}

#[derive(Copy)]
struct RegistryNode {
    is_final: bool,
    final_output: Output,
    trans: [Transition; 256],
}

#[derive(Debug)]
pub enum RegistryEntry<'a> {
    Found(CompiledAddr),
    NotFound(&'a mut RegistryCell),
    Rejected,
}

impl Registry {
    pub fn new(table_size: usize, lru_size: usize) -> Registry {
        let empty_cell = RegistryCell::none();
        let ncells = table_size.checked_mul(lru_size).unwrap();
        Registry {
            table: vec![empty_cell; ncells],
            table_size: table_size,
            lru_size: lru_size,
        }
    }

    pub fn entry<'a>(&'a mut self, bnode: &BuilderNode) -> RegistryEntry<'a> {
        if self.table.is_empty() {
            return RegistryEntry::Rejected;
        }
        let bucket = self.hash(bnode);
        let node = RegistryNode::from_builder_node(bnode);
        let start = self.lru_size * bucket;
        let end = start + self.lru_size;
        RegistryLru { cells: &mut self.table[start..end] }.entry(node)
    }

    fn hash(&self, node: &BuilderNode) -> usize {
        // The overhead of std's SipHasher is just so not worth it.
        // (In my unscientific tests, this dumb hash function has no
        // observable impact on compression ratio, but results in a perf
        // boost of ~38%.)
        // let mut n: usize = 0;
        // n = n.wrapping_add(2 * node.is_final as usize);
        // n = n.wrapping_add(7 * node.trans[0] as usize);
        // n = n.wrapping_add(11 * node.trans[1] as usize);
        // n = n.wrapping_add(3 * node.inputs[0] as usize);
        // n = n.wrapping_add(5 * node.inputs[1] as usize);
        // n = n.wrapping_add(13 * node.outputs[0].encode() as usize);
        // n = n.wrapping_add(17 * node.outputs[1].encode() as usize);
        // n = n.wrapping_add(19 * node.final_output.encode() as usize);
        // n % self.table_size
        let mut hasher = SipHasher::new();
        node.hash(&mut hasher);
        hasher.finish() as usize % self.table_size
    }
}

impl<'a> RegistryLru<'a> {
    fn entry(mut self, node: RegistryNode) -> RegistryEntry<'a> {
        if let Some(i) = self.cells.iter().position(|c| c.node == node) {
            // The lack of promotion here is odd given the name "LRU."
            // Initially, a cell was promoted on access, but this turned out
            // to not work so well in practice. Namely, it made it too easy for
            // very frequently occurring nodes to drop out of the cache.
            //
            // We do however continue to promote below, when we are trying
            // to compile a node that isn't in the cache. This means that
            // for a frequently occurring node to drop out of the cache, there
            // must have been K=lru_size cache misses. Any number of cache hits
            // can occur.
            //
            // If we promoted on access, then K=lru_size cache hits that
            // didn't involve the most frequently occurring node could push
            // that node to the bottom of this row of cells. The next cache
            // miss will then evict that node.
            //
            // My experience has been that this has a profound effect when
            // there are many keys that share the same prefix (i.e., URLs).
            // Namely, promoting on access makes it to easy for nodes
            // associated with the prefix (e.g., http://) to be evicted.
            RegistryEntry::Found(self.cells[i].addr)
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
        let t = Transition {
            inp: 0,
            out: Output::zero(),
            addr: NONE_STATE,
        };
        RegistryCell {
            addr: NONE_STATE,
            node: RegistryNode {
                is_final: false,
                final_output: Output::zero(),
                trans: [t; 256],
            },
        }
    }

    pub fn insert(&mut self, addr: CompiledAddr) {
        self.addr = addr;
    }
}

impl RegistryNode {
    fn from_builder_node(bnode: &BuilderNode) -> RegistryNode {
        let t = Transition {
            inp: 0,
            out: Output::zero(),
            addr: NONE_STATE,
        };
        let mut node = RegistryNode {
            is_final: bnode.is_final,
            final_output: bnode.final_output,
            trans: [t; 256],
        };
        for (i, &t) in bnode.trans.iter().enumerate() {
            node.trans[i] = t;
        }
        node
    }
}

impl fmt::Debug for RegistryNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RegistryNode {{ is_final: {:?}, \
                   final_output: {:?}, trans: ?? }}",
               self.is_final, self.final_output)
    }
}

impl Clone for RegistryNode {
    fn clone(&self) -> RegistryNode {
        RegistryNode {
            is_final: self.is_final,
            final_output: self.final_output,
            trans: self.trans,
        }
    }
}

impl PartialEq for RegistryNode {
    fn eq(&self, other: &RegistryNode) -> bool {
        self.is_final == other.is_final
        && self.final_output == other.final_output
        && self.trans[..] == other.trans[..]
    }
}
