use std::ptr;

use fst::{NONE_STATE, CompiledAddr, Output};
use fst::build::BuilderNode;

#[derive(Debug)]
pub struct Registry {
    table: Vec<RegistryCell>,
    table_size: usize, // number of rows
    mru_size: usize, // number of columns
}

#[derive(Debug)]
struct RegistryMru<'a> {
    cells: &'a mut [RegistryCell],
}

#[derive(Clone, Copy, Debug)]
pub struct RegistryCell {
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

#[derive(Debug)]
pub enum RegistryEntry<'a> {
    Found(CompiledAddr),
    NotFound(&'a mut RegistryCell),
    Rejected,
}

impl Registry {
    pub fn new(table_size: usize, mru_size: usize) -> Registry {
        let empty_cell = RegistryCell::none();
        let ncells = table_size.checked_mul(mru_size).unwrap();
        Registry {
            table: vec![empty_cell; ncells],
            table_size: table_size,
            mru_size: mru_size,
        }
    }

    pub fn entry<'a>(&'a mut self, bnode: &BuilderNode) -> RegistryEntry<'a> {
        if self.table.is_empty() {
            return RegistryEntry::Rejected;
        }
        let node = match RegistryNode::from_builder_node(bnode) {
            None => return RegistryEntry::Rejected,
            Some(node) => node,
        };
        let bucket = self.hash(&node);
        let start = self.mru_size * bucket;
        let end = start + self.mru_size;
        RegistryMru { cells: &mut self.table[start..end] }.entry(node)
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

impl<'a> RegistryMru<'a> {
    fn entry(mut self, node: RegistryNode) -> RegistryEntry<'a> {
        if let Some(i) = self.cells.iter().position(|c| c.node == node) {
            self.promote(i); // most recently used
            RegistryEntry::Found(self.cells[i].addr)
        } else {
            self.cells[0].node = node; // discard MRU
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

    pub fn insert(&mut self, addr: CompiledAddr) {
        self.addr = addr;
    }
}

impl RegistryNode {
    fn from_builder_node(node: &BuilderNode) -> Option<RegistryNode> {
        match node.trans.len() {
            0 if node.is_final => Some(RegistryNode {
                is_final: true,
                final_output: node.final_output,
                trans: [NONE_STATE, NONE_STATE],
                inputs: [0, 0],
                outputs: [Output::zero(), Output::zero()],
            }),
            1 => Some(RegistryNode {
                is_final: node.is_final,
                final_output: node.final_output,
                trans: [node.trans[0].addr, 0],
                inputs: [node.trans[0].inp, 0],
                outputs: [node.trans[0].out, Output::zero()],
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
            final_output: Output::zero(),
            trans: [NONE_STATE, NONE_STATE],
            inputs: [0, 0],
            outputs: [Output::zero(), Output::zero()],
        }
    }

    fn is_none(&self) -> bool {
        !self.is_final && self.trans == [NONE_STATE, NONE_STATE]
    }
}

#[cfg(test)]
mod tests {
    use fst::{Output, Transition};
    use fst::build::BuilderNode;
    use super::{
        Registry, RegistryCell, RegistryEntry, RegistryMru, RegistryNode,
    };

    fn assert_rejected(entry: RegistryEntry) {
        match entry {
            RegistryEntry::Rejected => {}
            entry => panic!("expected rejected entry, got: {:?}", entry),
        }
    }

    fn assert_not_found(entry: RegistryEntry) {
        match entry {
            RegistryEntry::NotFound(_) => {}
            entry => panic!("expected nout found entry, got: {:?}", entry),
        }
    }

    fn assert_insert_and_found(reg: &mut Registry, bnode: &BuilderNode) {
        match reg.entry(&bnode) {
            RegistryEntry::NotFound(cell) => cell.insert(1234),
            entry => panic!("unexpected not found entry, got: {:?}", entry),
        }
        match reg.entry(&bnode) {
            RegistryEntry::Found(addr) => assert_eq!(addr, 1234),
            entry => panic!("unexpected found entry, got: {:?}", entry),
        }
    }

    #[test]
    fn empty_is_ok() {
        let mut reg = Registry::new(0, 0);
        let bnode = BuilderNode {
            is_final: false,
            final_output: Output::zero(),
            trans: vec![],
        };
        assert_rejected(reg.entry(&bnode));
    }

    #[test]
    fn one_final_is_ok() {
        let mut reg = Registry::new(1, 1);
        let bnode = BuilderNode {
            is_final: true,
            final_output: Output::zero(),
            trans: vec![],
        };
        assert_insert_and_found(&mut reg, &bnode);
        assert_rejected(
            reg.entry(&BuilderNode { is_final: false, .. bnode.clone() }));
    }

    #[test]
    fn one_with_trans_is_ok() {
        let mut reg = Registry::new(1, 1);
        let bnode = BuilderNode {
            is_final: false,
            final_output: Output::zero(),
            trans: vec![Transition {
                addr: 0, inp: b'a', out: Output::zero(),
            }],
        };
        assert_insert_and_found(&mut reg, &bnode);
        assert_not_found(
            reg.entry(&BuilderNode { is_final: true, .. bnode.clone() }));
        assert_not_found(reg.entry(&BuilderNode {
            trans: vec![Transition {
                addr: 0, inp: b'b', out: Output::zero(),
            }],
            .. bnode.clone()
        }));
        assert_not_found(reg.entry(&BuilderNode {
            trans: vec![Transition {
                addr: 0, inp: b'a', out: Output::new(1),
            }],
            .. bnode.clone()
        }));
    }

    #[test]
    fn mru_works() {
        let mut reg = Registry::new(1, 1);

        let bnode1 = BuilderNode {
            is_final: true,
            final_output: Output::zero(),
            trans: vec![],
        };
        assert_insert_and_found(&mut reg, &bnode1);

        let bnode2 = BuilderNode {
            is_final: true,
            final_output: Output::new(1),
            trans: vec![],
        };
        assert_insert_and_found(&mut reg, &bnode2);
        assert_not_found(reg.entry(&bnode1));
    }

    #[test]
    fn promote() {
        let rn = RegistryNode {
            is_final: false,
            final_output: Output::zero(),
            trans: [0, 0],
            inputs: [0, 0],
            outputs: [Output::zero(), Output::zero()],
        };
        let mut bnodes = vec![
            RegistryCell { addr: 1, node: rn },
            RegistryCell { addr: 2, node: rn },
            RegistryCell { addr: 3, node: rn },
            RegistryCell { addr: 4, node: rn },
        ];
        let mut mru = RegistryMru { cells: &mut bnodes };

        mru.promote(0);
        assert_eq!(mru.cells[0].addr, 1);
        assert_eq!(mru.cells[1].addr, 2);
        assert_eq!(mru.cells[2].addr, 3);
        assert_eq!(mru.cells[3].addr, 4);

        mru.promote(1);
        assert_eq!(mru.cells[0].addr, 2);
        assert_eq!(mru.cells[1].addr, 1);
        assert_eq!(mru.cells[2].addr, 3);
        assert_eq!(mru.cells[3].addr, 4);

        mru.promote(3);
        assert_eq!(mru.cells[0].addr, 4);
        assert_eq!(mru.cells[1].addr, 2);
        assert_eq!(mru.cells[2].addr, 1);
        assert_eq!(mru.cells[3].addr, 3);

        mru.promote(2);
        assert_eq!(mru.cells[0].addr, 1);
        assert_eq!(mru.cells[1].addr, 4);
        assert_eq!(mru.cells[2].addr, 2);
        assert_eq!(mru.cells[3].addr, 3);
    }
}
