use std::cmp;
use std::collections::BinaryHeap;
use std::io;

use raw::build::Builder;
use raw::{Fst, FstStream, Output};
use Result;

#[derive(Copy, Clone, Debug)]
pub struct FstOutput {
    pub index: usize,
    pub output: u64,
}

pub fn union<'f, W, I>(
    bfst: &mut Builder<W>,
    fsts: I,
) -> Result<()>
where W: io::Write,
      I: IntoIterator<Item=&'f Fst> {
    let mut heap = SlotHeap::new(fsts);
    while let Some(slot) = heap.pop() {
        try!(bfst.add(slot.input()));
        heap.refill(slot);
    }
    Ok(())
}

pub fn union_with<'f, W, I, F>(
    bfst: &mut Builder<W>,
    fsts: I,
    mut merge: F,
) -> Result<()>
where W: io::Write,
      I: IntoIterator<Item=&'f Fst>,
      F: FnMut(&[u8], &[FstOutput]) -> u64 {
    let mut heap = SlotHeap::new(fsts);
    let mut outs = vec![];
    while let Some(slot) = heap.pop() {
        outs.clear();
        outs.push(slot.fst_output());
        while let Some(slot2) = heap.pop_if_equal(slot.input()) {
            outs.push(slot2.fst_output());
            heap.refill(slot2);
        }
        try!(bfst.insert(slot.input(), merge(slot.input(), &outs)));
        heap.refill(slot);
    }
    Ok(())
}

pub fn intersect<'f, W, I>(
    bfst: &mut Builder<W>,
    fsts: I,
) -> Result<()>
where W: io::Write,
      I: IntoIterator<Item=&'f Fst> {
    let mut heap = SlotHeap::new(fsts);
    while let Some(slot) = heap.pop() {
        let mut popped: usize = 1;
        while let Some(slot2) = heap.pop_if_equal(slot.input()) {
            heap.refill(slot2);
            popped += 1;
        }
        // This term is only in the intersection if we found it in
        // every FST given.
        if popped == heap.num_slots() {
            try!(bfst.add(slot.input()));
        }
        heap.refill(slot);
    }
    Ok(())
}

pub fn intersect_with<'f, W, I, F>(
    bfst: &mut Builder<W>,
    fsts: I,
    mut merge: F,
) -> Result<()>
where W: io::Write,
      I: IntoIterator<Item=&'f Fst>,
      F: FnMut(&[u8], &[FstOutput]) -> u64 {
    let mut heap = SlotHeap::new(fsts);
    let mut outs = vec![];
    while let Some(slot) = heap.pop() {
        outs.clear();
        outs.push(slot.fst_output());
        let mut popped: usize = 1;
        while let Some(slot2) = heap.pop_if_equal(slot.input()) {
            outs.push(slot2.fst_output());
            heap.refill(slot2);
            popped += 1;
        }
        // This term is only in the intersection if we found it in
        // every FST given.
        if popped == heap.num_slots() {
            try!(bfst.insert(slot.input(), merge(slot.input(), &outs)));
        }
        heap.refill(slot);
    }
    Ok(())
}

struct SlotHeap<'f> {
    rdrs: Vec<FstStream<'f>>,
    heap: BinaryHeap<Slot>,
}

impl<'f> SlotHeap<'f> {
    fn new<I: IntoIterator<Item=&'f Fst>>(fsts: I) -> SlotHeap<'f> {
        let mut u = SlotHeap {
            rdrs: fsts.into_iter().map(Fst::stream).collect(),
            heap: BinaryHeap::new(),
        };
        for i in 0..u.rdrs.len() {
            u.refill(Slot::new(i));
        }
        u
    }

    fn pop(&mut self) -> Option<Slot> {
        self.heap.pop()
    }

    fn peek_is_duplicate(&self, key: &[u8]) -> bool {
        self.heap.peek().map(|s| s.input() == key).unwrap_or(false)
    }

    fn pop_if_equal(&mut self, key: &[u8]) -> Option<Slot> {
        if self.peek_is_duplicate(key) {
            self.pop()
        } else {
            None
        }
    }

    fn num_slots(&self) -> usize {
        self.rdrs.len()
    }

    fn refill(&mut self, mut slot: Slot) {
        if let Some((input, output)) = self.rdrs[slot.idx].next() {
            slot.set_input(input);
            slot.set_output(output);
            self.heap.push(slot);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Slot {
    idx: usize,
    input: Vec<u8>,
    output: Output,
}

impl Slot {
    fn new(rdr_idx: usize) -> Slot {
        Slot {
            idx: rdr_idx,
            input: Vec::with_capacity(64),
            output: Output::zero(),
        }
    }

    fn fst_output(&self) -> FstOutput {
        FstOutput { index: self.idx, output: self.output.value() }
    }

    fn input(&self) -> &[u8] {
        &self.input
    }

    fn output(&self) -> Output {
        self.output
    }

    fn set_input(&mut self, input: &[u8]) {
        let addcap = input.len().checked_sub(self.input.len()).unwrap_or(0);
        self.input.clear();
        self.input.extend(input);
    }

    fn set_output(&mut self, output: Output) {
        self.output = output;
    }
}

impl PartialOrd for Slot {
    fn partial_cmp(&self, other: &Slot) -> Option<cmp::Ordering> {
        (&self.input, self.output)
        .partial_cmp(&(&other.input, other.output))
        .map(|ord| ord.reverse())
    }
}

impl Ord for Slot {
    fn cmp(&self, other: &Slot) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use raw::build::Builder;
    use raw::tests::{fst_map, fst_set, fst_inputstrs_outputs, fst_input_strs};
    use raw::Fst;
    use super::{intersect, intersect_with, union, union_with};

    fn s(string: &str) -> String { string.to_owned() }

    #[test]
    fn union_set() {
        let set1 = fst_set(&["a", "b", "c"]);
        let set2 = fst_set(&["x", "y", "z"]);

        let mut bfst = Builder::memory();
        union(&mut bfst, &[set1, set2]).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_input_strs(&fst), vec!["a", "b", "c", "x", "y", "z"]);
    }

    #[test]
    fn union_set_dupes() {
        let set1 = fst_set(&["aa", "b", "cc"]);
        let set2 = fst_set(&["b", "cc", "z"]);

        let mut bfst = Builder::memory();
        union(&mut bfst, &[set1, set2]).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_input_strs(&fst), vec!["aa", "b", "cc", "z"]);
    }

    #[test]
    fn union_map() {
        let map1 = fst_map(vec![("a", 1), ("b", 2), ("c", 3)]);
        let map2 = fst_map(vec![("x", 1), ("y", 2), ("z", 3)]);

        let mut bfst = Builder::memory();
        union_with(
            &mut bfst,
            &[map1, map2],
            |_, os| os.iter().fold(0, |a, b| a + b.output),
        ).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(
            fst_inputstrs_outputs(&fst),
            vec![
                (s("a"), 1), (s("b"), 2), (s("c"), 3),
                (s("x"), 1), (s("y"), 2), (s("z"), 3),
            ]);
    }

    #[test]
    fn union_map_dupes() {
        let map1 = fst_map(vec![("aa", 1), ("b", 2), ("cc", 3)]);
        let map2 = fst_map(vec![("b", 1), ("cc", 2), ("z", 3)]);
        let map3 = fst_map(vec![("b", 1)]);

        let mut bfst = Builder::memory();
        union_with(
            &mut bfst,
            &[map1, map2, map3],
            |_, os| os.iter().fold(0, |a, b| a + b.output),
        ).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(
            fst_inputstrs_outputs(&fst),
            vec![
                (s("aa"), 1), (s("b"), 4), (s("cc"), 5), (s("z"), 3),
            ]);
    }

    #[test]
    fn intersect_set() {
        let set1 = fst_set(&["a", "b", "c"]);
        let set2 = fst_set(&["x", "y", "z"]);

        let mut bfst = Builder::memory();
        intersect(&mut bfst, &[set1, set2]).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_input_strs(&fst), Vec::<&str>::new());
    }

    #[test]
    fn intersect_set_dupes() {
        let set1 = fst_set(&["aa", "b", "cc"]);
        let set2 = fst_set(&["b", "cc", "z"]);

        let mut bfst = Builder::memory();
        intersect(&mut bfst, &[set1, set2]).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_input_strs(&fst), vec!["b", "cc"]);
    }

    #[test]
    fn intersect_map() {
        let map1 = fst_map(vec![("a", 1), ("b", 2), ("c", 3)]);
        let map2 = fst_map(vec![("x", 1), ("y", 2), ("z", 3)]);

        let mut bfst = Builder::memory();
        intersect_with(
            &mut bfst,
            &[map1, map2],
            |_, os| os.iter().fold(0, |a, b| a + b.output),
        ).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_inputstrs_outputs(&fst), Vec::<(String, u64)>::new());
    }

    #[test]
    fn intersect_map_dupes() {
        let map1 = fst_map(vec![("aa", 1), ("b", 2), ("cc", 3)]);
        let map2 = fst_map(vec![("b", 1), ("cc", 2), ("z", 3)]);
        let map3 = fst_map(vec![("b", 1)]);

        let mut bfst = Builder::memory();
        intersect_with(
            &mut bfst,
            &[map1, map2, map3],
            |_, os| os.iter().fold(0, |a, b| a + b.output),
        ).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_inputstrs_outputs(&fst), vec![(s("b"), 4)]);
    }
}
