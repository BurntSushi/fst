use std::cmp;
use std::collections::BinaryHeap;
use std::io;

use fst::build::Builder;
use fst::{Fst, FstReader, Output};
use Result;

pub fn union_ignore_outputs<'f, W, I>(
    bfst: &mut Builder<W>,
    fsts: I,
) -> Result<()>
where W: io::Write,
      I: IntoIterator<Item=&'f Fst> {
    let mut union = Union::new(fsts);
    while let Some(slot) = union.pop() {
        try!(bfst.add(slot.input()));
        union.refill(slot);
    }
    Ok(())
}

pub fn union_with_outputs<'f, W, F, I>(
    bfst: &mut Builder<W>,
    fsts: I,
    mut merge: F,
) -> Result<()>
where W: io::Write,
      F: FnMut(&[u8], &[u64]) -> u64,
      I: IntoIterator<Item=&'f Fst> {
    let mut union = Union::new(fsts);
    let mut outs = vec![];
    while let Some(slot) = union.pop() {
        unsafe { outs.set_len(0); }
        outs.push(slot.output().value());
        while union.peek_is_duplicate(slot.input()) {
            let slot2 = union.pop().unwrap();
            outs.push(slot2.output().value());
            union.refill(slot2);
        }
        try!(bfst.insert(slot.input(), merge(slot.input(), &outs)));
        union.refill(slot);
    }
    Ok(())
}

struct Union<'f> {
    rdrs: Vec<FstReader<'f>>,
    heap: BinaryHeap<Slot>,
}

impl<'f> Union<'f> {
    fn new<I: IntoIterator<Item=&'f Fst>>(fsts: I) -> Union<'f> {
        let mut u = Union {
            rdrs: fsts.into_iter().map(Fst::reader).collect(),
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

    fn input(&self) -> &[u8] {
        &self.input
    }

    fn output(&self) -> Output {
        self.output
    }

    fn set_input(&mut self, input: &[u8]) {
        let addcap = input.len().checked_sub(self.input.len()).unwrap_or(0);
        self.input.reserve(addcap);
        unsafe { self.input.set_len(input.len()); }
        for (dst, &byte) in self.input.iter_mut().zip(input.iter()) {
            *dst = byte;
        }
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
    use fst::build::Builder;
    use fst::tests::{fst_map, fst_set, fst_inputstrs_outputs, fst_input_strs};
    use fst::Fst;
    use super::{union_ignore_outputs, union_with_outputs};

    fn s(string: &str) -> String { string.to_owned() }

    #[test]
    fn basic_set() {
        let set1 = fst_set(&["a", "b", "c"]);
        let set2 = fst_set(&["x", "y", "z"]);

        let mut bfst = Builder::memory();
        union_ignore_outputs(&mut bfst, &[set1, set2]).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_input_strs(&fst), vec!["a", "b", "c", "x", "y", "z"]);
    }

    #[test]
    fn basic_set_dupes() {
        let set1 = fst_set(&["aa", "b", "cc"]);
        let set2 = fst_set(&["b", "cc", "z"]);

        let mut bfst = Builder::memory();
        union_ignore_outputs(&mut bfst, &[set1, set2]).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_input_strs(&fst), vec!["aa", "b", "cc", "z"]);
    }

    #[test]
    fn basic_map() {
        let map1 = fst_map(vec![("a", 1), ("b", 2), ("c", 3)]);
        let map2 = fst_map(vec![("x", 1), ("y", 2), ("z", 3)]);

        let mut bfst = Builder::memory();
        union_with_outputs(
            &mut bfst,
            &[map1, map2],
            |_, os| os.iter().fold(0, |a, &b| a + b),
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
    fn basic_map_dupes() {
        let map1 = fst_map(vec![("aa", 1), ("b", 2), ("cc", 3)]);
        let map2 = fst_map(vec![("b", 1), ("cc", 2), ("z", 3)]);
        let map3 = fst_map(vec![("b", 1)]);

        let mut bfst = Builder::memory();
        union_with_outputs(
            &mut bfst,
            &[map1, map2, map3],
            |_, os| os.iter().fold(0, |a, &b| a + b),
        ).unwrap();
        let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(
            fst_inputstrs_outputs(&fst),
            vec![
                (s("aa"), 1), (s("b"), 4), (s("cc"), 5), (s("z"), 3),
            ]);
    }
}
