use std::cmp;
use std::collections::BinaryHeap;
use std::io;

use fst::build::Builder;
use fst::{Fst, FstReader, Output};
use Result;

pub fn union_ignore_outputs<B, W>(
    bfst: &mut Builder<W>,
    fsts: &[Fst<B>],
) -> Result<()>
where B: AsRef<[u8]>, W: io::Write {
    let mut union = Union::new(fsts);
    while let Some(slot) = union.pop() {
        try!(bfst.add(slot.input()));
        union.refill(slot);
    }
    Ok(())
}

struct Union<'a, B: 'a> {
    rdrs: Vec<FstReader<'a, B>>,
    heap: BinaryHeap<Slot>,
}

impl<'a, B: AsRef<[u8]>> Union<'a, B> {
    fn new(fsts: &'a [Fst<B>]) -> Union<'a, B> {
        let mut u = Union {
            rdrs: fsts.iter().map(Fst::reader).collect(),
            heap: BinaryHeap::with_capacity(fsts.len()),
        };
        for i in 0..u.rdrs.len() {
            u.refill(Slot::new(i));
        }
        u
    }

    fn pop(&mut self) -> Option<Slot> {
        self.heap.pop()
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
            output: Output::none(),
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
    use fst::tests::{fst_set, fst_inputs};
    use fst::Fst;
    use super::union_ignore_outputs;

    #[test]
    fn basic() {
        let set1 = fst_set(&[b"a", b"b", b"c"]);
        let set2 = fst_set(&[b"x", b"y", b"z"]);

        let mut bfst = Builder::memory();
        union_ignore_outputs(&mut bfst, &[set1, set2]).unwrap();
        let fst = Fst::new(bfst.into_inner().unwrap()).unwrap();
        assert_eq!(fst_inputs(&fst), vec![b"a", b"b", b"c", b"x", b"y", b"z"]);
    }
}
