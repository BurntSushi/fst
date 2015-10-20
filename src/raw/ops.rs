use std::cmp;
use std::collections::BinaryHeap;
use std::iter::FromIterator;

use raw::Output;
use stream::{IntoStream, Stream};

// Permits stream operations to be hetergenous with respect to streams.
type BoxedStream<'f> = Box<for<'a> Stream<'a, Item=(&'a [u8], Output)> + 'f>;

#[derive(Copy, Clone, Debug)]
pub struct FstOutput {
    pub index: usize,
    pub output: u64,
}

pub struct StreamOp<'f> {
    streams: Vec<BoxedStream<'f>>,
}

impl<'f> StreamOp<'f> {
    pub fn new() -> Self {
        StreamOp { streams: vec![] }
    }

    pub fn add<I, S>(mut self, stream: I) -> Self
            where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], Output)>,
                  S: 'f + for<'a> Stream<'a, Item=(&'a [u8], Output)> {
        self.push(stream);
        self
    }

    pub fn push<I, S>(&mut self, stream: I)
            where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], Output)>,
                  S: 'f + for<'a> Stream<'a, Item=(&'a [u8], Output)> {
        self.streams.push(Box::new(stream.into_stream()));
    }

    pub fn union(self) -> StreamUnion<'f> {
        StreamUnion {
            heap: StreamHeap::new(self.streams),
            outs: vec![],
            cur_slot: None,
        }
    }

    pub fn intersection(self) -> StreamIntersection<'f> {
        StreamIntersection {
            heap: StreamHeap::new(self.streams),
            outs: vec![],
            cur_slot: None,
        }
    }

    pub fn difference(mut self) -> StreamDifference<'f> {
        let first = self.streams.swap_remove(0);
        StreamDifference {
            set: first,
            key: vec![],
            heap: StreamHeap::new(self.streams),
            outs: vec![],
        }
    }

    pub fn symmetric_difference(self) -> StreamSymmetricDifference<'f> {
        StreamSymmetricDifference {
            heap: StreamHeap::new(self.streams),
            outs: vec![],
            cur_slot: None,
        }
    }
}

impl<'f, I, S> Extend<I> for StreamOp<'f>
    where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], Output)>,
          S: 'f + for<'a> Stream<'a, Item=(&'a [u8], Output)> {
    fn extend<T>(&mut self, it: T) where T: IntoIterator<Item=I> {
        for stream in it {
            self.push(stream);
        }
    }
}

impl<'f, I, S> FromIterator<I> for StreamOp<'f>
    where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], Output)>,
          S: 'f + for<'a> Stream<'a, Item=(&'a [u8], Output)> {
    fn from_iter<T>(it: T) -> Self where T: IntoIterator<Item=I> {
        let mut op = StreamOp::new();
        op.extend(it);
        op
    }
}

pub struct StreamUnion<'f> {
    heap: StreamHeap<'f>,
    outs: Vec<FstOutput>,
    cur_slot: Option<Slot>,
}

impl<'a, 'f> Stream<'a> for StreamUnion<'f> {
    type Item = (&'a [u8], &'a [FstOutput]);

    fn next(&'a mut self) -> Option<Self::Item> {
        if let Some(slot) = self.cur_slot.take() {
            self.heap.refill(slot);
        }
        let slot = match self.heap.pop() {
            None => return None,
            Some(slot) => {
                self.cur_slot = Some(slot);
                self.cur_slot.as_ref().unwrap()
            }
        };
        self.outs.clear();
        self.outs.push(slot.fst_output());
        while let Some(slot2) = self.heap.pop_if_equal(slot.input()) {
            self.outs.push(slot2.fst_output());
            self.heap.refill(slot2);
        }
        Some((slot.input(), &self.outs))
    }
}

pub struct StreamIntersection<'f> {
    heap: StreamHeap<'f>,
    outs: Vec<FstOutput>,
    cur_slot: Option<Slot>,
}

impl<'a, 'f> Stream<'a> for StreamIntersection<'f> {
    type Item = (&'a [u8], &'a [FstOutput]);

    fn next(&'a mut self) -> Option<Self::Item> {
        if let Some(slot) = self.cur_slot.take() {
            self.heap.refill(slot);
        }
        loop {
            let slot = match self.heap.pop() {
                None => return None,
                Some(slot) => slot,
            };
            self.outs.clear();
            self.outs.push(slot.fst_output());
            let mut popped: usize = 1;
            while let Some(slot2) = self.heap.pop_if_equal(slot.input()) {
                self.outs.push(slot2.fst_output());
                self.heap.refill(slot2);
                popped += 1;
            }
            if popped < self.heap.num_slots() {
                self.heap.refill(slot);
            } else {
                self.cur_slot = Some(slot);
                let key = self.cur_slot.as_ref().unwrap().input();
                return Some((key, &self.outs))
            }
        }
    }
}

pub struct StreamDifference<'f> {
    set: BoxedStream<'f>,
    key: Vec<u8>,
    heap: StreamHeap<'f>,
    outs: Vec<FstOutput>,
}

impl<'a, 'f> Stream<'a> for StreamDifference<'f> {
    type Item = (&'a [u8], &'a [FstOutput]);

    fn next(&'a mut self) -> Option<Self::Item> {
        loop {
            match self.set.next() {
                None => return None,
                Some((key, out)) => {
                    self.key.clear();
                    self.key.extend(key);
                    self.outs.clear();
                    self.outs.push(FstOutput {
                        index: 0,
                        output: out.value(),
                    });
                }
            };
            let mut popped: usize = 0;
            while let Some(slot) = self.heap.pop_if_equal(&self.key) {
                self.heap.refill(slot);
                popped += 1;
            }
            if popped == 0 {
                return Some((&self.key, &self.outs))
            }
        }
    }
}

pub struct StreamSymmetricDifference<'f> {
    heap: StreamHeap<'f>,
    outs: Vec<FstOutput>,
    cur_slot: Option<Slot>,
}

impl<'a, 'f> Stream<'a> for StreamSymmetricDifference<'f> {
    type Item = (&'a [u8], &'a [FstOutput]);

    fn next(&'a mut self) -> Option<Self::Item> {
        if let Some(slot) = self.cur_slot.take() {
            self.heap.refill(slot);
        }
        loop {
            let slot = match self.heap.pop() {
                None => return None,
                Some(slot) => slot,
            };
            self.outs.clear();
            self.outs.push(slot.fst_output());
            let mut popped: usize = 1;
            while let Some(slot2) = self.heap.pop_if_equal(slot.input()) {
                self.outs.push(slot2.fst_output());
                self.heap.refill(slot2);
                popped += 1;
            }
            // This key is in the symmetric difference if and only if it
            // appears in an odd number of sets.
            if popped % 2 == 0 {
                self.heap.refill(slot);
            } else {
                self.cur_slot = Some(slot);
                let key = self.cur_slot.as_ref().unwrap().input();
                return Some((key, &self.outs))
            }
        }
    }
}

struct StreamHeap<'f> {
    rdrs: Vec<Box<for<'a> Stream<'a, Item=(&'a [u8], Output)> + 'f>>,
    heap: BinaryHeap<Slot>,
}

impl<'f> StreamHeap<'f> {
    fn new(streams: Vec<BoxedStream<'f>>) -> StreamHeap<'f> {
        let mut u = StreamHeap {
            rdrs: streams,
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
    use raw::tests::{fst_map, fst_set};
    use raw::Fst;
    use stream::{IntoStream, Stream};

    use super::StreamOp;

    fn s(string: &str) -> String { string.to_owned() }

    macro_rules! create_set_op {
        ($name:ident, $op:ident) => {
            fn $name(sets: Vec<Vec<&str>>) -> Vec<String> {
                let fsts: Vec<Fst> = sets.into_iter().map(fst_set).collect();
                let op: StreamOp = fsts.iter().collect();
                let mut stream = op.$op().into_stream();
                let mut keys = vec![];
                while let Some((key, _)) = stream.next() {
                    keys.push(String::from_utf8(key.to_vec()).unwrap());
                }
                keys
            }
        }
    }

    macro_rules! create_map_op {
        ($name:ident, $op:ident) => {
            fn $name(sets: Vec<Vec<(&str, u64)>>) -> Vec<(String, u64)> {
                let fsts: Vec<Fst> = sets.into_iter().map(fst_map).collect();
                let op: StreamOp = fsts.iter().collect();
                let mut stream = op.$op().into_stream();
                let mut keys = vec![];
                while let Some((key, outs)) = stream.next() {
                    let merged = outs.iter().fold(0, |a, b| a + b.output);
                    let s = String::from_utf8(key.to_vec()).unwrap();
                    keys.push((s, merged));
                }
                keys
            }
        }
    }

    create_set_op!(fst_union, union);
    create_set_op!(fst_intersection, intersection);
    create_set_op!(fst_symmetric_difference, symmetric_difference);
    create_set_op!(fst_difference, difference);
    create_map_op!(fst_union_map, union);
    create_map_op!(fst_intersection_map, intersection);
    create_map_op!(fst_symmetric_difference_map, symmetric_difference);
    create_map_op!(fst_difference_map, difference);

    #[test]
    fn union_set() {
        let v = fst_union(vec![
            vec!["a", "b", "c"],
            vec!["x", "y", "z"],
        ]);
        assert_eq!(v, vec!["a", "b", "c", "x", "y", "z"]);
    }

    #[test]
    fn union_set_dupes() {
        let v = fst_union(vec![
            vec!["aa", "b", "cc"],
            vec!["b", "cc", "z"],
        ]);
        assert_eq!(v, vec!["aa", "b", "cc", "z"]);
    }

    #[test]
    fn union_map() {
        let v = fst_union_map(vec![
            vec![("a", 1), ("b", 2), ("c", 3)],
            vec![("x", 1), ("y", 2), ("z", 3)],
        ]);
        assert_eq!(v, vec![
            (s("a"), 1), (s("b"), 2), (s("c"), 3),
            (s("x"), 1), (s("y"), 2), (s("z"), 3),
        ]);
    }

    #[test]
    fn union_map_dupes() {
        let v = fst_union_map(vec![
            vec![("aa", 1), ("b", 2), ("cc", 3)],
            vec![("b", 1), ("cc", 2), ("z", 3)],
            vec![("b", 1)],
        ]);
        assert_eq!(v, vec![
            (s("aa"), 1), (s("b"), 4), (s("cc"), 5), (s("z"), 3),
        ]);
    }

    #[test]
    fn intersect_set() {
        let v = fst_intersection(vec![
            vec!["a", "b", "c"],
            vec!["x", "y", "z"],
        ]);
        assert_eq!(v, Vec::<String>::new());
    }

    #[test]
    fn intersect_set_dupes() {
        let v = fst_intersection(vec![
            vec!["aa", "b", "cc"],
            vec!["b", "cc", "z"],
        ]);
        assert_eq!(v, vec!["b", "cc"]);
    }

    #[test]
    fn intersect_map() {
        let v = fst_intersection_map(vec![
            vec![("a", 1), ("b", 2), ("c", 3)],
            vec![("x", 1), ("y", 2), ("z", 3)],
        ]);
        assert_eq!(v, Vec::<(String, u64)>::new());
    }

    #[test]
    fn intersect_map_dupes() {
        let v = fst_intersection_map(vec![
            vec![("aa", 1), ("b", 2), ("cc", 3)],
            vec![("b", 1), ("cc", 2), ("z", 3)],
            vec![("b", 1)],
        ]);
        assert_eq!(v, vec![(s("b"), 4)]);
    }

    #[test]
    fn symmetric_difference() {
        let v = fst_symmetric_difference(vec![
            vec!["a", "b", "c"],
            vec!["a", "b"],
            vec!["a"],
        ]);
        assert_eq!(v, vec!["a", "c"]);
    }

    #[test]
    fn symmetric_difference_map() {
        let v = fst_symmetric_difference_map(vec![
            vec![("a", 1), ("b", 2), ("c", 3)],
            vec![("a", 1), ("b", 2)],
            vec![("a", 1)],
        ]);
        assert_eq!(v, vec![(s("a"), 3), (s("c"), 3)]);
    }

    #[test]
    fn difference() {
        let v = fst_difference(vec![
            vec!["a", "b", "c"],
            vec!["a", "b"],
            vec!["a"],
        ]);
        assert_eq!(v, vec!["c"]);
    }

    #[test]
    fn difference_map() {
        let v = fst_difference_map(vec![
            vec![("a", 1), ("b", 2), ("c", 3)],
            vec![("a", 1), ("b", 2)],
            vec![("a", 1)],
        ]);
        assert_eq!(v, vec![(s("c"), 3)]);
    }
}
