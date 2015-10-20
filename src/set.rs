use std::iter::FromIterator;
use std::io;
use std::path::Path;

use automaton::{Automaton, AlwaysMatch};
use raw::{
    Builder, Fst, FstStream, FstStreamBuilder, Output,
    StreamOp, StreamUnion, StreamIntersection,
    StreamDifference, StreamSymmetricDifference,
};
use stream::{IntoStream, Stream};
use Result;

pub struct Set(Fst);

impl Set {
    pub fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        Fst::from_file_path(path).map(Set)
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self> {
        Fst::from_bytes(bytes).map(Set)
    }

    pub fn stream(&self) -> SetStream {
        SetStream(self.0.stream())
    }

    pub fn range(&self) -> SetStreamBuilder {
        SetStreamBuilder(self.0.range())
    }

    pub fn contains<B: AsRef<[u8]>>(&self, key: B) -> bool {
        self.0.find(key).is_some()
    }

    pub fn search<A: Automaton>(&self, aut: A) -> SetStreamBuilder<A> {
        SetStreamBuilder(self.0.search(aut))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn as_fst(&self) -> &Fst {
        &self.0
    }
}

impl<'s, 'a> IntoStream<'a> for &'s Set {
    type Item = &'a [u8];
    type Into = SetStream<'s>;

    fn into_stream(self) -> Self::Into {
        SetStream(self.0.stream())
    }
}

pub struct SetBuilder<W>(Builder<W>);

impl SetBuilder<Vec<u8>> {
    pub fn memory() -> Self {
        SetBuilder(Builder::memory())
    }
}

impl<W: io::Write> SetBuilder<W> {
    pub fn new(wtr: W) -> Result<SetBuilder<W>> {
        Builder::new_type(wtr, 1).map(SetBuilder)
    }

    pub fn insert<B: AsRef<[u8]>>(&mut self, bytes: B) -> Result<()> {
        self.0.add(bytes)
    }

    pub fn finish(self) -> Result<()> {
        self.0.finish()
    }

    pub fn into_inner(self) -> Result<W> {
        self.0.into_inner()
    }
}

pub struct SetStream<'s, A=AlwaysMatch>(FstStream<'s, A>) where A: Automaton;

impl<'a, 's, A: Automaton> Stream<'a> for SetStream<'s, A> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

pub struct SetStreamBuilder<'s, A=AlwaysMatch>(FstStreamBuilder<'s, A>);

impl<'s, A: Automaton> SetStreamBuilder<'s, A> {
    pub fn ge<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        SetStreamBuilder(self.0.ge(bound))
    }

    pub fn gt<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        SetStreamBuilder(self.0.gt(bound))
    }

    pub fn le<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        SetStreamBuilder(self.0.le(bound))
    }

    pub fn lt<T: AsRef<[u8]>>(mut self, bound: T) -> Self {
        SetStreamBuilder(self.0.lt(bound))
    }
}

impl<'s, 'a, A: Automaton> IntoStream<'a> for SetStreamBuilder<'s, A> {
    type Item = &'a [u8];
    type Into = SetStream<'s, A>;

    fn into_stream(self) -> Self::Into {
        SetStream(self.0.into_stream())
    }
}

pub struct SetOp<'s>(StreamOp<'s>);

impl<'s> SetOp<'s> {
    pub fn new() -> Self {
        SetOp(StreamOp::new())
    }

    pub fn add<I, S>(mut self, streamable: I) -> Self
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 's + for<'a> Stream<'a, Item=&'a [u8]> {
        self.push(streamable);
        self
    }

    pub fn push<I, S>(&mut self, streamable: I)
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 's + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.push(SetStreamZeroOutput(streamable.into_stream()));
    }

    pub fn union(self) -> SetUnion<'s> {
        SetUnion(self.0.union())
    }

    pub fn intersection(self) -> SetIntersection<'s> {
        SetIntersection(self.0.intersection())
    }

    pub fn difference(self) -> SetDifference<'s> {
        SetDifference(self.0.difference())
    }

    pub fn symmetric_difference(self) -> SetSymmetricDifference<'s> {
        SetSymmetricDifference(self.0.symmetric_difference())
    }
}

impl<'f, I, S> Extend<I> for SetOp<'f>
    where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
          S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
    fn extend<T>(&mut self, it: T) where T: IntoIterator<Item=I> {
        for stream in it {
            self.push(stream);
        }
    }
}

impl<'f, I, S> FromIterator<I> for SetOp<'f>
    where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
          S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
    fn from_iter<T>(it: T) -> Self where T: IntoIterator<Item=I> {
        let mut op = SetOp::new();
        op.extend(it);
        op
    }
}

pub struct SetUnion<'s>(StreamUnion<'s>);

impl<'a, 's> Stream<'a> for SetUnion<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

pub struct SetIntersection<'s>(StreamIntersection<'s>);

impl<'a, 's> Stream<'a> for SetIntersection<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

pub struct SetDifference<'s>(StreamDifference<'s>);

impl<'a, 's> Stream<'a> for SetDifference<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

pub struct SetSymmetricDifference<'s>(StreamSymmetricDifference<'s>);

impl<'a, 's> Stream<'a> for SetSymmetricDifference<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

struct SetStreamZeroOutput<S>(S);

impl<'a, S: Stream<'a>> Stream<'a> for SetStreamZeroOutput<S> {
    type Item = (S::Item, Output);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|key| (key, Output::zero()))
    }
}
