use std::io;
use std::path::Path;

use automaton::{Automaton, AlwaysMatch};
use raw::{Builder, Fst, FstStream, FstStreamBuilder};
use Result;

pub struct Set(Fst);

impl Set {
    pub fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        Fst::from_file_path(path).map(Set)
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self> {
        Fst::from_bytes(bytes).map(Set)
    }

    pub fn contains<B: AsRef<[u8]>>(&self, key: B) -> bool {
        self.0.find(key).is_some()
    }

    pub fn stream(&self) -> SetStream {
        SetStream(self.0.stream())
    }

    pub fn search<A: Automaton>(&self, aut: A) -> SetStreamBuilder<A> {
        SetStreamBuilder(self.0.search(aut))
    }

    pub fn range(&self) -> SetStreamBuilder {
        SetStreamBuilder(self.0.range())
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

impl<'s, A: Automaton> SetStream<'s, A> {
    pub fn next(&mut self) -> Option<&[u8]> {
        self.0.next().map(|(key, _)| key)
    }
}

pub struct SetStreamBuilder<'s, A=AlwaysMatch>(FstStreamBuilder<'s, A>);

impl<'s, A: Automaton> SetStreamBuilder<'s, A> {
    pub fn into_stream(self) -> SetStream<'s, A> {
        SetStream(self.0.into_stream())
    }

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
