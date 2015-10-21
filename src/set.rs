use std::fmt;
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

/// Set is a lexicographically ordered set of byte strings.
///
/// A `Set` is constructed with the `SetBuilder` type. Alternatively, a `Set`
/// can be constructed in memory from a lexicographically ordered iterator
/// of byte strings (`Set::new`).
///
/// A key feature of `Set` is that it can be serialized to disk compactly. Its
/// underlying representation is built such that the `Set` can be memory mapped
/// (`Set::from_file_path`) and searched without necessarily loading the entire
/// set into memory.
///
/// It supports most common operations associated with sets, such as
/// membership, union, intersection, subset/superset, etc. It also supports
/// range queries and automata based searches (e.g. a regular expression).
///
/// Sets are represented by a finite state transducer where output values are
/// always zero. As such, sets have the following invariants:
///
/// 1. Once constructed, a `Set` can never be modified.
/// 2. Sets must be constructed with lexicographically ordered byte sequences.
pub struct Set(Fst);

impl Set {
    /// Opens a set stored at the given file path via a memory map.
    ///
    /// The set must have been written with a compatible finite state
    /// transducer builder (`SetBuilder` qualifies). If the format is invalid
    /// or if there is a mismatch between the API version of this library
    /// and the set, then an error is returned.
    pub fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        Fst::from_file_path(path).map(Set)
    }

    /// Creates a set from its representation as a raw byte sequence.
    ///
    /// Note that this operation is very cheap (no allocations and no copies).
    ///
    /// The set must have been written with a compatible finite state
    /// transducer builder (`SetBuilder` qualifies). If the format is invalid
    /// or if there is a mismatch between the API version of this library
    /// and the set, then an error is returned.
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self> {
        Fst::from_bytes(bytes).map(Set)
    }

    /// Create a `Set` from an iterator of lexicographically ordered byte
    /// strings.
    ///
    /// If the iterator does not yield values in lexicographic order, then an
    /// error is returned.
    ///
    /// Note that this is a convenience function to build a set in memory.
    /// To build a set that streams to an arbitrary `io::Write`, use
    /// `SetBuilder`.
    pub fn from_iter<T, I>(iter: I) -> Result<Self>
            where T: AsRef<[u8]>, I: IntoIterator<Item=T> {
        let mut builder = SetBuilder::memory();
        try!(builder.extend_iter(iter));
        Set::from_bytes(try!(builder.into_inner()))
    }

    /// Return a lexicographically ordered stream of all keys in this set.
    ///
    /// While this is a stream, it does require heap space proportional to the
    /// longest key in the set.
    ///
    /// If the set is memory mapped, then no further heap space is needed.
    /// Note though that your operating system may fill your page cache
    /// (which will cause the resident memory usage of the process to go up
    /// correspondingly).
    ///
    /// # Example
    ///
    /// Since streams are not iterators, the traditional `for` loop cannot be
    /// used. `while let` is useful instead:
    ///
    /// ```rust
    /// use fst::{IntoStream, Stream, Set};
    ///
    /// let set = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let mut stream = set.into_stream();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = stream.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"a", b"b", b"c"]);
    /// ```
    pub fn stream(&self) -> SetStream {
        SetStream(self.0.stream())
    }

    /// Return a builder for range queries.
    ///
    /// A range query returns a subset of keys in this set in a range given in
    /// lexicographic order.
    ///
    /// Memory requirements are the same as described on `Set::stream`.
    ///
    /// # Example
    ///
    /// Returns only the keys in the range given.
    ///
    /// ```rust
    /// use fst::{IntoStream, Stream, Set};
    ///
    /// let set = Set::from_iter(&["a", "b", "c", "d", "e"]).unwrap();
    /// let mut stream = set.range().ge("b").lt("e").into_stream();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = stream.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"b", b"c", b"d"]);
    /// ```
    pub fn range(&self) -> SetStreamBuilder {
        SetStreamBuilder(self.0.range())
    }

    /// Tests the membership of a single key.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::Set;
    ///
    /// let set = Set::from_iter(&["a", "b", "c"]).unwrap();
    ///
    /// assert_eq!(set.contains("b"), true);
    /// assert_eq!(set.contains("z"), false);
    /// ```
    pub fn contains<B: AsRef<[u8]>>(&self, key: B) -> bool {
        self.0.find(key).is_some()
    }

    /// Executes an automaton on the keys of this set.
    ///
    /// Note that this returns a `SetStreamBuilder`, which can be used to
    /// add a range query to the search (see the `range` method).
    ///
    /// Memory requirements are the same as described on `Set::stream`.
    ///
    /// # Example
    ///
    /// This crate provides an implementation of regular expressions
    /// for `Automaton`. Make sure to see the documentation for `fst::Regex`
    /// for more details such as what kind of regular expressions are allowed.
    ///
    /// ```rust
    /// use fst::{IntoStream, Stream, Regex, Set};
    ///
    /// let set = Set::from_iter(&["foo", "foo1", "foo2", "foo3", "foobar"])
    ///               .unwrap();
    ///
    /// let re = Regex::new("f[a-z]+3?").unwrap();
    /// let mut stream = set.search(re).into_stream();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = stream.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![
    ///     "foo".as_bytes(), "foo3".as_bytes(), "foobar".as_bytes(),
    /// ]);
    /// ```
    pub fn search<A: Automaton>(&self, aut: A) -> SetStreamBuilder<A> {
        SetStreamBuilder(self.0.search(aut))
    }

    /// Returns the number of elements in this set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if and only if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns true if and only if the `self` set is disjoint with the set
    /// `stream`.
    ///
    /// `stream` must be a lexicographically ordered sequence of byte strings.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStream, Stream, Set};
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["x", "y", "z"]).unwrap();
    ///
    /// assert_eq!(set1.is_disjoint(&set2), true);
    ///
    /// let set3 = Set::from_iter(&["a", "c"]).unwrap();
    ///
    /// assert_eq!(set1.is_disjoint(&set3), false);
    /// ```
    pub fn is_disjoint<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.is_disjoint(SetStreamZeroOutput(stream.into_stream()))
    }

    /// Returns true if and only if the `self` set is a subset of `stream`.
    ///
    /// `stream` must be a lexicographically ordered sequence of byte strings.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::Set;
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["x", "y", "z"]).unwrap();
    ///
    /// assert_eq!(set1.is_subset(&set2), false);
    ///
    /// let set3 = Set::from_iter(&["a", "c"]).unwrap();
    ///
    /// assert_eq!(set1.is_subset(&set3), false);
    /// assert_eq!(set3.is_subset(&set1), true);
    /// ```
    pub fn is_subset<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.is_subset(SetStreamZeroOutput(stream.into_stream()))
    }

    /// Returns true if and only if the `self` set is a superset of `stream`.
    ///
    /// `stream` must be a lexicographically ordered sequence of byte strings.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::Set;
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["x", "y", "z"]).unwrap();
    ///
    /// assert_eq!(set1.is_superset(&set2), false);
    ///
    /// let set3 = Set::from_iter(&["a", "c"]).unwrap();
    ///
    /// assert_eq!(set1.is_superset(&set3), true);
    /// assert_eq!(set3.is_superset(&set1), false);
    /// ```
    pub fn is_superset<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.is_superset(SetStreamZeroOutput(stream.into_stream()))
    }
}

impl fmt::Debug for Set {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Set(["));
        let mut stream = self.stream();
        let mut first = true;
        while let Some(key) = stream.next() {
            if !first {
                try!(write!(f, ", "));
            }
            first = false;
            try!(write!(f, "{}", String::from_utf8_lossy(key)));
        }
        write!(f, "])")
    }
}

impl AsRef<Fst> for Set {
    /// Returns the underlying finite state transducer.
    fn as_ref(&self) -> &Fst {
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

    pub fn extend_iter<T, I>(&mut self, iter: I) -> Result<()>
            where T: AsRef<[u8]>, I: IntoIterator<Item=T> {
        self.0.extend_iter(iter.into_iter().map(|key| (key, Output::zero())))
    }

    pub fn extend_stream<'f, I, S>(&mut self, stream: I) -> Result<()>
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.extend_stream(SetStreamZeroOutput(stream.into_stream()))
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
    pub fn ge<T: AsRef<[u8]>>(self, bound: T) -> Self {
        SetStreamBuilder(self.0.ge(bound))
    }

    pub fn gt<T: AsRef<[u8]>>(self, bound: T) -> Self {
        SetStreamBuilder(self.0.gt(bound))
    }

    pub fn le<T: AsRef<[u8]>>(self, bound: T) -> Self {
        SetStreamBuilder(self.0.le(bound))
    }

    pub fn lt<T: AsRef<[u8]>>(self, bound: T) -> Self {
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
