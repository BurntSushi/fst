use std::fmt;
use std::iter::FromIterator;
use std::io;
use std::path::Path;

use automaton::{Automaton, AlwaysMatch};
use raw::{
    Builder, Fst, FstStream, FstStreamBuilder, Output, FstOpBuilder,
    FstUnion, FstIntersection, FstDifference, FstSymmetricDifference,
};
pub use raw::IndexedValue as IndexedValue;
use set::SetStream;
use stream::{IntoStream, Stream};
use Result;

pub struct Map(Fst);

impl Map {
    /// Opens a map stored at the given file path via a memory map.
    ///
    /// The map must have been written with a compatible finite state
    /// transducer builder (`MapBuilder` qualifies). If the format is invalid
    /// or if there is a mismatch between the API version of this library
    /// and the map, then an error is returned.
    pub fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        Fst::from_file_path(path).map(Map)
    }

    /// Creates a map from its representation as a raw byte sequence.
    ///
    /// Note that this operation is very cheap (no allocations and no copies).
    ///
    /// The map must have been written with a compatible finite state
    /// transducer builder (`MapBuilder` qualifies). If the format is invalid
    /// or if there is a mismatch between the API version of this library
    /// and the map, then an error is returned.
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self> {
        Fst::from_bytes(bytes).map(Map)
    }

    /// Create a `Map` from an iterator of lexicographically ordered byte
    /// strings and associated values.
    ///
    /// If the iterator does not yield unique keys in lexicographic order, then
    /// an error is returned.
    ///
    /// Note that this is a convenience function to build a map in memory.
    /// To build a map that streams to an arbitrary `io::Write`, use
    /// `MapBuilder`.
    pub fn from_iter<K, I>(iter: I) -> Result<Self>
            where K: AsRef<[u8]>, I: IntoIterator<Item=(K, u64)> {
        let mut builder = MapBuilder::memory();
        try!(builder.extend_iter(iter));
        Map::from_bytes(try!(builder.into_inner()))
    }

    pub fn stream(&self) -> MapStream {
        MapStream(self.0.stream())
    }

    /// Return a lexicographically ordered stream of all keys in this map.
    ///
    /// While this is a stream, it does require heap space proportional to the
    /// longest key in the map.
    ///
    /// If the map is memory mapped, then no further heap space is needed.
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
    /// use fst::{IntoStream, Stream, Map};
    ///
    /// let map = Map::from_iter(vec![("a", 1), ("b", 2), ("c", 3)]).unwrap();
    /// let mut stream = map.into_stream();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, v)) = stream.next() {
    ///     kvs.push((k.to_vec(), v));
    /// }
    /// assert_eq!(kvs, vec![
    ///     ("a".as_bytes().to_vec(), 1),
    ///     ("b".as_bytes().to_vec(), 2),
    ///     ("c".as_bytes().to_vec(), 3),
    /// ]);
    /// ```
    pub fn stream_keys(&self) -> SetStream {
        SetStream::new(self.0.stream())
    }

    pub fn range(&self) -> MapStreamBuilder {
        MapStreamBuilder(self.0.range())
    }

    /// Tests the membership of a single key.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::Map;
    ///
    /// let map = Map::from_iter(vec![("a", 1), ("b", 2), ("c", 3)]).unwrap();
    ///
    /// assert_eq!(map.contains("b"), true);
    /// assert_eq!(map.contains("z"), false);
    /// ```
    pub fn contains<K: AsRef<[u8]>>(&self, key: K) -> bool {
        self.0.find(key).is_some()
    }

    pub fn search<A: Automaton>(&self, aut: A) -> MapStreamBuilder<A> {
        MapStreamBuilder(self.0.search(aut))
    }

    /// Returns the number of elements in this map.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if and only if this map is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn op(&self) -> MapOpBuilder {
        MapOpBuilder::new().add(self)
    }

    pub fn is_disjoint<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.is_disjoint(SetStreamZeroOutput(stream.into_stream()))
    }

    pub fn is_subset<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.is_subset(SetStreamZeroOutput(stream.into_stream()))
    }

    pub fn is_superset<'f, I, S>(&self, stream: I) -> bool
            where I: for<'a> IntoStream<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Stream<'a, Item=&'a [u8]> {
        self.0.is_superset(SetStreamZeroOutput(stream.into_stream()))
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Map(["));
        let mut stream = self.stream();
        let mut first = true;
        while let Some((k, v)) = stream.next() {
            if !first {
                try!(write!(f, ", "));
            }
            first = false;
            try!(write!(f, "({}, {})", String::from_utf8_lossy(k), v));
        }
        write!(f, "])")
    }
}

/// Returns the underlying finite state transducer.
impl AsRef<Fst> for Map {
    fn as_ref(&self) -> &Fst {
        &self.0
    }
}

impl<'s, 'a> IntoStream<'a> for &'s Map {
    type Item = (&'a [u8], u64);
    type Into = MapStream<'s>;

    fn into_stream(self) -> Self::Into {
        MapStream(self.0.stream())
    }
}
pub struct MapBuilder<W>(Builder<W>);

impl MapBuilder<Vec<u8>> {
    /// Create a builder that builds a map in memory.
    pub fn memory() -> Self {
        MapBuilder(Builder::memory())
    }
}

impl<W: io::Write> MapBuilder<W> {
    /// Create a builder that builds a map by writing it to `wtr` in a
    /// streaming fashion.
    pub fn new(wtr: W) -> Result<MapBuilder<W>> {
        Builder::new_type(wtr, 1).map(MapBuilder)
    }

    /// Insert a new key-value pair into the map.
    ///
    /// If a key is inserted that is less than or equal to any previous key
    /// added, then an error is returned. Similarly, if there was a problem
    /// writing to the underlying writer, an error is returned.
    pub fn insert<K: AsRef<[u8]>>(&mut self, key: K, val: u64) -> Result<()> {
        self.0.insert(key, val)
    }

    /// Calls insert on each item in the iterator.
    ///
    /// If an error occurred while adding an element, processing is stopped
    /// and the error is returned.
    pub fn extend_iter<K, I>(&mut self, iter: I) -> Result<()>
            where K: AsRef<[u8]>, I: IntoIterator<Item=(K, u64)> {
        self.0.extend_iter(iter.into_iter().map(|(k, v)| (k, Output::new(v))))
    }

    /// Calls insert on each item in the stream.
    ///
    /// Note that unlike `extend_iter`, this is not generic on the items in
    /// the stream.
    pub fn extend_stream<'f, I, S>(&mut self, stream: I) -> Result<()>
            where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], u64)>,
                  S: 'f + for<'a> Stream<'a, Item=(&'a [u8], u64)> {
        self.0.extend_stream(MapStreamOutput(stream.into_stream()))
    }

    /// Finishes the construction of the map and flushes the underlying
    /// writer. After completion, the data written to `W` may be read using
    /// one of `Map`'s constructor methods.
    pub fn finish(self) -> Result<()> {
        self.0.finish()
    }

    /// Just like `finish`, except it returns the underlying writer after
    /// flushing it.
    pub fn into_inner(self) -> Result<W> {
        self.0.into_inner()
    }
}

/// A lexicographically ordered stream of key-value pairs from a map.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying map.
pub struct MapStream<'s, A=AlwaysMatch>(FstStream<'s, A>) where A: Automaton;

impl<'a, 's, A: Automaton> Stream<'a> for MapStream<'s, A> {
    type Item = (&'a [u8], u64);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, out)| (key, out.value()))
    }
}

/// A builder for constructing range queries on streams.
///
/// Once all bounds are set, one should call `into_stream` to get a
/// `MapStream`.
///
/// Bounds are not additive. That is, if `ge` is called twice on the same
/// builder, then the second setting wins.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying map.
pub struct MapStreamBuilder<'s, A=AlwaysMatch>(FstStreamBuilder<'s, A>);

impl<'s, A: Automaton> MapStreamBuilder<'s, A> {
    /// Specify a greater-than-or-equal-to bound.
    pub fn ge<T: AsRef<[u8]>>(self, bound: T) -> Self {
        MapStreamBuilder(self.0.ge(bound))
    }

    /// Specify a greater-than bound.
    pub fn gt<T: AsRef<[u8]>>(self, bound: T) -> Self {
        MapStreamBuilder(self.0.gt(bound))
    }

    /// Specify a less-than-or-equal-to bound.
    pub fn le<T: AsRef<[u8]>>(self, bound: T) -> Self {
        MapStreamBuilder(self.0.le(bound))
    }

    /// Specify a less-than bound.
    pub fn lt<T: AsRef<[u8]>>(self, bound: T) -> Self {
        MapStreamBuilder(self.0.lt(bound))
    }
}

impl<'s, 'a, A: Automaton> IntoStream<'a> for MapStreamBuilder<'s, A> {
    type Item = (&'a [u8], u64);
    type Into = MapStream<'s, A>;

    fn into_stream(self) -> Self::Into {
        MapStream(self.0.into_stream())
    }
}

pub struct MapOpBuilder<'s>(FstOpBuilder<'s>);

impl<'s> MapOpBuilder<'s> {
    pub fn new() -> Self {
        MapOpBuilder(FstOpBuilder::new())
    }

    pub fn add<I, S>(mut self, streamable: I) -> Self
            where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], u64)>,
                  S: 's + for<'a> Stream<'a, Item=(&'a [u8], u64)> {
        self.push(streamable);
        self
    }

    pub fn push<I, S>(&mut self, streamable: I)
            where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], u64)>,
                  S: 's + for<'a> Stream<'a, Item=(&'a [u8], u64)> {
        self.0.push(MapStreamOutput(streamable.into_stream()));
    }

    pub fn union(self) -> MapUnion<'s> {
        MapUnion(self.0.union())
    }

    pub fn intersection(self) -> MapIntersection<'s> {
        MapIntersection(self.0.intersection())
    }

    pub fn difference(self) -> MapDifference<'s> {
        MapDifference(self.0.difference())
    }

    pub fn symmetric_difference(self) -> MapSymmetricDifference<'s> {
        MapSymmetricDifference(self.0.symmetric_difference())
    }
}

impl<'f, I, S> Extend<I> for MapOpBuilder<'f>
    where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], u64)>,
          S: 'f + for<'a> Stream<'a, Item=(&'a [u8], u64)> {
    fn extend<T>(&mut self, it: T) where T: IntoIterator<Item=I> {
        for stream in it {
            self.push(stream);
        }
    }
}

impl<'f, I, S> FromIterator<I> for MapOpBuilder<'f>
    where I: for<'a> IntoStream<'a, Into=S, Item=(&'a [u8], u64)>,
          S: 'f + for<'a> Stream<'a, Item=(&'a [u8], u64)> {
    fn from_iter<T>(it: T) -> Self where T: IntoIterator<Item=I> {
        let mut op = MapOpBuilder::new();
        op.extend(it);
        op
    }
}

pub struct MapUnion<'s>(FstUnion<'s>);

impl<'a, 's> Stream<'a> for MapUnion<'s> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub struct MapIntersection<'s>(FstIntersection<'s>);

impl<'a, 's> Stream<'a> for MapIntersection<'s> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub struct MapDifference<'s>(FstDifference<'s>);

impl<'a, 's> Stream<'a> for MapDifference<'s> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub struct MapSymmetricDifference<'s>(FstSymmetricDifference<'s>);

impl<'a, 's> Stream<'a> for MapSymmetricDifference<'s> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

/// A specialized stream for mapping map streams (`&[u8]`) to streams used
/// by raw fsts (`(&[u8], Output)`).
///
/// If this were iterators, we could use `iter::Map`, but doing this on streams
/// requires HKT, so we need to write out the monomorphization ourselves.
struct MapStreamOutput<S>(S);

impl<'a, S> Stream<'a> for MapStreamOutput<S>
        where S: Stream<'a, Item=(&'a [u8], u64)> {
    type Item = (&'a [u8], Output);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| (k, Output::new(v)))
    }
}

/// A specialized stream for mapping set streams (`&[u8]`) to streams used
/// by raw fsts (`(&[u8], Output)`).
///
/// If this were iterators, we could use `iter::Map`, but doing this on streams
/// requires HKT, so we need to write out the monomorphization ourselves.
struct SetStreamZeroOutput<S>(S);

impl<'a, S: Stream<'a>> Stream<'a> for SetStreamZeroOutput<S> {
    type Item = (S::Item, Output);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|key| (key, Output::zero()))
    }
}
