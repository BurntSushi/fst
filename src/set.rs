use std::fmt;
use std::iter::FromIterator;
use std::io;
#[cfg(feature = "mmap")]
use std::path::Path;

use automaton::{Automaton, AlwaysMatch};
use raw;
use stream::{IntoStreamer, Streamer};
use Result;

/// Set is a lexicographically ordered set of byte strings.
///
/// A `Set` is constructed with the `SetBuilder` type. Alternatively, a `Set`
/// can be constructed in memory from a lexicographically ordered iterator
/// of byte strings (`Set::from_iter`).
///
/// A key feature of `Set` is that it can be serialized to disk compactly. Its
/// underlying representation is built such that the `Set` can be memory mapped
/// (`Set::from_path`) and searched without necessarily loading the entire
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
pub struct Set(raw::Fst);

impl Set {
    /// Opens a set stored at the given file path via a memory map.
    ///
    /// The set must have been written with a compatible finite state
    /// transducer builder (`SetBuilder` qualifies). If the format is invalid
    /// or if there is a mismatch between the API version of this library
    /// and the set, then an error is returned.
    ///
    /// This is unsafe because Rust programs cannot guarantee that memory
    /// backed by a memory mapped file won't be mutably aliased. It is up to
    /// the caller to enforce that the memory map is not modified while it is
    /// opened.
    #[cfg(feature = "mmap")]
    pub unsafe fn from_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        raw::Fst::from_path(path).map(Set)
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
        raw::Fst::from_bytes(bytes).map(Set)
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
        builder.extend_iter(iter)?;
        Set::from_bytes(builder.into_inner()?)
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
    pub fn contains<K: AsRef<[u8]>>(&self, key: K) -> bool {
        self.0.contains_key(key)
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
    /// use fst::{IntoStreamer, Streamer, Set};
    ///
    /// let set = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let mut stream = set.stream();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = stream.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"a", b"b", b"c"]);
    /// ```
    pub fn stream(&self) -> Stream {
        Stream(self.0.stream())
    }

    /// Return a builder for range queries.
    ///
    /// A range query returns a subset of keys in this set in a range given in
    /// lexicographic order.
    ///
    /// Memory requirements are the same as described on `Set::stream`.
    /// Notably, only the keys in the range are read; keys outside the range
    /// are not.
    ///
    /// # Example
    ///
    /// Returns only the keys in the range given.
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Set};
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
    pub fn range(&self) -> StreamBuilder {
        StreamBuilder(self.0.range())
    }

    /// Executes an automaton on the keys of this set.
    ///
    /// Note that this returns a `StreamBuilder`, which can be used to
    /// add a range query to the search (see the `range` method).
    ///
    /// Memory requirements are the same as described on `Set::stream`.
    ///
    /// # Example
    ///
    /// An implementation of regular expressions for `Automaton` is available
    /// in the `fst-regex` crate, which can be used to search sets.
    ///
    /// ```rust
    /// extern crate fst;
    /// extern crate fst_regex;
    ///
    /// use std::error::Error;
    ///
    /// use fst::{IntoStreamer, Streamer, Set};
    /// use fst_regex::Regex;
    ///
    /// # fn main() { example().unwrap(); }
    /// fn example() -> Result<(), Box<Error>> {
    ///     let set = Set::from_iter(&[
    ///         "foo", "foo1", "foo2", "foo3", "foobar",
    ///     ]).unwrap();
    ///
    ///     let re = Regex::new("f[a-z]+3?").unwrap();
    ///     let mut stream = set.search(&re).into_stream();
    ///
    ///     let mut keys = vec![];
    ///     while let Some(key) = stream.next() {
    ///         keys.push(key.to_vec());
    ///     }
    ///     assert_eq!(keys, vec![
    ///         "foo".as_bytes(), "foo3".as_bytes(), "foobar".as_bytes(),
    ///     ]);
    ///
    ///     Ok(())
    /// }
    /// ```
    pub fn search<A: Automaton>(&self, aut: A) -> StreamBuilder<A> {
        StreamBuilder(self.0.search(aut))
    }

    /// Returns the number of elements in this set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if and only if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates a new set operation with this set added to it.
    ///
    /// The `OpBuilder` type can be used to add additional set streams
    /// and perform set operations like union, intersection, difference and
    /// symmetric difference.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Set};
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["a", "y", "z"]).unwrap();
    ///
    /// let mut union = set1.op().add(&set2).union();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = union.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"a", b"b", b"c", b"y", b"z"]);
    /// ```
    pub fn op(&self) -> OpBuilder {
        OpBuilder::new().add(self)
    }

    /// Returns true if and only if the `self` set is disjoint with the set
    /// `stream`.
    ///
    /// `stream` must be a lexicographically ordered sequence of byte strings.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Set};
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
            where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Streamer<'a, Item=&'a [u8]> {
        self.0.is_disjoint(StreamZeroOutput(stream.into_stream()))
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
            where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Streamer<'a, Item=&'a [u8]> {
        self.0.is_subset(StreamZeroOutput(stream.into_stream()))
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
            where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Streamer<'a, Item=&'a [u8]> {
        self.0.is_superset(StreamZeroOutput(stream.into_stream()))
    }

    /// Returns a reference to the underlying raw finite state transducer.
    pub fn as_fst(&self) -> &raw::Fst {
        &self.0
    }
}

impl fmt::Debug for Set {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Set([")?;
        let mut stream = self.stream();
        let mut first = true;
        while let Some(key) = stream.next() {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}", String::from_utf8_lossy(key))?;
        }
        write!(f, "])")
    }
}

/// Returns the underlying finite state transducer.
impl AsRef<raw::Fst> for Set {
    fn as_ref(&self) -> &raw::Fst {
        &self.0
    }
}

impl<'s, 'a> IntoStreamer<'a> for &'s Set {
    type Item = &'a [u8];
    type Into = Stream<'s>;

    fn into_stream(self) -> Self::Into {
        Stream(self.0.stream())
    }
}

// Construct a set from an Fst object.
impl From<raw::Fst> for Set {
    fn from(fst: raw::Fst) -> Set {
        Set(fst)
    }
}

/// A builder for creating a set.
///
/// This is not your average everyday builder. It has two important qualities
/// that make it a bit unique from what you might expect:
///
/// 1. All keys must be added in lexicographic order. Adding a key out of order
///    will result in an error.
/// 2. The representation of a set is streamed to *any* `io::Write` as it is
///    built. For an in memory representation, this can be a `Vec<u8>`.
///
/// Point (2) is especially important because it means that a set can be
/// constructed *without storing the entire set in memory*. Namely, since it
/// works with any `io::Write`, it can be streamed directly to a file.
///
/// With that said, the builder does use memory, but **memory usage is bounded
/// to a constant size**. The amount of memory used trades off with the
/// compression ratio. Currently, the implementation hard codes this trade off
/// which can result in about 5-20MB of heap usage during construction. (N.B.
/// Guaranteeing a maximal compression ratio requires memory proportional to
/// the size of the set, which defeats the benefit of streaming it to disk.
/// In practice, a small bounded amount of memory achieves close-to-minimal
/// compression ratios.)
///
/// The algorithmic complexity of set construction is `O(n)` where `n` is the
/// number of elements added to the set.
///
/// # Example: build in memory
///
/// This shows how to use the builder to construct a set in memory. Note that
/// `Set::from_iter` provides a convenience function that achieves this same
/// goal without needing to explicitly use `SetBuilder`.
///
/// ```rust
/// use fst::{IntoStreamer, Streamer, Set, SetBuilder};
///
/// let mut build = SetBuilder::memory();
/// build.insert("bruce").unwrap();
/// build.insert("clarence").unwrap();
/// build.insert("stevie").unwrap();
///
/// // You could also call `finish()` here, but since we're building the set in
/// // memory, there would be no way to get the `Vec<u8>` back.
/// let bytes = build.into_inner().unwrap();
///
/// // At this point, the set has been constructed, but here's how to read it.
/// let set = Set::from_bytes(bytes).unwrap();
/// let mut stream = set.into_stream();
/// let mut keys = vec![];
/// while let Some(key) = stream.next() {
///     keys.push(key.to_vec());
/// }
/// assert_eq!(keys, vec![
///     "bruce".as_bytes(), "clarence".as_bytes(), "stevie".as_bytes(),
/// ]);
/// ```
///
/// # Example: stream to file
///
/// This shows how to stream construction of a set to a file.
///
/// ```rust,no_run
/// use std::fs::File;
/// use std::io;
///
/// use fst::{IntoStreamer, Streamer, Set, SetBuilder};
///
/// let mut wtr = io::BufWriter::new(File::create("set.fst").unwrap());
/// let mut build = SetBuilder::new(wtr).unwrap();
/// build.insert("bruce").unwrap();
/// build.insert("clarence").unwrap();
/// build.insert("stevie").unwrap();
///
/// // If you want the writer back, then call `into_inner`. Otherwise, this
/// // will finish construction and call `flush`.
/// build.finish().unwrap();
///
/// // At this point, the set has been constructed, but here's how to read it.
/// let set = unsafe { Set::from_path("set.fst").unwrap() };
/// let mut stream = set.into_stream();
/// let mut keys = vec![];
/// while let Some(key) = stream.next() {
///     keys.push(key.to_vec());
/// }
/// assert_eq!(keys, vec![
///     "bruce".as_bytes(), "clarence".as_bytes(), "stevie".as_bytes(),
/// ]);
/// ```
pub struct SetBuilder<W>(raw::Builder<W>);

impl SetBuilder<Vec<u8>> {
    /// Create a builder that builds a set in memory.
    pub fn memory() -> Self {
        SetBuilder(raw::Builder::memory())
    }
}

impl<W: io::Write> SetBuilder<W> {
    /// Create a builder that builds a set by writing it to `wtr` in a
    /// streaming fashion.
    pub fn new(wtr: W) -> Result<SetBuilder<W>> {
        raw::Builder::new_type(wtr, 0).map(SetBuilder)
    }

    /// Insert a new key into the set.
    ///
    /// If a key is inserted that is less than any previous key added, then
    /// an error is returned. Similarly, if there was a problem writing to
    /// the underlying writer, an error is returned.
    pub fn insert<K: AsRef<[u8]>>(&mut self, key: K) -> Result<()> {
        self.0.add(key)
    }

    /// Calls insert on each item in the iterator.
    ///
    /// If an error occurred while adding an element, processing is stopped
    /// and the error is returned.
    pub fn extend_iter<T, I>(&mut self, iter: I) -> Result<()>
            where T: AsRef<[u8]>, I: IntoIterator<Item=T> {
        for key in iter {
            self.0.add(key)?;
        }
        Ok(())
    }

    /// Calls insert on each item in the stream.
    ///
    /// Note that unlike `extend_iter`, this is not generic on the items in
    /// the stream.
    pub fn extend_stream<'f, I, S>(&mut self, stream: I) -> Result<()>
            where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
                  S: 'f + for<'a> Streamer<'a, Item=&'a [u8]> {
        self.0.extend_stream(StreamZeroOutput(stream.into_stream()))
    }

    /// Finishes the construction of the set and flushes the underlying
    /// writer. After completion, the data written to `W` may be read using
    /// one of `Set`'s constructor methods.
    pub fn finish(self) -> Result<()> {
        self.0.finish()
    }

    /// Just like `finish`, except it returns the underlying writer after
    /// flushing it.
    pub fn into_inner(self) -> Result<W> {
        self.0.into_inner()
    }

    /// Gets a reference to the underlying writer.
    pub fn get_ref(&self) -> &W {
        self.0.get_ref()
    }

    /// Returns the number of bytes written to the underlying writer
    pub fn bytes_written(&self) -> u64 {
        self.0.bytes_written()
    }

}

/// A lexicographically ordered stream of keys from a set.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct Stream<'s, A=AlwaysMatch>(raw::Stream<'s, A>) where A: Automaton;

impl<'s, A: Automaton> Stream<'s, A> {
    /// Creates a new set stream from an fst stream.
    ///
    /// Not part of the public API, but useful in sibling module `map`.
    #[doc(hidden)]
    pub fn new(fst_stream: raw::Stream<'s, A>) -> Self {
        Stream(fst_stream)
    }

    /// Convert this stream into a vector of Unicode strings.
    ///
    /// If any key is not valid UTF-8, then iteration on the stream is stopped
    /// and a UTF-8 decoding error is returned.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_strs(self) -> Result<Vec<String>> {
        self.0.into_str_keys()
    }

    /// Convert this stream into a vector of byte strings.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_bytes(self) -> Vec<Vec<u8>> {
        self.0.into_byte_keys()
    }
}

impl<'a, 's, A: Automaton> Streamer<'a> for Stream<'s, A> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A builder for constructing range queries on streams.
///
/// Once all bounds are set, one should call `into_stream` to get a
/// `Stream`.
///
/// Bounds are not additive. That is, if `ge` is called twice on the same
/// builder, then the second setting wins.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct StreamBuilder<'s, A=AlwaysMatch>(raw::StreamBuilder<'s, A>);

impl<'s, A: Automaton> StreamBuilder<'s, A> {
    /// Specify a greater-than-or-equal-to bound.
    pub fn ge<T: AsRef<[u8]>>(self, bound: T) -> Self {
        StreamBuilder(self.0.ge(bound))
    }

    /// Specify a greater-than bound.
    pub fn gt<T: AsRef<[u8]>>(self, bound: T) -> Self {
        StreamBuilder(self.0.gt(bound))
    }

    /// Specify a less-than-or-equal-to bound.
    pub fn le<T: AsRef<[u8]>>(self, bound: T) -> Self {
        StreamBuilder(self.0.le(bound))
    }

    /// Specify a less-than bound.
    pub fn lt<T: AsRef<[u8]>>(self, bound: T) -> Self {
        StreamBuilder(self.0.lt(bound))
    }
}

impl<'s, 'a, A: Automaton> IntoStreamer<'a> for StreamBuilder<'s, A> {
    type Item = &'a [u8];
    type Into = Stream<'s, A>;

    fn into_stream(self) -> Self::Into {
        Stream(self.0.into_stream())
    }
}

/// A builder for collecting set streams on which to perform set operations.
///
/// Set operations include intersection, union, difference and symmetric
/// difference. The result of each set operation is itself a stream that emits
/// keys in lexicographic order.
///
/// All set operations work efficiently on an arbitrary number of
/// streams with memory proportional to the number of streams.
///
/// The algorithmic complexity of all set operations is `O(n1 + n2 + n3 + ...)`
/// where `n1, n2, n3, ...` correspond to the number of elements in each
/// stream.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct OpBuilder<'s>(raw::OpBuilder<'s>);

impl<'s> OpBuilder<'s> {
    /// Create a new set operation builder.
    pub fn new() -> Self {
        OpBuilder(raw::OpBuilder::new())
    }

    /// Add a stream to this set operation.
    ///
    /// This is useful for a chaining style pattern, e.g.,
    /// `builder.add(stream1).add(stream2).union()`.
    ///
    /// The stream must emit a lexicographically ordered sequence of byte
    /// strings.
    pub fn add<I, S>(mut self, streamable: I) -> Self
            where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
                  S: 's + for<'a> Streamer<'a, Item=&'a [u8]> {
        self.push(streamable);
        self
    }

    /// Add a stream to this set operation.
    ///
    /// The stream must emit a lexicographically ordered sequence of byte
    /// strings.
    pub fn push<I, S>(&mut self, streamable: I)
            where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
                  S: 's + for<'a> Streamer<'a, Item=&'a [u8]> {
        self.0.push(StreamZeroOutput(streamable.into_stream()));
    }

    /// Performs a union operation on all streams that have been added.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Set};
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["a", "y", "z"]).unwrap();
    ///
    /// let mut union = set1.op().add(&set2).union();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = union.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"a", b"b", b"c", b"y", b"z"]);
    /// ```
    pub fn union(self) -> Union<'s> {
        Union(self.0.union())
    }

    /// Performs an intersection operation on all streams that have been added.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Set};
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["a", "y", "z"]).unwrap();
    ///
    /// let mut intersection = set1.op().add(&set2).intersection();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = intersection.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"a"]);
    /// ```
    pub fn intersection(self) -> Intersection<'s> {
        Intersection(self.0.intersection())
    }

    /// Performs a difference operation with respect to the first stream added.
    /// That is, this returns a stream of all elements in the first stream
    /// that don't exist in any other stream that has been added.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Set};
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["a", "y", "z"]).unwrap();
    ///
    /// let mut difference = set1.op().add(&set2).difference();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = difference.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"b", b"c"]);
    /// ```
    pub fn difference(self) -> Difference<'s> {
        Difference(self.0.difference())
    }

    /// Performs a symmetric difference operation on all of the streams that
    /// have been added.
    ///
    /// When there are only two streams, then the keys returned correspond to
    /// keys that are in either stream but *not* in both streams.
    ///
    /// More generally, for any number of streams, keys that occur in an odd
    /// number of streams are returned.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Set};
    ///
    /// let set1 = Set::from_iter(&["a", "b", "c"]).unwrap();
    /// let set2 = Set::from_iter(&["a", "y", "z"]).unwrap();
    ///
    /// let mut sym_difference = set1.op().add(&set2).symmetric_difference();
    ///
    /// let mut keys = vec![];
    /// while let Some(key) = sym_difference.next() {
    ///     keys.push(key.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"b", b"c", b"y", b"z"]);
    /// ```
    pub fn symmetric_difference(self) -> SymmetricDifference<'s> {
        SymmetricDifference(self.0.symmetric_difference())
    }
}

impl<'f, I, S> Extend<I> for OpBuilder<'f>
    where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
          S: 'f + for<'a> Streamer<'a, Item=&'a [u8]> {
    fn extend<T>(&mut self, it: T) where T: IntoIterator<Item=I> {
        for stream in it {
            self.push(stream);
        }
    }
}

impl<'f, I, S> FromIterator<I> for OpBuilder<'f>
    where I: for<'a> IntoStreamer<'a, Into=S, Item=&'a [u8]>,
          S: 'f + for<'a> Streamer<'a, Item=&'a [u8]> {
    fn from_iter<T>(it: T) -> Self where T: IntoIterator<Item=I> {
        let mut op = OpBuilder::new();
        op.extend(it);
        op
    }
}

/// A stream of set union over multiple streams in lexicographic order.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct Union<'s>(raw::Union<'s>);

impl<'a, 's> Streamer<'a> for Union<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A stream of set intersection over multiple streams in lexicographic order.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct Intersection<'s>(raw::Intersection<'s>);

impl<'a, 's> Streamer<'a> for Intersection<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A stream of set difference over multiple streams in lexicographic order.
///
/// The difference operation is taken with respect to the first stream and the
/// rest of the streams. i.e., All elements in the first stream that do not
/// appear in any other streams.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct Difference<'s>(raw::Difference<'s>);

impl<'a, 's> Streamer<'a> for Difference<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A stream of set symmetric difference over multiple streams in lexicographic
/// order.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct SymmetricDifference<'s>(raw::SymmetricDifference<'s>);

impl<'a, 's> Streamer<'a> for SymmetricDifference<'s> {
    type Item = &'a [u8];

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A specialized stream for mapping set streams (`&[u8]`) to streams used
/// by raw fsts (`(&[u8], Output)`).
///
/// If this were iterators, we could use `iter::Map`, but doing this on streams
/// requires HKT, so we need to write out the monomorphization ourselves.
struct StreamZeroOutput<S>(S);

impl<'a, S: Streamer<'a>> Streamer<'a> for StreamZeroOutput<S> {
    type Item = (S::Item, raw::Output);

    fn next(&'a mut self) -> Option<Self::Item> {
        self.0.next().map(|key| (key, raw::Output::zero()))
    }
}

#[cfg(test)]
mod tests {
    use Streamer;
    use super::OpBuilder;

    #[test]
    fn no_fsts() {
        struct Iter<'a> {
            i: usize,
            xs: Vec<&'a [u8]>,
        }

        impl<'a> Iter<'a> {
            fn new(xs: Vec<&'a [u8]>) -> Iter<'a> {
                Iter { i: 0, xs: xs }
            }
        }

        impl<'a, 's> Streamer<'a> for Iter<'s> {
            type Item = &'a [u8];
            fn next(&'a mut self) -> Option<&'a [u8]> {
                if self.i >= self.xs.len() {
                    None
                } else {
                    let i = self.i;
                    self.i += 1;
                    Some(self.xs[i])
                }
            }
        }

        let mut stream = OpBuilder::new()
            .add(Iter::new(vec![
                &b"bar"[..],
                &b"baz"[..],
                &b"foo"[..],
                &b"fubar"[..],
                &b"quux"[..],
            ]))
            .add(Iter::new(vec![
                &b"bar"[..],
                &b"foofoo"[..],
                &b"fubar"[..],
            ]))
            .intersection();

        let mut got = vec![];
        while let Some(x) = stream.next() {
            got.push(x.to_vec());
        }
        assert_eq!(got, vec![&b"bar"[..], &b"fubar"[..]]);
    }
}
