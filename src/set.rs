use std::fmt;
use std::io;
use std::iter::{self, FromIterator};

use crate::automaton::{AlwaysMatch, Automaton};
use crate::raw;
use crate::stream::{IntoStreamer, Streamer};
use crate::Result;

/// Set is a lexicographically ordered set of byte strings.
///
/// A `Set` is constructed with the `SetBuilder` type. Alternatively, a `Set`
/// can be constructed in memory from a lexicographically ordered iterator
/// of byte strings (`Set::from_iter`).
///
/// A key feature of `Set` is that it can be serialized to disk compactly. Its
/// underlying representation is built such that the `Set` can be memory mapped
/// and searched without necessarily loading the entire set into memory.
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
#[derive(Clone)]
pub struct Set<D>(raw::Fst<D>);

impl Set<Vec<u8>> {
    /// Create a `Set` from an iterator of lexicographically ordered byte
    /// strings.
    ///
    /// If the iterator does not yield values in lexicographic order, then an
    /// error is returned.
    ///
    /// Note that this is a convenience function to build a set in memory.
    /// To build a set that streams to an arbitrary `io::Write`, use
    /// `SetBuilder`.
    pub fn from_iter<T, I>(iter: I) -> Result<Set<Vec<u8>>>
    where
        T: AsRef<[u8]>,
        I: IntoIterator<Item = T>,
    {
        let mut builder = SetBuilder::memory();
        builder.extend_iter(iter)?;
        Set::new(builder.into_inner()?)
    }
}

impl<D: AsRef<[u8]>> Set<D> {
    /// Creates a set from its representation as a raw byte sequence.
    ///
    /// This accepts anything that can be cheaply converted to a `&[u8]`. The
    /// caller is responsible for guaranteeing that the given bytes refer to
    /// a valid FST. While memory safety will not be violated by invalid input,
    /// a panic could occur while reading the FST at any point.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use fst::Set;
    ///
    /// // File written from a build script using SetBuilder.
    /// # const IGNORE: &str = stringify! {
    /// static FST: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/set.fst"));
    /// # };
    /// # static FST: &[u8] = &[];
    ///
    /// let set = Set::new(FST).unwrap();
    /// ```
    pub fn new(data: D) -> Result<Set<D>> {
        raw::Fst::new(data).map(Set)
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
    #[inline]
    pub fn stream(&self) -> Stream<'_> {
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
    #[inline]
    pub fn range(&self) -> StreamBuilder<'_> {
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
    /// An implementation of subsequence search for `Automaton` can be used
    /// to search sets:
    ///
    /// ```rust
    /// use fst::automaton::Subsequence;
    /// use fst::{IntoStreamer, Streamer, Set};
    ///
    /// # fn main() { example().unwrap(); }
    /// fn example() -> Result<(), Box<dyn std::error::Error>> {
    ///     let set = Set::from_iter(&[
    ///         "a foo bar", "foo", "foo1", "foo2", "foo3", "foobar",
    ///     ]).unwrap();
    ///
    ///     let matcher = Subsequence::new("for");
    ///     let mut stream = set.search(&matcher).into_stream();
    ///
    ///     let mut keys = vec![];
    ///     while let Some(key) = stream.next() {
    ///         keys.push(String::from_utf8(key.to_vec())?);
    ///     }
    ///     assert_eq!(keys, vec![
    ///         "a foo bar", "foobar",
    ///     ]);
    ///
    ///     Ok(())
    /// }
    /// ```
    pub fn search<A: Automaton>(&self, aut: A) -> StreamBuilder<'_, A> {
        StreamBuilder(self.0.search(aut))
    }

    /// Executes an automaton on the values of this set and yields matching
    /// values along with the corresponding matching states in the given
    /// automaton.
    ///
    /// Note that this returns a `StreamWithStateBuilder`, which can be used to
    /// add a range query to the search (see the `range` method).
    ///
    /// Memory requirements are the same as described on `Map::stream`.
    ///
    #[cfg_attr(
        feature = "levenshtein",
        doc = r##"
# Example

An implementation of fuzzy search using Levenshtein automata can be used
to search sets:

```rust
use fst::automaton::Levenshtein;
use fst::{IntoStreamer, Streamer, Set};

# fn main() { example().unwrap(); }
fn example() -> Result<(), Box<dyn std::error::Error>> {
    let set = Set::from_iter(vec![
        "foo",
        "foob",
        "foobar",
        "fozb",
    ]).unwrap();

    let query = Levenshtein::new("foo", 2)?;
    let mut stream = set.search_with_state(&query).into_stream();

    let mut vs = vec![];
    while let Some((v, s)) = stream.next() {
        vs.push((String::from_utf8(v.to_vec())?, s));
    }
    // Currently, there isn't much interesting that you can do with the states.
    assert_eq!(vs, vec![
        ("foo".to_string(), Some(183)),
        ("foob".to_string(), Some(123)),
        ("fozb".to_string(), Some(83)),
    ]);

    Ok(())
}
```
"##
    )]
    pub fn search_with_state<A: Automaton>(
        &self,
        aut: A,
    ) -> StreamWithStateBuilder<'_, A> {
        StreamWithStateBuilder(self.0.search_with_state(aut))
    }

    /// Returns the number of elements in this set.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if and only if this set is empty.
    #[inline]
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
    #[inline]
    pub fn op(&self) -> OpBuilder<'_> {
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
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
        S: 'f + for<'a> Streamer<'a, Item = &'a [u8]>,
    {
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
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
        S: 'f + for<'a> Streamer<'a, Item = &'a [u8]>,
    {
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
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
        S: 'f + for<'a> Streamer<'a, Item = &'a [u8]>,
    {
        self.0.is_superset(StreamZeroOutput(stream.into_stream()))
    }

    /// Returns a reference to the underlying raw finite state transducer.
    #[inline]
    pub fn as_fst(&self) -> &raw::Fst<D> {
        &self.0
    }

    /// Returns the underlying raw finite state transducer.
    #[inline]
    pub fn into_fst(self) -> raw::Fst<D> {
        self.0
    }

    /// Maps the underlying data of the fst Set to another data type.
    ///
    /// # Example
    ///
    /// This example shows that you can map an fst Set based on a `Vec<u8>`
    /// into an fst Set based on a `Cow<[u8]>`, it can also work with a
    /// reference counted type (e.g. `Arc`, `Rc`).
    ///
    /// ```
    /// use std::borrow::Cow;
    ///
    /// use fst::Set;
    ///
    /// let set: Set<Vec<u8>> = Set::from_iter(
    ///     &["hello", "world"],
    /// ).unwrap();
    ///
    /// let set_on_cow: Set<Cow<[u8]>> = set.map_data(Cow::Owned).unwrap();
    /// ```
    #[inline]
    pub fn map_data<F, T>(self, f: F) -> Result<Set<T>>
    where
        F: FnMut(D) -> T,
        T: AsRef<[u8]>,
    {
        self.into_fst().map_data(f).map(Set::from)
    }
}

impl Default for Set<Vec<u8>> {
    #[inline]
    fn default() -> Set<Vec<u8>> {
        Set::from_iter(iter::empty::<&[u8]>()).unwrap()
    }
}

impl<D: AsRef<[u8]>> fmt::Debug for Set<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
impl<D: AsRef<[u8]>> AsRef<raw::Fst<D>> for Set<D> {
    #[inline]
    fn as_ref(&self) -> &raw::Fst<D> {
        &self.0
    }
}

impl<'s, 'a, D: AsRef<[u8]>> IntoStreamer<'a> for &'s Set<D> {
    type Item = &'a [u8];
    type Into = Stream<'s>;

    #[inline]
    fn into_stream(self) -> Stream<'s> {
        Stream(self.0.stream())
    }
}

// Construct a set from an Fst object.
impl<D: AsRef<[u8]>> From<raw::Fst<D>> for Set<D> {
    #[inline]
    fn from(fst: raw::Fst<D>) -> Set<D> {
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
/// let set = Set::new(bytes).unwrap();
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
/// // NOTE: Normally, one would memory map a file instead of reading its
/// // entire contents on to the heap.
/// let set = Set::new(std::fs::read("set.fst").unwrap()).unwrap();
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
    #[inline]
    pub fn memory() -> SetBuilder<Vec<u8>> {
        SetBuilder(raw::Builder::memory())
    }

    /// Finishes the construction of the set and returns it.
    #[inline]
    pub fn into_set(self) -> Set<Vec<u8>> {
        Set(self.0.into_fst())
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
    where
        T: AsRef<[u8]>,
        I: IntoIterator<Item = T>,
    {
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
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
        S: 'f + for<'a> Streamer<'a, Item = &'a [u8]>,
    {
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
pub struct Stream<'s, A = AlwaysMatch>(raw::Stream<'s, A>)
where
    A: Automaton;

impl<'s, A: Automaton> Stream<'s, A> {
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

    fn next(&'a mut self) -> Option<&'a [u8]> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A lexicographically ordered stream of key-state pairs from a set and
/// an automaton.
///
/// The keys are from the set while the states are from the automaton.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying set.
pub struct StreamWithState<'m, A = AlwaysMatch>(raw::StreamWithState<'m, A>)
where
    A: Automaton;

impl<'a, 'm, A: 'a + Automaton> Streamer<'a> for StreamWithState<'m, A>
where
    A::State: Clone,
{
    type Item = (&'a [u8], A::State);

    fn next(&'a mut self) -> Option<(&'a [u8], A::State)> {
        self.0.next().map(|(key, _, state)| (key, state))
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
pub struct StreamBuilder<'s, A = AlwaysMatch>(raw::StreamBuilder<'s, A>);

impl<'s, A: Automaton> StreamBuilder<'s, A> {
    /// Specify a greater-than-or-equal-to bound.
    pub fn ge<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'s, A> {
        StreamBuilder(self.0.ge(bound))
    }

    /// Specify a greater-than bound.
    pub fn gt<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'s, A> {
        StreamBuilder(self.0.gt(bound))
    }

    /// Specify a less-than-or-equal-to bound.
    pub fn le<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'s, A> {
        StreamBuilder(self.0.le(bound))
    }

    /// Specify a less-than bound.
    pub fn lt<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'s, A> {
        StreamBuilder(self.0.lt(bound))
    }
}

impl<'s, 'a, A: Automaton> IntoStreamer<'a> for StreamBuilder<'s, A> {
    type Item = &'a [u8];
    type Into = Stream<'s, A>;

    fn into_stream(self) -> Stream<'s, A> {
        Stream(self.0.into_stream())
    }
}

/// A builder for constructing range queries on streams that include automaton
/// states.
///
/// In general, one should use `StreamBuilder` unless you have a specific need
/// for accessing the states of the underlying automaton that is being used to
/// filter this stream.
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
pub struct StreamWithStateBuilder<'s, A = AlwaysMatch>(
    raw::StreamWithStateBuilder<'s, A>,
);

impl<'s, A: Automaton> StreamWithStateBuilder<'s, A> {
    /// Specify a greater-than-or-equal-to bound.
    pub fn ge<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'s, A> {
        StreamWithStateBuilder(self.0.ge(bound))
    }

    /// Specify a greater-than bound.
    pub fn gt<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'s, A> {
        StreamWithStateBuilder(self.0.gt(bound))
    }

    /// Specify a less-than-or-equal-to bound.
    pub fn le<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'s, A> {
        StreamWithStateBuilder(self.0.le(bound))
    }

    /// Specify a less-than bound.
    pub fn lt<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'s, A> {
        StreamWithStateBuilder(self.0.lt(bound))
    }
}

impl<'s, 'a, A: 'a + Automaton> IntoStreamer<'a>
    for StreamWithStateBuilder<'s, A>
where
    A::State: Clone,
{
    type Item = (&'a [u8], A::State);
    type Into = StreamWithState<'s, A>;

    fn into_stream(self) -> StreamWithState<'s, A> {
        StreamWithState(self.0.into_stream())
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
    #[inline]
    pub fn new() -> OpBuilder<'s> {
        OpBuilder(raw::OpBuilder::new())
    }

    /// Add a stream to this set operation.
    ///
    /// This is useful for a chaining style pattern, e.g.,
    /// `builder.add(stream1).add(stream2).union()`.
    ///
    /// The stream must emit a lexicographically ordered sequence of byte
    /// strings.
    pub fn add<I, S>(mut self, streamable: I) -> OpBuilder<'s>
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
        S: 's + for<'a> Streamer<'a, Item = &'a [u8]>,
    {
        self.push(streamable);
        self
    }

    /// Add a stream to this set operation.
    ///
    /// The stream must emit a lexicographically ordered sequence of byte
    /// strings.
    pub fn push<I, S>(&mut self, streamable: I)
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
        S: 's + for<'a> Streamer<'a, Item = &'a [u8]>,
    {
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
    #[inline]
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
    #[inline]
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
    #[inline]
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
    #[inline]
    pub fn symmetric_difference(self) -> SymmetricDifference<'s> {
        SymmetricDifference(self.0.symmetric_difference())
    }
}

impl<'f, I, S> Extend<I> for OpBuilder<'f>
where
    I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
    S: 'f + for<'a> Streamer<'a, Item = &'a [u8]>,
{
    fn extend<T>(&mut self, it: T)
    where
        T: IntoIterator<Item = I>,
    {
        for stream in it {
            self.push(stream);
        }
    }
}

impl<'f, I, S> FromIterator<I> for OpBuilder<'f>
where
    I: for<'a> IntoStreamer<'a, Into = S, Item = &'a [u8]>,
    S: 'f + for<'a> Streamer<'a, Item = &'a [u8]>,
{
    fn from_iter<T>(it: T) -> OpBuilder<'f>
    where
        T: IntoIterator<Item = I>,
    {
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

    #[inline]
    fn next(&'a mut self) -> Option<&'a [u8]> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A stream of set intersection over multiple streams in lexicographic order.
///
/// The `'s` lifetime parameter refers to the lifetime of the underlying set.
pub struct Intersection<'s>(raw::Intersection<'s>);

impl<'a, 's> Streamer<'a> for Intersection<'s> {
    type Item = &'a [u8];

    #[inline]
    fn next(&'a mut self) -> Option<&'a [u8]> {
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

    #[inline]
    fn next(&'a mut self) -> Option<&'a [u8]> {
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

    #[inline]
    fn next(&'a mut self) -> Option<&'a [u8]> {
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

    fn next(&'a mut self) -> Option<(S::Item, raw::Output)> {
        self.0.next().map(|key| (key, raw::Output::zero()))
    }
}

#[cfg(test)]
mod tests {
    use super::OpBuilder;
    use crate::Streamer;

    #[test]
    fn no_fsts() {
        struct Iter<'a> {
            i: usize,
            xs: Vec<&'a [u8]>,
        }

        impl<'a> Iter<'a> {
            fn new(xs: Vec<&'a [u8]>) -> Iter<'a> {
                Iter { i: 0, xs }
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
            .add(Iter::new(vec![&b"bar"[..], &b"foofoo"[..], &b"fubar"[..]]))
            .intersection();

        let mut got = vec![];
        while let Some(x) = stream.next() {
            got.push(x.to_vec());
        }
        assert_eq!(got, vec![&b"bar"[..], &b"fubar"[..]]);
    }
}
