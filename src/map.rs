use std::fmt;
use std::io;
use std::iter::{self, FromIterator};

use crate::automaton::{AlwaysMatch, Automaton};
use crate::raw;
pub use crate::raw::IndexedValue;
use crate::stream::{IntoStreamer, Streamer};
use crate::Result;

/// Map is a lexicographically ordered map from byte strings to integers.
///
/// A `Map` is constructed with the `MapBuilder` type. Alternatively, a `Map`
/// can be constructed in memory from a lexicographically ordered iterator
/// of key-value pairs (`Map::from_iter`).
///
/// A key feature of `Map` is that it can be serialized to disk compactly. Its
/// underlying representation is built such that the `Map` can be memory mapped
/// and searched without necessarily loading the entire map into memory.
///
/// It supports most common operations associated with maps, such as key
/// lookup and search. It also supports set operations on its keys along with
/// the ability to specify how conflicting values are merged together. Maps
/// also support range queries and automata based searches (e.g. a regular
/// expression).
///
/// Maps are represented by a finite state transducer where inputs are the keys
/// and outputs are the values. As such, maps have the following invariants:
///
/// 1. Once constructed, a `Map` can never be modified.
/// 2. Maps must be constructed with lexicographically ordered byte sequences.
///    There is no restricting on the ordering of values.
///
/// # Differences with sets
///
/// Maps and sets are represented by the same underlying data structure: the
/// finite state transducer. The principal difference between them is that
/// sets always have their output values set to `0`. This has an impact on the
/// representation size and is reflected in the type system for convenience.
/// A secondary but subtle difference is that duplicate values can be added
/// to a set, but it is an error to do so with maps. That is, a set can have
/// the same key added sequentially, but a map can't.
///
/// # The future
///
/// It is regrettable that the output value is fixed to `u64`. Indeed, it is
/// not necessary, but it was a major simplification in the implementation.
/// In the future, the value type may become generic to an extent (outputs must
/// satisfy a basic algebra).
///
/// Keys will always be byte strings; however, we may grow more conveniences
/// around dealing with them (such as a serialization/deserialization step,
/// although it isn't clear where exactly this should live).
#[derive(Clone)]
pub struct Map<D>(raw::Fst<D>);

impl Map<Vec<u8>> {
    /// Create a `Map` from an iterator of lexicographically ordered byte
    /// strings and associated values.
    ///
    /// If the iterator does not yield unique keys in lexicographic order, then
    /// an error is returned.
    ///
    /// Note that this is a convenience function to build a map in memory.
    /// To build a map that streams to an arbitrary `io::Write`, use
    /// `MapBuilder`.
    pub fn from_iter<K, I>(iter: I) -> Result<Map<Vec<u8>>>
    where
        K: AsRef<[u8]>,
        I: IntoIterator<Item = (K, u64)>,
    {
        let mut builder = MapBuilder::memory();
        builder.extend_iter(iter)?;
        Map::new(builder.into_inner()?)
    }
}

impl<D: AsRef<[u8]>> Map<D> {
    /// Creates a map from its representation as a raw byte sequence.
    ///
    /// This accepts anything that can be cheaply converted to a `&[u8]`. The
    /// caller is responsible for guaranteeing that the given bytes refer to
    /// a valid FST. While memory safety will not be violated by invalid input,
    /// a panic could occur while reading the FST at any point.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use fst::Map;
    ///
    /// // File written from a build script using MapBuilder.
    /// # const IGNORE: &str = stringify! {
    /// static FST: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/map.fst"));
    /// # };
    /// # static FST: &[u8] = &[];
    ///
    /// let map = Map::new(FST).unwrap();
    /// ```
    pub fn new(data: D) -> Result<Map<D>> {
        raw::Fst::new(data).map(Map)
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
    /// assert_eq!(map.contains_key("b"), true);
    /// assert_eq!(map.contains_key("z"), false);
    /// ```
    pub fn contains_key<K: AsRef<[u8]>>(&self, key: K) -> bool {
        self.0.contains_key(key)
    }

    /// Retrieves the value associated with a key.
    ///
    /// If the key does not exist, then `None` is returned.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::Map;
    ///
    /// let map = Map::from_iter(vec![("a", 1), ("b", 2), ("c", 3)]).unwrap();
    ///
    /// assert_eq!(map.get("b"), Some(2));
    /// assert_eq!(map.get("z"), None);
    /// ```
    pub fn get<K: AsRef<[u8]>>(&self, key: K) -> Option<u64> {
        self.0.get(key).map(|output| output.value())
    }

    /// Return a lexicographically ordered stream of all key-value pairs in
    /// this map.
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
    /// use fst::{IntoStreamer, Streamer, Map};
    ///
    /// let map = Map::from_iter(vec![("a", 1), ("b", 2), ("c", 3)]).unwrap();
    /// let mut stream = map.stream();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, v)) = stream.next() {
    ///     kvs.push((k.to_vec(), v));
    /// }
    /// assert_eq!(kvs, vec![
    ///     (b"a".to_vec(), 1),
    ///     (b"b".to_vec(), 2),
    ///     (b"c".to_vec(), 3),
    /// ]);
    /// ```
    #[inline]
    pub fn stream(&self) -> Stream<'_> {
        Stream(self.0.stream())
    }

    /// Return a lexicographically ordered stream of all keys in this map.
    ///
    /// Memory requirements are the same as described on `Map::stream`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Map};
    ///
    /// let map = Map::from_iter(vec![("a", 1), ("b", 2), ("c", 3)]).unwrap();
    /// let mut stream = map.keys();
    ///
    /// let mut keys = vec![];
    /// while let Some(k) = stream.next() {
    ///     keys.push(k.to_vec());
    /// }
    /// assert_eq!(keys, vec![b"a", b"b", b"c"]);
    /// ```
    #[inline]
    pub fn keys(&self) -> Keys<'_> {
        Keys(self.0.stream())
    }

    /// Return a stream of all values in this map ordered lexicographically
    /// by each value's corresponding key.
    ///
    /// Memory requirements are the same as described on `Map::stream`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Map};
    ///
    /// let map = Map::from_iter(vec![("a", 1), ("b", 2), ("c", 3)]).unwrap();
    /// let mut stream = map.values();
    ///
    /// let mut values = vec![];
    /// while let Some(v) = stream.next() {
    ///     values.push(v);
    /// }
    /// assert_eq!(values, vec![1, 2, 3]);
    /// ```
    #[inline]
    pub fn values(&self) -> Values<'_> {
        Values(self.0.stream())
    }

    /// Return a builder for range queries.
    ///
    /// A range query returns a subset of key-value pairs in this map in a
    /// range given in lexicographic order.
    ///
    /// Memory requirements are the same as described on `Map::stream`.
    /// Notably, only the keys in the range are read; keys outside the range
    /// are not.
    ///
    /// # Example
    ///
    /// Returns only the key-value pairs in the range given.
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Map};
    ///
    /// let map = Map::from_iter(vec![
    ///     ("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5),
    /// ]).unwrap();
    /// let mut stream = map.range().ge("b").lt("e").into_stream();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, v)) = stream.next() {
    ///     kvs.push((k.to_vec(), v));
    /// }
    /// assert_eq!(kvs, vec![
    ///     (b"b".to_vec(), 2),
    ///     (b"c".to_vec(), 3),
    ///     (b"d".to_vec(), 4),
    /// ]);
    /// ```
    #[inline]
    pub fn range(&self) -> StreamBuilder<'_> {
        StreamBuilder(self.0.range())
    }

    /// Executes an automaton on the keys of this map.
    ///
    /// Note that this returns a `StreamBuilder`, which can be used to
    /// add a range query to the search (see the `range` method).
    ///
    /// Memory requirements are the same as described on `Map::stream`.
    ///
    /// # Example
    ///
    /// An implementation of regular expressions for `Automaton` is available
    /// in the `regex-automata` crate with the `fst1` feature enabled, which
    /// can be used to search maps.
    ///
    /// # Example
    ///
    /// An implementation of subsequence search for `Automaton` can be used
    /// to search maps:
    ///
    /// ```rust
    /// use fst::automaton::Subsequence;
    /// use fst::{IntoStreamer, Streamer, Map};
    ///
    /// # fn main() { example().unwrap(); }
    /// fn example() -> Result<(), Box<dyn std::error::Error>> {
    ///     let map = Map::from_iter(vec![
    ///         ("a foo bar", 1),
    ///         ("foo", 2),
    ///         ("foo1", 3),
    ///         ("foo2", 4),
    ///         ("foo3", 5),
    ///         ("foobar", 6),
    ///     ]).unwrap();
    ///
    ///     let matcher = Subsequence::new("for");
    ///     let mut stream = map.search(&matcher).into_stream();
    ///
    ///     let mut kvs = vec![];
    ///     while let Some((k, v)) = stream.next() {
    ///         kvs.push((String::from_utf8(k.to_vec())?, v));
    ///     }
    ///     assert_eq!(kvs, vec![
    ///         ("a foo bar".to_string(), 1), ("foobar".to_string(), 6),
    ///     ]);
    ///
    ///     Ok(())
    /// }
    /// ```
    pub fn search<A: Automaton>(&self, aut: A) -> StreamBuilder<'_, A> {
        StreamBuilder(self.0.search(aut))
    }

    /// Executes an automaton on the keys of this map and yields matching
    /// keys along with the corresponding matching states in the given
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
to search maps:

```rust
use fst::automaton::Levenshtein;
use fst::{IntoStreamer, Streamer, Map};

# fn main() { example().unwrap(); }
fn example() -> Result<(), Box<dyn std::error::Error>> {
    let map = Map::from_iter(vec![
        ("foo", 1),
        ("foob", 2),
        ("foobar", 3),
        ("fozb", 4),
    ]).unwrap();

    let query = Levenshtein::new("foo", 2)?;
    let mut stream = map.search_with_state(&query).into_stream();

    let mut kvs = vec![];
    while let Some((k, v, s)) = stream.next() {
        kvs.push((String::from_utf8(k.to_vec())?, v, s));
    }
    // Currently, there isn't much interesting that you can do with the states.
    assert_eq!(kvs, vec![
        ("foo".to_string(), 1, Some(183)),
        ("foob".to_string(), 2, Some(123)),
        ("fozb".to_string(), 4, Some(83)),
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

    /// Returns the number of elements in this map.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if and only if this map is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates a new map operation with this map added to it.
    ///
    /// The `OpBuilder` type can be used to add additional map streams
    /// and perform set operations like union, intersection, difference and
    /// symmetric difference on the keys of the map. These set operations also
    /// allow one to specify how conflicting values are merged in the stream.
    ///
    /// # Example
    ///
    /// This example demonstrates a union on multiple map streams. Notice that
    /// the stream returned from the union is not a sequence of key-value
    /// pairs, but rather a sequence of keys associated with one or more
    /// values. Namely, a key is associated with each value associated with
    /// that same key in the all of the streams.
    ///
    /// ```rust
    /// use fst::{Streamer, Map};
    /// use fst::map::IndexedValue;
    ///
    /// let map1 = Map::from_iter(vec![
    ///     ("a", 1), ("b", 2), ("c", 3),
    /// ]).unwrap();
    /// let map2 = Map::from_iter(vec![
    ///     ("a", 10), ("y", 11), ("z", 12),
    /// ]).unwrap();
    ///
    /// let mut union = map1.op().add(&map2).union();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, vs)) = union.next() {
    ///     kvs.push((k.to_vec(), vs.to_vec()));
    /// }
    /// assert_eq!(kvs, vec![
    ///     (b"a".to_vec(), vec![
    ///         IndexedValue { index: 0, value: 1 },
    ///         IndexedValue { index: 1, value: 10 },
    ///     ]),
    ///     (b"b".to_vec(), vec![IndexedValue { index: 0, value: 2 }]),
    ///     (b"c".to_vec(), vec![IndexedValue { index: 0, value: 3 }]),
    ///     (b"y".to_vec(), vec![IndexedValue { index: 1, value: 11 }]),
    ///     (b"z".to_vec(), vec![IndexedValue { index: 1, value: 12 }]),
    /// ]);
    /// ```
    #[inline]
    pub fn op(&self) -> OpBuilder<'_> {
        OpBuilder::new().add(self)
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

    /// Maps the underlying data of the fst Map to another data type.
    ///
    /// # Example
    ///
    /// This example shows that you can map an fst Map based on a `Vec<u8>`
    /// into an fst Map based on a `Cow<[u8]>`, it can also work with a
    /// reference counted type (e.g. `Arc`, `Rc`).
    ///
    /// ```
    /// use std::borrow::Cow;
    ///
    /// use fst::Map;
    ///
    /// let map: Map<Vec<u8>> = Map::from_iter(
    ///     [("hello", 12), ("world", 42)].iter().cloned(),
    /// ).unwrap();
    ///
    /// let map_on_cow: Map<Cow<[u8]>> = map.map_data(Cow::Owned).unwrap();
    /// ```
    #[inline]
    pub fn map_data<F, T>(self, f: F) -> Result<Map<T>>
    where
        F: FnMut(D) -> T,
        T: AsRef<[u8]>,
    {
        self.into_fst().map_data(f).map(Map::from)
    }
}

impl Default for Map<Vec<u8>> {
    #[inline]
    fn default() -> Map<Vec<u8>> {
        Map::from_iter(iter::empty::<(&[u8], u64)>()).unwrap()
    }
}

impl<D: AsRef<[u8]>> fmt::Debug for Map<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Map([")?;
        let mut stream = self.stream();
        let mut first = true;
        while let Some((k, v)) = stream.next() {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "({}, {})", String::from_utf8_lossy(k), v)?;
        }
        write!(f, "])")
    }
}

// Construct a map from an Fst object.
impl<D: AsRef<[u8]>> From<raw::Fst<D>> for Map<D> {
    #[inline]
    fn from(fst: raw::Fst<D>) -> Map<D> {
        Map(fst)
    }
}

/// Returns the underlying finite state transducer.
impl<D: AsRef<[u8]>> AsRef<raw::Fst<D>> for Map<D> {
    #[inline]
    fn as_ref(&self) -> &raw::Fst<D> {
        &self.0
    }
}

impl<'m, 'a, D: AsRef<[u8]>> IntoStreamer<'a> for &'m Map<D> {
    type Item = (&'a [u8], u64);
    type Into = Stream<'m>;

    #[inline]
    fn into_stream(self) -> Stream<'m> {
        Stream(self.0.stream())
    }
}

/// A builder for creating a map.
///
/// This is not your average everyday builder. It has two important qualities
/// that make it a bit unique from what you might expect:
///
/// 1. All keys must be added in lexicographic order. Adding a key out of order
///    will result in an error. Additionally, adding a duplicate key will also
///    result in an error. That is, once a key is associated with a value,
///    that association can never be modified or deleted.
/// 2. The representation of a map is streamed to *any* `io::Write` as it is
///    built. For an in memory representation, this can be a `Vec<u8>`.
///
/// Point (2) is especially important because it means that a map can be
/// constructed *without storing the entire map in memory*. Namely, since it
/// works with any `io::Write`, it can be streamed directly to a file.
///
/// With that said, the builder does use memory, but **memory usage is bounded
/// to a constant size**. The amount of memory used trades off with the
/// compression ratio. Currently, the implementation hard codes this trade off
/// which can result in about 5-20MB of heap usage during construction. (N.B.
/// Guaranteeing a maximal compression ratio requires memory proportional to
/// the size of the map, which defeats some of the benefit of streaming
/// it to disk. In practice, a small bounded amount of memory achieves
/// close-to-minimal compression ratios.)
///
/// The algorithmic complexity of map construction is `O(n)` where `n` is the
/// number of elements added to the map.
///
/// # Example: build in memory
///
/// This shows how to use the builder to construct a map in memory. Note that
/// `Map::from_iter` provides a convenience function that achieves this same
/// goal without needing to explicitly use `MapBuilder`.
///
/// ```rust
/// use fst::{IntoStreamer, Streamer, Map, MapBuilder};
///
/// let mut build = MapBuilder::memory();
/// build.insert("bruce", 1).unwrap();
/// build.insert("clarence", 2).unwrap();
/// build.insert("stevie", 3).unwrap();
///
/// // You could also call `finish()` here, but since we're building the map in
/// // memory, there would be no way to get the `Vec<u8>` back.
/// let bytes = build.into_inner().unwrap();
///
/// // At this point, the map has been constructed, but here's how to read it.
/// let map = Map::new(bytes).unwrap();
/// let mut stream = map.into_stream();
/// let mut kvs = vec![];
/// while let Some((k, v)) = stream.next() {
///     kvs.push((k.to_vec(), v));
/// }
/// assert_eq!(kvs, vec![
///     (b"bruce".to_vec(), 1),
///     (b"clarence".to_vec(), 2),
///     (b"stevie".to_vec(), 3),
/// ]);
/// ```
///
/// # Example: stream to file
///
/// This shows how to do stream construction of a map to a file.
///
/// ```rust,no_run
/// use std::fs::File;
/// use std::io;
///
/// use fst::{IntoStreamer, Streamer, Map, MapBuilder};
///
/// let mut wtr = io::BufWriter::new(File::create("map.fst").unwrap());
/// let mut build = MapBuilder::new(wtr).unwrap();
/// build.insert("bruce", 1).unwrap();
/// build.insert("clarence", 2).unwrap();
/// build.insert("stevie", 3).unwrap();
///
/// // If you want the writer back, then call `into_inner`. Otherwise, this
/// // will finish construction and call `flush`.
/// build.finish().unwrap();
///
/// // At this point, the map has been constructed, but here's how to read it.
/// // NOTE: Normally, one would memory map a file instead of reading its
/// // entire contents on to the heap.
/// let map = Map::new(std::fs::read("map.fst").unwrap()).unwrap();
/// let mut stream = map.into_stream();
/// let mut kvs = vec![];
/// while let Some((k, v)) = stream.next() {
///     kvs.push((k.to_vec(), v));
/// }
/// assert_eq!(kvs, vec![
///     (b"bruce".to_vec(), 1),
///     (b"clarence".to_vec(), 2),
///     (b"stevie".to_vec(), 3),
/// ]);
/// ```
pub struct MapBuilder<W>(raw::Builder<W>);

impl MapBuilder<Vec<u8>> {
    /// Create a builder that builds a map in memory.
    #[inline]
    pub fn memory() -> MapBuilder<Vec<u8>> {
        MapBuilder(raw::Builder::memory())
    }

    /// Finishes the construction of the map and returns it.
    #[inline]
    pub fn into_map(self) -> Map<Vec<u8>> {
        Map(self.0.into_fst())
    }
}

impl<W: io::Write> MapBuilder<W> {
    /// Create a builder that builds a map by writing it to `wtr` in a
    /// streaming fashion.
    pub fn new(wtr: W) -> Result<MapBuilder<W>> {
        raw::Builder::new_type(wtr, 0).map(MapBuilder)
    }

    /// Insert a new key-value pair into the map.
    ///
    /// Keys must be convertible to byte strings. Values must be a `u64`, which
    /// is a restriction of the current implementation of finite state
    /// transducers. (Values may one day be expanded to other types.)
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
    ///
    /// If a key is inserted that is less than or equal to any previous key
    /// added, then an error is returned. Similarly, if there was a problem
    /// writing to the underlying writer, an error is returned.
    pub fn extend_iter<K, I>(&mut self, iter: I) -> Result<()>
    where
        K: AsRef<[u8]>,
        I: IntoIterator<Item = (K, u64)>,
    {
        self.0.extend_iter(
            iter.into_iter().map(|(k, v)| (k, raw::Output::new(v))),
        )
    }

    /// Calls insert on each item in the stream.
    ///
    /// Note that unlike `extend_iter`, this is not generic on the items in
    /// the stream.
    ///
    /// If a key is inserted that is less than or equal to any previous key
    /// added, then an error is returned. Similarly, if there was a problem
    /// writing to the underlying writer, an error is returned.
    pub fn extend_stream<'f, I, S>(&mut self, stream: I) -> Result<()>
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = (&'a [u8], u64)>,
        S: 'f + for<'a> Streamer<'a, Item = (&'a [u8], u64)>,
    {
        self.0.extend_stream(StreamOutput(stream.into_stream()))
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

    /// Gets a reference to the underlying writer.
    pub fn get_ref(&self) -> &W {
        self.0.get_ref()
    }

    /// Returns the number of bytes written to the underlying writer
    pub fn bytes_written(&self) -> u64 {
        self.0.bytes_written()
    }
}

/// A lexicographically ordered stream of key-value pairs from a map.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct Stream<'m, A = AlwaysMatch>(raw::Stream<'m, A>)
where
    A: Automaton;

impl<'a, 'm, A: Automaton> Streamer<'a> for Stream<'m, A> {
    type Item = (&'a [u8], u64);

    fn next(&'a mut self) -> Option<(&'a [u8], u64)> {
        self.0.next().map(|(key, out)| (key, out.value()))
    }
}

impl<'m, A: Automaton> Stream<'m, A> {
    /// Convert this stream into a vector of byte strings and outputs.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_byte_vec(self) -> Vec<(Vec<u8>, u64)> {
        self.0.into_byte_vec()
    }

    /// Convert this stream into a vector of Unicode strings and outputs.
    ///
    /// If any key is not valid UTF-8, then iteration on the stream is stopped
    /// and a UTF-8 decoding error is returned.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_str_vec(self) -> Result<Vec<(String, u64)>> {
        self.0.into_str_vec()
    }

    /// Convert this stream into a vector of byte strings.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_byte_keys(self) -> Vec<Vec<u8>> {
        self.0.into_byte_keys()
    }

    /// Convert this stream into a vector of Unicode strings.
    ///
    /// If any key is not valid UTF-8, then iteration on the stream is stopped
    /// and a UTF-8 decoding error is returned.
    ///
    /// Note that this creates a new allocation for every key in the stream.
    pub fn into_str_keys(self) -> Result<Vec<String>> {
        self.0.into_str_keys()
    }

    /// Convert this stream into a vector of outputs.
    pub fn into_values(self) -> Vec<u64> {
        self.0.into_values()
    }
}

/// A lexicographically ordered stream of key-value-state triples from a map
/// and an automaton.
///
/// The key-values are from the map while the states are from the automaton.
///
/// The `A` type parameter corresponds to an optional automaton to filter
/// the stream. By default, no filtering is done.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct StreamWithState<'m, A = AlwaysMatch>(raw::StreamWithState<'m, A>)
where
    A: Automaton;

impl<'a, 'm, A: 'a + Automaton> Streamer<'a> for StreamWithState<'m, A>
where
    A::State: Clone,
{
    type Item = (&'a [u8], u64, A::State);

    fn next(&'a mut self) -> Option<(&'a [u8], u64, A::State)> {
        self.0.next().map(|(key, out, state)| (key, out.value(), state))
    }
}

/// A lexicographically ordered stream of keys from a map.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct Keys<'m>(raw::Stream<'m>);

impl<'a, 'm> Streamer<'a> for Keys<'m> {
    type Item = &'a [u8];

    #[inline]
    fn next(&'a mut self) -> Option<&'a [u8]> {
        self.0.next().map(|(key, _)| key)
    }
}

/// A stream of values from a map, lexicographically ordered by each value's
/// corresponding key.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct Values<'m>(raw::Stream<'m>);

impl<'a, 'm> Streamer<'a> for Values<'m> {
    type Item = u64;

    #[inline]
    fn next(&'a mut self) -> Option<u64> {
        self.0.next().map(|(_, out)| out.value())
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
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct StreamBuilder<'m, A = AlwaysMatch>(raw::StreamBuilder<'m, A>);

impl<'m, A: Automaton> StreamBuilder<'m, A> {
    /// Specify a greater-than-or-equal-to bound.
    pub fn ge<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'m, A> {
        StreamBuilder(self.0.ge(bound))
    }

    /// Specify a greater-than bound.
    pub fn gt<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'m, A> {
        StreamBuilder(self.0.gt(bound))
    }

    /// Specify a less-than-or-equal-to bound.
    pub fn le<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'m, A> {
        StreamBuilder(self.0.le(bound))
    }

    /// Specify a less-than bound.
    pub fn lt<T: AsRef<[u8]>>(self, bound: T) -> StreamBuilder<'m, A> {
        StreamBuilder(self.0.lt(bound))
    }
}

impl<'m, 'a, A: Automaton> IntoStreamer<'a> for StreamBuilder<'m, A> {
    type Item = (&'a [u8], u64);
    type Into = Stream<'m, A>;

    fn into_stream(self) -> Stream<'m, A> {
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
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct StreamWithStateBuilder<'m, A = AlwaysMatch>(
    raw::StreamWithStateBuilder<'m, A>,
);

impl<'m, A: Automaton> StreamWithStateBuilder<'m, A> {
    /// Specify a greater-than-or-equal-to bound.
    pub fn ge<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'m, A> {
        StreamWithStateBuilder(self.0.ge(bound))
    }

    /// Specify a greater-than bound.
    pub fn gt<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'m, A> {
        StreamWithStateBuilder(self.0.gt(bound))
    }

    /// Specify a less-than-or-equal-to bound.
    pub fn le<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'m, A> {
        StreamWithStateBuilder(self.0.le(bound))
    }

    /// Specify a less-than bound.
    pub fn lt<T: AsRef<[u8]>>(
        self,
        bound: T,
    ) -> StreamWithStateBuilder<'m, A> {
        StreamWithStateBuilder(self.0.lt(bound))
    }
}

impl<'m, 'a, A: 'a + Automaton> IntoStreamer<'a>
    for StreamWithStateBuilder<'m, A>
where
    A::State: Clone,
{
    type Item = (&'a [u8], u64, A::State);
    type Into = StreamWithState<'m, A>;

    fn into_stream(self) -> StreamWithState<'m, A> {
        StreamWithState(self.0.into_stream())
    }
}

/// A builder for collecting map streams on which to perform set operations
/// on the keys of maps.
///
/// Set operations include intersection, union, difference and symmetric
/// difference. The result of each set operation is itself a stream that emits
/// pairs of keys and a sequence of each occurrence of that key in the
/// participating streams. This information allows one to perform set
/// operations on maps and customize how conflicting output values are handled.
///
/// All set operations work efficiently on an arbitrary number of
/// streams with memory proportional to the number of streams.
///
/// The algorithmic complexity of all set operations is `O(n1 + n2 + n3 + ...)`
/// where `n1, n2, n3, ...` correspond to the number of elements in each
/// stream.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying set.
pub struct OpBuilder<'m>(raw::OpBuilder<'m>);

impl<'m> OpBuilder<'m> {
    /// Create a new set operation builder.
    #[inline]
    pub fn new() -> OpBuilder<'m> {
        OpBuilder(raw::OpBuilder::new())
    }

    /// Add a stream to this set operation.
    ///
    /// This is useful for a chaining style pattern, e.g.,
    /// `builder.add(stream1).add(stream2).union()`.
    ///
    /// The stream must emit a lexicographically ordered sequence of key-value
    /// pairs.
    pub fn add<I, S>(mut self, streamable: I) -> OpBuilder<'m>
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = (&'a [u8], u64)>,
        S: 'm + for<'a> Streamer<'a, Item = (&'a [u8], u64)>,
    {
        self.push(streamable);
        self
    }

    /// Add a stream to this set operation.
    ///
    /// The stream must emit a lexicographically ordered sequence of key-value
    /// pairs.
    pub fn push<I, S>(&mut self, streamable: I)
    where
        I: for<'a> IntoStreamer<'a, Into = S, Item = (&'a [u8], u64)>,
        S: 'm + for<'a> Streamer<'a, Item = (&'a [u8], u64)>,
    {
        self.0.push(StreamOutput(streamable.into_stream()));
    }

    /// Performs a union operation on all streams that have been added.
    ///
    /// Note that this returns a stream of `(&[u8], &[IndexedValue])`. The
    /// first element of the tuple is the byte string key. The second element
    /// of the tuple is a list of all occurrences of that key in participating
    /// streams. The `IndexedValue` contains an index and the value associated
    /// with that key in that stream. The index uniquely identifies each
    /// stream, which is an integer that is auto-incremented when a stream
    /// is added to this operation (starting at `0`).
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Map};
    /// use fst::map::IndexedValue;
    ///
    /// let map1 = Map::from_iter(vec![
    ///     ("a", 1), ("b", 2), ("c", 3),
    /// ]).unwrap();
    /// let map2 = Map::from_iter(vec![
    ///     ("a", 11), ("y", 12), ("z", 13),
    /// ]).unwrap();
    ///
    /// let mut union = map1.op().add(&map2).union();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, vs)) = union.next() {
    ///     kvs.push((k.to_vec(), vs.to_vec()));
    /// }
    /// assert_eq!(kvs, vec![
    ///     (b"a".to_vec(), vec![
    ///         IndexedValue { index: 0, value: 1 },
    ///         IndexedValue { index: 1, value: 11 },
    ///     ]),
    ///     (b"b".to_vec(), vec![IndexedValue { index: 0, value: 2 }]),
    ///     (b"c".to_vec(), vec![IndexedValue { index: 0, value: 3 }]),
    ///     (b"y".to_vec(), vec![IndexedValue { index: 1, value: 12 }]),
    ///     (b"z".to_vec(), vec![IndexedValue { index: 1, value: 13 }]),
    /// ]);
    /// ```
    #[inline]
    pub fn union(self) -> Union<'m> {
        Union(self.0.union())
    }

    /// Performs an intersection operation on all streams that have been added.
    ///
    /// Note that this returns a stream of `(&[u8], &[IndexedValue])`. The
    /// first element of the tuple is the byte string key. The second element
    /// of the tuple is a list of all occurrences of that key in participating
    /// streams. The `IndexedValue` contains an index and the value associated
    /// with that key in that stream. The index uniquely identifies each
    /// stream, which is an integer that is auto-incremented when a stream
    /// is added to this operation (starting at `0`).
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Map};
    /// use fst::map::IndexedValue;
    ///
    /// let map1 = Map::from_iter(vec![
    ///     ("a", 1), ("b", 2), ("c", 3),
    /// ]).unwrap();
    /// let map2 = Map::from_iter(vec![
    ///     ("a", 11), ("y", 12), ("z", 13),
    /// ]).unwrap();
    ///
    /// let mut intersection = map1.op().add(&map2).intersection();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, vs)) = intersection.next() {
    ///     kvs.push((k.to_vec(), vs.to_vec()));
    /// }
    /// assert_eq!(kvs, vec![
    ///     (b"a".to_vec(), vec![
    ///         IndexedValue { index: 0, value: 1 },
    ///         IndexedValue { index: 1, value: 11 },
    ///     ]),
    /// ]);
    /// ```
    #[inline]
    pub fn intersection(self) -> Intersection<'m> {
        Intersection(self.0.intersection())
    }

    /// Performs a difference operation with respect to the first stream added.
    /// That is, this returns a stream of all elements in the first stream
    /// that don't exist in any other stream that has been added.
    ///
    /// Note that this returns a stream of `(&[u8], &[IndexedValue])`. The
    /// first element of the tuple is the byte string key. The second element
    /// of the tuple is a list of all occurrences of that key in participating
    /// streams. The `IndexedValue` contains an index and the value associated
    /// with that key in that stream. The index uniquely identifies each
    /// stream, which is an integer that is auto-incremented when a stream
    /// is added to this operation (starting at `0`).
    ///
    /// While the interface is the same for all the operations combining multiple
    /// maps, due to the nature of `difference` there's exactly one `IndexValue`
    /// for each yielded value.
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{Streamer, Map};
    /// use fst::map::IndexedValue;
    ///
    /// let map1 = Map::from_iter(vec![
    ///     ("a", 1), ("b", 2), ("c", 3),
    /// ]).unwrap();
    /// let map2 = Map::from_iter(vec![
    ///     ("a", 11), ("y", 12), ("z", 13),
    /// ]).unwrap();
    ///
    /// let mut difference = map1.op().add(&map2).difference();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, vs)) = difference.next() {
    ///     kvs.push((k.to_vec(), vs.to_vec()));
    /// }
    /// assert_eq!(kvs, vec![
    ///     (b"b".to_vec(), vec![IndexedValue { index: 0, value: 2 }]),
    ///     (b"c".to_vec(), vec![IndexedValue { index: 0, value: 3 }]),
    /// ]);
    /// ```
    #[inline]
    pub fn difference(self) -> Difference<'m> {
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
    /// Note that this returns a stream of `(&[u8], &[IndexedValue])`. The
    /// first element of the tuple is the byte string key. The second element
    /// of the tuple is a list of all occurrences of that key in participating
    /// streams. The `IndexedValue` contains an index and the value associated
    /// with that key in that stream. The index uniquely identifies each
    /// stream, which is an integer that is auto-incremented when a stream
    /// is added to this operation (starting at `0`).
    ///
    /// # Example
    ///
    /// ```rust
    /// use fst::{IntoStreamer, Streamer, Map};
    /// use fst::map::IndexedValue;
    ///
    /// let map1 = Map::from_iter(vec![
    ///     ("a", 1), ("b", 2), ("c", 3),
    /// ]).unwrap();
    /// let map2 = Map::from_iter(vec![
    ///     ("a", 11), ("y", 12), ("z", 13),
    /// ]).unwrap();
    ///
    /// let mut sym_difference = map1.op().add(&map2).symmetric_difference();
    ///
    /// let mut kvs = vec![];
    /// while let Some((k, vs)) = sym_difference.next() {
    ///     kvs.push((k.to_vec(), vs.to_vec()));
    /// }
    /// assert_eq!(kvs, vec![
    ///     (b"b".to_vec(), vec![IndexedValue { index: 0, value: 2 }]),
    ///     (b"c".to_vec(), vec![IndexedValue { index: 0, value: 3 }]),
    ///     (b"y".to_vec(), vec![IndexedValue { index: 1, value: 12 }]),
    ///     (b"z".to_vec(), vec![IndexedValue { index: 1, value: 13 }]),
    /// ]);
    /// ```
    #[inline]
    pub fn symmetric_difference(self) -> SymmetricDifference<'m> {
        SymmetricDifference(self.0.symmetric_difference())
    }
}

impl<'f, I, S> Extend<I> for OpBuilder<'f>
where
    I: for<'a> IntoStreamer<'a, Into = S, Item = (&'a [u8], u64)>,
    S: 'f + for<'a> Streamer<'a, Item = (&'a [u8], u64)>,
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
    I: for<'a> IntoStreamer<'a, Into = S, Item = (&'a [u8], u64)>,
    S: 'f + for<'a> Streamer<'a, Item = (&'a [u8], u64)>,
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

/// A stream of set union over multiple map streams in lexicographic order.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct Union<'m>(raw::Union<'m>);

impl<'a, 'm> Streamer<'a> for Union<'m> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    #[inline]
    fn next(&'a mut self) -> Option<(&'a [u8], &'a [IndexedValue])> {
        self.0.next()
    }
}

/// A stream of set intersection over multiple map streams in lexicographic
/// order.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct Intersection<'m>(raw::Intersection<'m>);

impl<'a, 'm> Streamer<'a> for Intersection<'m> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    #[inline]
    fn next(&'a mut self) -> Option<(&'a [u8], &'a [IndexedValue])> {
        self.0.next()
    }
}

/// A stream of set difference over multiple map streams in lexicographic
/// order.
///
/// The difference operation is taken with respect to the first stream and the
/// rest of the streams. i.e., All elements in the first stream that do not
/// appear in any other streams.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct Difference<'m>(raw::Difference<'m>);

impl<'a, 'm> Streamer<'a> for Difference<'m> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    #[inline]
    fn next(&'a mut self) -> Option<(&'a [u8], &'a [IndexedValue])> {
        self.0.next()
    }
}

/// A stream of set symmetric difference over multiple map streams in
/// lexicographic order.
///
/// The `'m` lifetime parameter refers to the lifetime of the underlying map.
pub struct SymmetricDifference<'m>(raw::SymmetricDifference<'m>);

impl<'a, 'm> Streamer<'a> for SymmetricDifference<'m> {
    type Item = (&'a [u8], &'a [IndexedValue]);

    #[inline]
    fn next(&'a mut self) -> Option<(&'a [u8], &'a [IndexedValue])> {
        self.0.next()
    }
}

/// A specialized stream for mapping map streams (`(&[u8], u64)`) to streams
/// used by raw fsts (`(&[u8], Output)`).
///
/// If this were iterators, we could use `iter::Map`, but doing this on streams
/// requires HKT, so we need to write out the monomorphization ourselves.
struct StreamOutput<S>(S);

impl<'a, S> Streamer<'a> for StreamOutput<S>
where
    S: Streamer<'a, Item = (&'a [u8], u64)>,
{
    type Item = (&'a [u8], raw::Output);

    fn next(&'a mut self) -> Option<(&'a [u8], raw::Output)> {
        self.0.next().map(|(k, v)| (k, raw::Output::new(v)))
    }
}
