/*!
Crate `fst` is a library for efficiently storing and searching ordered sets or
maps where the keys are byte strings. A key design goal of this crate is to
support storing and searching *very large* sets or maps (i.e., billions). This
means that much effort has gone in to making sure that all operations are
memory efficient.

Sets and maps are represented by a finite state machine, which acts as a form
of compression on common prefixes and suffixes in the keys. Additionally,
finite state machines can be efficiently queried with automata (like regular
expressions or Levenshtein distance for fuzzy queries) or lexicographic ranges.

To read more about the mechanics of finite state transducers, including a
bibliography for algorithms used in this crate, see the docs for the
[`raw::Fst`](raw/struct.Fst.html) type.

# Installation

Simply add a corresponding entry to your `Cargo.toml` dependency list:

```ignore
[dependencies]
fst = "0.2"
```

And add this to your crate root:

```ignore
extern crate fst;
```

The examples in this documentation will show the rest.

# Other crates

The
[`fst-regex`](https://docs.rs/fst-regex)
and
[`fst-levenshtein`](https://docs.rs/fst-levenshtein)
crates provide regular expression matching and fuzzy searching on FSTs,
respectively.

# Overview of types and modules

This crate provides the high level abstractions---namely sets and maps---in the
top-level module.

The `set` and `map` sub-modules contain types specific to sets and maps, such
as range queries and streams.

The `raw` module permits direct interaction with finite state transducers.
Namely, the states and transitions of a transducer can be directly accessed
with the `raw` module.

# Example: fuzzy query

This example shows how to create a set of strings in memory, and then execute
a fuzzy query. Namely, the query looks for all keys within an edit distance
of `1` of `foo`. (Edit distance is the number of character insertions,
deletions or substitutions required to get from one string to another. In this
case, a character is a Unicode codepoint.)

```rust
extern crate fst;
extern crate fst_levenshtein; // the fst-levenshtein crate

use std::error::Error;

use fst::{IntoStreamer, Streamer, Set};
use fst_levenshtein::Levenshtein;

# fn main() { example().unwrap(); }
fn example() -> Result<(), Box<Error>> {
    // A convenient way to create sets in memory.
    let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
    let set = Set::from_iter(keys)?;

    // Build our fuzzy query.
    let lev = Levenshtein::new("foo", 1)?;

    // Apply our fuzzy query to the set we built.
    let mut stream = set.search(lev).into_stream();

    let keys = stream.into_strs()?;
    assert_eq!(keys, vec!["fo", "fob", "foo", "food"]);
    Ok(())
}
```

# Example: stream a map to a file

This shows how to create a `MapBuilder` that will stream construction of the
map to a file. Notably, this will never store the entire transducer in memory.
Instead, only constant memory is required.

```rust,no_run
# fn example() -> Result<(), fst::Error> {
use std::fs::File;
use std::io;

use fst::{IntoStreamer, Streamer, Map, MapBuilder};

// This is where we'll write our map to.
let mut wtr = io::BufWriter::new(File::create("map.fst")?);

// Create a builder that can be used to insert new key-value pairs.
let mut build = MapBuilder::new(wtr)?;
build.insert("bruce", 1).unwrap();
build.insert("clarence", 2).unwrap();
build.insert("stevie", 3).unwrap();

// Finish construction of the map and flush its contents to disk.
build.finish()?;

// At this point, the map has been constructed. Now we'd like to search it.
// This creates a memory map, which enables searching the map without loading
// all of it into memory.
let map = unsafe { Map::from_path("map.fst") }?;

// Query for keys that are greater than or equal to clarence.
let mut stream = map.range().ge("clarence").into_stream();

let kvs = stream.into_str_vec()?;
assert_eq!(kvs, vec![
    ("clarence".to_owned(), 2),
    ("stevie".to_owned(), 3),
]);
# Ok(())
# }
# example().unwrap();
```

# Example: case insensitive search

We can perform case insensitive search on a set using a regular expression.
Note that while sets can store arbitrary byte strings, a regular expression
will only match valid UTF-8 encoded byte strings.

```rust
extern crate fst;
extern crate fst_regex; // the fst-regex crate

use std::error::Error;

use fst::{IntoStreamer, Streamer, Set};
use fst_regex::Regex;

# fn main() { example().unwrap(); }
fn example() -> Result<(), Box<Error>> {
    let set = Set::from_iter(&["FoO", "Foo", "fOO", "foo"])?;

    let re = Regex::new("(?i)foo")?;
    let mut stream = set.search(&re).into_stream();

    let keys = stream.into_strs()?;
    assert_eq!(keys, vec!["FoO", "Foo", "fOO", "foo"]);
    Ok(())
}
```

# Example: searching multiple sets efficiently

Since queries can search a transducer without reading the entire data structure
into memory, it is possible to search *many* transducers very quickly.

This crate provides efficient set/map operations that allow one to combine
multiple streams of search results. Each operation only uses memory
proportional to the number of streams.

The example below shows how to find all keys that have at least one capital
letter that doesn't appear at the beginning of the key. The example below uses
sets, but the same operations are available on maps too.

```rust
extern crate fst;
extern crate fst_regex; // the fst-regex crate

use std::error::Error;

use fst::{Streamer, Set};
use fst::set;
use fst_regex::Regex;

# fn main() { example().unwrap(); }
fn example() -> Result<(), Box<Error>> {
    let set1 = Set::from_iter(&["AC/DC", "Aerosmith"])?;
    let set2 = Set::from_iter(&["Bob Seger", "Bruce Springsteen"])?;
    let set3 = Set::from_iter(&["George Thorogood", "Golden Earring"])?;
    let set4 = Set::from_iter(&["Kansas"])?;
    let set5 = Set::from_iter(&["Metallica"])?;

    // Create the regular expression. We can reuse it to search all of the sets.
    let re = Regex::new(r".+\p{Lu}.*")?;

    // Build a set operation. All we need to do is add a search result stream for
    // each set and ask for the union. (Other operations, like intersection and
    // difference are also available.)
    let mut stream =
        set::OpBuilder::new()
        .add(set1.search(&re))
        .add(set2.search(&re))
        .add(set3.search(&re))
        .add(set4.search(&re))
        .add(set5.search(&re))
        .union();

    // Now collect all of the keys. Alternatively, you could build another set here
    // using `SetBuilder::extend_stream`.
    let mut keys = vec![];
    while let Some(key) = stream.next() {
        keys.push(key.to_vec());
    }
    assert_eq!(keys, vec![
        "AC/DC".as_bytes(),
        "Bob Seger".as_bytes(),
        "Bruce Springsteen".as_bytes(),
        "George Thorogood".as_bytes(),
        "Golden Earring".as_bytes(),
    ]);
    Ok(())
}
```

# Memory usage

An important advantage of using finite state transducers to represent sets and
maps is that they can compress very well depending on the distribution of keys.
The smaller your set/map is, the more likely it is that it will fit into
memory. If it's in memory, then searching it is faster. Therefore, it is
important to do what we can to limit what actually needs to be in memory.

This is where automata shine, because they can be queried in their compressed
state without loading the entire data structure into memory. This means that
one can store a set/map created by this crate on disk and search it without
actually reading the entire set/map into memory. This use case is served well
by *memory maps*, which lets one assign the entire contents of a file to a
contiguous region of virtual memory.

Indeed, this crate encourages this mode of operation. Both sets and maps have
methods for memory mapping a finite state transducer from disk.

This is particularly important for long running processes that use this crate,
since it enables the operating system to determine which regions of your
finite state transducers are actually in memory.

Of course, there are downsides to this approach. Namely, navigating a
transducer during a key lookup or a search will likely follow a pattern
approximating random access. Supporting random access when reading from disk
can be very slow because of how often `seek` must be called (or, in the case
of memory maps, page faults). This is somewhat mitigated by the prevalence of
solid state drives where seek time is eliminated. Nevertheless, solid state
drives are not ubiquitous and it is possible that the OS will not be smart
enough to keep your memory mapped transducers in the page cache. In that case,
it is advisable to load the entire transducer into your process's memory (e.g.,
`Set::from_bytes`).

# Streams

Searching a set or a map needs to provide some way to iterate over the search
results. Idiomatic Rust calls for something satisfying the `Iterator` trait
to be used here. Unfortunately, this is not possible to do efficiently because
the `Iterator` trait does not permit values emitted by the iterator to borrow
from the iterator. Borrowing from the iterator is required in our case because
keys and values are constructed *during iteration*.

Namely, if we were to use iterators, then every key would need its own
allocation, which could be quite costly.

Instead, this crate provides a `Streamer`, which can be thought of as a
streaming iterator. Namely, a stream in this crate maintains a single key
buffer and lends it out on each iteration.

For more details, including important limitations, see the `Streamer` trait.

# Quirks

There's no doubt about it, finite state transducers are a specialty data
structure. They have a host of restrictions that don't apply to other similar
data structures found in the standard library, such as `BTreeSet` and
`BTreeMap`. Here are some of them:

1. Sets can only contain keys that are byte strings.
2. Maps can also only contain keys that are byte strings, and its values are
   limited to unsigned 64 bit integers. (The restriction on values may be
   relaxed some day.)
3. Creating a set or a map requires inserting keys in lexicographic order.
   Often, keys are not already sorted, which can make constructing large
   sets or maps tricky. One way to do it is to sort pieces of the data and
   build a set/map for each piece. This can be parallelized trivially. Once
   done, they can be merged together into one big set/map if desired.
   A somewhat simplistic example of this procedure can be seen in
   `fst-bin/src/merge.rs` from the root of this crate's repository.

# Warning: regexes and Levenshtein automatons use a lot of memory

The construction of automatons for both regular expressions and Levenshtein
automatons should be consider "proof of concept" quality. Namely, they do just
enough to be *correct*. But they haven't had any effort put into them to be
memory conscious. These are important parts of this library, so they will be
improved.

Note that whether you're using regexes or Levenshtein automatons, an error
will be returned if the automaton gets too big (tens of MB in heap usage).
*/

#![deny(missing_docs)]

extern crate byteorder;
#[cfg(test)] extern crate fst_levenshtein;
#[cfg(test)] extern crate fst_regex;
#[cfg(feature = "mmap")] extern crate memmap;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

pub use automaton::Automaton;
pub use error::{Error, Result};
pub use map::{Map, MapBuilder};
pub use set::{Set, SetBuilder};
pub use stream::{IntoStreamer, Streamer};

#[path = "automaton/mod.rs"]
mod inner_automaton;
mod error;
#[path = "map.rs"]
mod inner_map;
pub mod raw;
#[path = "set.rs"]
mod inner_set;
mod stream;

/// Automaton implementations for finite state transducers.
///
/// This module defines a trait, `Automaton`, with several implementations
/// including, but not limited to, union, intersection and complement.
pub mod automaton {
    pub use inner_automaton::*;
}

/// Map operations implemented by finite state transducers.
///
/// This API provided by this sub-module is close in spirit to the API
/// provided by
/// [`std::collections::BTreeMap`](http://doc.rust-lang.org/stable/std/collections/struct.BTreeMap.html).
///
/// # Overview of types
///
/// `Map` is a read only interface to pre-constructed sets. `MapBuilder` is
/// used to create new sets. (Once a set is created, it can never be modified.)
/// `Stream`, `Keys` and `Values` are streams that originated from a map.
/// `StreamBuilder` builds range queries. `OpBuilder` collects a set of streams
/// and executes set operations like `union` or `intersection` on them with the
/// option of specifying a merge strategy for a map's values. The rest of the
/// types are streams for set operations.
pub mod map {
    pub use inner_map::*;
}

/// Set operations implemented by finite state transducers.
///
/// This API provided by this sub-module is close in spirit to the API
/// provided by
/// [`std::collections::BTreeSet`](http://doc.rust-lang.org/stable/std/collections/struct.BTreeSet.html).
/// The principle difference, as with everything else in this crate, is that
/// operations are performed on streams of byte strings instead of generic
/// iterators. Another difference is that most of the set operations (union,
/// intersection, difference and symmetric difference) work on multiple sets at
/// the same time, instead of two.
///
/// # Overview of types
///
/// `Set` is a read only interface to pre-constructed sets. `SetBuilder` is
/// used to create new sets. (Once a set is created, it can never be modified.)
/// `Stream` is a stream of values that originated from a set (analogous to an
/// iterator). `StreamBuilder` builds range queries. `OpBuilder` collects a set
/// of streams and executes set operations like `union` or `intersection` on
/// them. The rest of the types are streams for set operations.
pub mod set {
    pub use inner_set::*;
}
