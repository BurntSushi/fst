// Copyright 2015 2016 2017 2018 Andrew Gallant
// Copyright 2019 Paul Masurel
//
// This is a fork over Andrew Gallant `fst` crate.
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
#![warn(missing_docs)]

extern crate byteorder;
#[cfg(test)] extern crate fst_regex;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

pub use automaton::Automaton;
pub use error::{Error, Result};
pub use map::{Map, MapBuilder};
pub use stream::{IntoStreamer, Streamer};

#[path = "automaton/mod.rs"]
mod inner_automaton;
mod error;
#[path = "map.rs"]
mod inner_map;
pub mod raw;
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
