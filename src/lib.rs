extern crate byteorder;
extern crate memchr;
extern crate memmap;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;
extern crate regex_syntax;
extern crate utf8_ranges;

pub use automaton::Automaton;
pub use error::{Error, Result};
pub use levenshtein::Error as LevenshteinError;
pub use levenshtein::Levenshtein;
pub use map::{Map, MapBuilder};
pub use regex::Error as RegexError;
pub use regex::Regex;
pub use set::{Set, SetBuilder};
pub use stream::{IntoStreamer, Streamer};

mod automaton;
mod error;
mod levenshtein;
#[path = "map.rs"]
mod inner_map;
pub mod raw;
mod regex;
#[path = "set.rs"]
mod inner_set;
mod stream;

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
