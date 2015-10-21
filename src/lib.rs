extern crate byteorder;
extern crate memchr;
extern crate memmap;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;
extern crate regex_syntax;
extern crate utf8_ranges;

pub use automaton::Automaton;
pub use error::{Error, Result};
pub use regex::Error as RegexError;
pub use regex::Regex;
pub use set::{Set, SetBuilder, SetOp};
pub use stream::{IntoStream, Stream};

mod automaton;
mod error;
mod map;
pub mod raw;
mod regex;
#[path = "set.rs"]
mod inner_set;
mod stream;

/// Set operations implemented by finite state transducers.
///
/// This API provided by this sub-module is close in spirit to the API
/// provided by
/// [`std::collections::BTreeSet`](http://doc.rust-lang.org/stable/std/collections/struct.BTreeSet.html).
/// The principle difference, as with everything else in this crate, is
/// that operations are performed on streams instead of iterators. Another
/// difference is that most of the set operations (union, intersection,
/// difference and symmetric difference) work on multiple sets at the same
/// time, instead of two.
///
/// # Overview of types
///
/// `Set` is a read only interface to pre-constructed sets. `SetBuilder` is
/// used to create new sets. (Once a set is created, it can never be modified.)
/// `SetStream` is a stream of values that originated from a set (analogous to
/// an iterator). `SetStreamBuilder` builds range queries. `SetOp` collects
/// a set of streams and executes set operations like `union` or `intersection`
/// on them. The rest of the types are streams for set operations.
pub mod set {
    pub use inner_set::*;
}
