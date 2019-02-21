// Copyright 2015 2016 2017 2018 Andrew Gallant
// Copyright 2019 Paul Masurel
//
//! This is a fork over Andrew Gallant `fst` crate.
#![warn(missing_docs)]

extern crate byteorder;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

pub use automaton::Automaton;
pub use error::{Error, Result};
pub use map::{Map, MapBuilder};
pub use stream::{IntoStreamer, Streamer};

mod regex;

pub use self::regex::Regex;

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
