#![allow(dead_code, unused_mut, unused_variables)]

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
pub use set::{Set, SetBuilder, SetStream, SetStreamBuilder};
pub use stream::Stream;

mod automaton;
mod error;
mod map;
pub mod raw;
mod regex;
mod set;
mod stream;
