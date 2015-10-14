#![allow(dead_code, unused_mut, unused_variables)]

extern crate byteorder;
extern crate memchr;
extern crate memmap;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

pub use error::{Error, Result};
pub use set::{Set, SetBuilder, SetStream, SetStreamBuilder};

mod error;
mod map;
pub mod raw;
mod set;
