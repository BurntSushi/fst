#![allow(dead_code, unused_mut, unused_variables)]

extern crate byteorder;
extern crate memmap;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

pub use error::{Error, Result};

mod error;
pub mod fst;
