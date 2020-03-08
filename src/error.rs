use std::fmt;
use std::io;

use crate::raw;

/// A `Result` type alias for this crate's `Error` type.
pub type Result<T> = std::result::Result<T, Error>;

/// An error that encapsulates all possible errors in this crate.
#[derive(Debug)]
pub enum Error {
    /// An error that occurred while reading or writing a finite state
    /// transducer.
    Fst(raw::Error),
    /// An IO error that occurred while writing a finite state transducer.
    Io(io::Error),
}

impl From<io::Error> for Error {
    #[inline]
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<raw::Error> for Error {
    #[inline]
    fn from(err: raw::Error) -> Error {
        Error::Fst(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::Fst(_) => write!(f, "FST error"),
            Error::Io(_) => write!(f, "I/O error"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            Error::Fst(ref err) => Some(err),
            Error::Io(ref err) => Some(err),
        }
    }
}
