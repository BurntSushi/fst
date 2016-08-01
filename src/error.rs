use std::error;
use std::fmt;
use std::io;

use regex_syntax;

use raw;
use {LevenshteinError, RegexError};

/// A `Result` type alias for this crate's `Error` type.
pub type Result<T> = ::std::result::Result<T, Error>;

/// An error that encapsulates all possible errors in this crate.
#[derive(Debug)]
pub enum Error {
    /// An error that occurred while reading or writing a finite state
    /// transducer.
    Fst(raw::Error),
    /// An error that occurred while compiling a regular expression.
    Regex(RegexError),
    /// An error that occurred while building a Levenshtein automaton.
    Levenshtein(LevenshteinError),
    /// An IO error that occurred while writing a finite state transducer.
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<RegexError> for Error {
    fn from(err: RegexError) -> Error {
        Error::Regex(err)
    }
}

impl From<LevenshteinError> for Error {
    fn from(err: LevenshteinError) -> Error {
        Error::Levenshtein(err)
    }
}

impl From<raw::Error> for Error {
    fn from(err: raw::Error) -> Error {
        Error::Fst(err)
    }
}

impl From<regex_syntax::Error> for Error {
    fn from(err: regex_syntax::Error) -> Error {
        Error::Regex(RegexError::Syntax(err))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match *self {
            Fst(ref err) => err.fmt(f),
            Regex(ref err) => err.fmt(f),
            Levenshtein(ref err) => err.fmt(f),
            Io(ref err) => err.fmt(f),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;
        match *self {
            Fst(ref err) => err.description(),
            Regex(ref err) => err.description(),
            Levenshtein(ref err) => err.description(),
            Io(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        use self::Error::*;
        match *self {
            Fst(ref err) => Some(err),
            Regex(ref err) => Some(err),
            Levenshtein(ref err) => Some(err),
            Io(ref err) => Some(err),
        }
    }
}
