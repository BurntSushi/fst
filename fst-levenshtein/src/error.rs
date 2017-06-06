use std::error;
use std::fmt;

/// An error that occurred while building a Levenshtein automaton.
#[derive(Debug)]
pub enum Error {
    /// If construction of the automaton reaches some hard-coded limit
    /// on the number of states, then this error is returned.
    ///
    /// The number given is the limit that was exceeded.
    TooManyStates(usize),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match *self {
            TooManyStates(size_limit) => {
                write!(f, "Levenshtein automaton exceeds size limit of \
                           {} states", size_limit)
            }
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;
        match *self {
            TooManyStates(_) => "levenshtein automaton has too many states",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
