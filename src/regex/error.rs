use std::error;
use std::fmt;

use regex_syntax;

#[derive(Debug)]
pub enum Error {
    Syntax(regex_syntax::Error),
    CompiledTooBig(usize),
    NoLazy,
    NoWordBoundary,
    NoEmpty,
}

impl From<regex_syntax::Error> for Error {
    fn from(err: regex_syntax::Error) -> Error {
        Error::Syntax(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match *self {
            Syntax(ref err) => err.fmt(f),
            CompiledTooBig(size_limit) => {
                write!(f, "Compiled regex exceeds size limit of {} bytes",
                       size_limit)
            }
            NoLazy => write!(f, "Lazy reptition operators such as '+?' are \
                                 not allowed."),
            NoWordBoundary => write!(f, "Word boundary operators are not \
                                           allowed."),
            NoEmpty => write!(f, "Empty match operators are not allowed \
                                  (hopefully temporary)."),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;
        match *self {
            Syntax(ref err) => err.description(),
            CompiledTooBig(_) => "compiled regex is too big",
            NoLazy => "lazy repetition operators are not allowed",
            NoWordBoundary => "word boundary operators are not allowed",
            NoEmpty => "empty match operators are not allowed",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        use self::Error::*;
        match *self {
            Syntax(ref err) => Some(err),
            _ => None,
        }
    }
}
