use std::error;
use std::fmt;
use std::io;
use std::str;

use byteorder;

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Version { expected: u64, got: u64 },
    Format,
    Value { got: u64 },
    DuplicateKey { got: Vec<u8> },
    OutOfOrder { got: Vec<u8> },
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<byteorder::Error> for Error {
    fn from(err: byteorder::Error) -> Error {
        Error::Io(From::from(err))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match *self {
            Io(ref err) => err.fmt(f),
            Version { expected, got } => {
                write!(f, "\
Error opening FST: expected API version {}, got API version {}.
It looks like the FST you're trying to open was generated with a different
version of the 'fst' crate. You'll either need to change the version of the
'fst' crate you're using, or re-generate the FST.", expected, got)
            }
            Format => write!(f, "\
Error opening FST: An unknown error occurred. This usually means you're trying
to read data that isn't actually an encoded FST."),
            Value { got } => write!(f, "\
Invalid value for FST map: {}. The maximum value is `2^64 - 2`. Said
differently, the only invalid value is `2^64 - 1`.", got),
            DuplicateKey { ref got } => write!(f, "\
Error inserting duplicate key: {}.", format_bytes(&*got)),
            OutOfOrder { ref got } => write!(f, "\
Error inserting out-of-order key: {}. Keys must be inserted in lexicographic
order.", format_bytes(&*got)),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;
        match *self {
            Io(ref err) => err.description(),
            Version { .. } => "incompatible version found when opening FST",
            Format => "unknown invalid format found when opening FST",
            Value { .. } => "invalid value",
            DuplicateKey { .. } => "duplicate key insertion",
            OutOfOrder { .. } => "out-of-order key insertion",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Io(ref err) => Some(err),
            _ => None,
        }
    }
}

/// Attempt to convert an arbitrary byte string to a more convenient display
/// form.
///
/// Essentially, try to decode the bytes as UTF-8 and show that. Failing that,
/// just show the sequence of bytes.
fn format_bytes(bytes: &[u8]) -> String {
    match str::from_utf8(bytes) {
        Ok(s) => s.to_owned(),
        Err(_) => format!("{:?}", bytes),
    }
}
