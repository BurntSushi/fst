use core::fmt;
use core::str;
#[cfg(feature = "alloc")]
use alloc::string::FromUtf8Error;
#[cfg(feature = "alloc")]
use alloc::{vec::Vec, string::String, borrow::ToOwned, format};

use crate::raw::FstType;

/// An error that occurred while using a finite state transducer.
///
/// This enum is non-exhaustive. New variants may be added to it in
/// compatible releases.
pub enum Error {
    /// A version mismatch occurred while reading a finite state transducer.
    ///
    /// This occurs when the API version (of the crate) does not match the
    /// version encoded in the finite state transducer.
    ///
    /// When this error is encountered, there are only two ways to fix it:
    ///
    /// 1. Change the version of the library to one that is compatible with
    ///    the given finite state transducer.
    /// 2. Rebuild the finite state transducer.
    Version {
        /// The expected version, which is hard-coded into the current version
        /// of this crate.
        expected: u64,
        /// The version read from the finite state transducer.
        got: u64,
    },
    /// An unexpected error occurred while reading a finite state transducer.
    /// Usually this occurs because the data is corrupted or is not actually
    /// a finite state transducer serialized by this library.
    Format {
        /// The number of bytes given to the FST constructor.
        size: usize,
    },
    /// An error that is returned if verification of an FST fails because of a
    /// checksum mismatch.
    ChecksumMismatch {
        /// The checksum that was expected.
        expected: u32,
        /// The checksum that was actually computed.
        got: u32,
    },
    /// An error that is returned if the caller attempts to verify an FST
    /// that does not have a checksum, as is the case for all FSTs generated
    /// by this crate before version `0.4`.
    ChecksumMissing,
    /// A duplicate key was inserted into a finite state transducer, which is
    /// not allowed.
    #[cfg(feature = "alloc")]
    DuplicateKey {
        /// The duplicate key.
        got: Vec<u8>,
    },
    /// A key was inserted out of order into a finite state transducer.
    ///
    /// Keys must always be inserted in lexicographic order.
    #[cfg(feature = "alloc")]
    OutOfOrder {
        /// The last key successfully inserted.
        previous: Vec<u8>,
        /// The key that caused this error to occur.
        got: Vec<u8>,
    },
    /// A finite state transducer with an unexpected type was found.
    ///
    /// This is not currently used in this crate, but callers may wish to
    /// employ its use for alternative data structures implemented on top of
    /// finite state transducers.
    WrongType {
        /// The expected finite state transducer type.
        expected: FstType,
        /// The type read from a finite state transducer.
        got: FstType,
    },
    /// An error that occurred when trying to decode a UTF-8 byte key.
    #[cfg(feature = "alloc")]
    FromUtf8(FromUtf8Error),
    /// Hints that destructuring should not be exhaustive.
    ///
    /// This enum may grow additional variants, so this makes sure clients
    /// don't count on exhaustive matching. (Otherwise, adding a new variant
    /// could break existing code.)
    #[doc(hidden)]
    __Nonexhaustive,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            #[cfg(feature = "alloc")]
            Error::FromUtf8(ref err) => err.fmt(f),
            Error::Version { expected, got } => write!(
                f,
                "\
Error opening FST: expected API version {}, got API version {}. \
It looks like the FST you're trying to open is either not an FST file or it \
was generated with a different version of the 'fst' crate. You'll either need \
to change the version of the 'fst' crate you're using, or re-generate the
FST.",
                expected, got
            ),
            Error::Format { size } => write!(
                f,
                "\
Error opening FST with size {} bytes: An unknown error occurred. This \
usually means you're trying to read data that isn't actually an encoded FST.",
                size
            ),
            Error::ChecksumMismatch { expected, got } => write!(
                f,
                "FST verification failed: expected checksum of {} but got {}",
                expected, got,
            ),
            Error::ChecksumMissing => write!(
                f,
                "FST verification failed: FST does not contain a checksum",
            ),
            #[cfg(feature = "alloc")]
            Error::DuplicateKey { ref got } => write!(
                f,
                "Error inserting duplicate key: '{}'.",
                format_bytes(&*got)
            ),
            #[cfg(feature = "alloc")]
            Error::OutOfOrder { ref previous, ref got } => write!(
                f,
                "\
Error inserting out-of-order key: '{}'. (Previous key was '{}'.) Keys must be \
inserted in lexicographic order.",
                format_bytes(&*got),
                format_bytes(&*previous)
            ),
            Error::WrongType { expected, got } => write!(
                f,
                "\
Error opening FST: expected type '{}', got type '{}'.",
                expected, got
            ),
            Error::__Nonexhaustive => unreachable!(),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self {
            #[cfg(feature = "alloc")]
            Error::FromUtf8(ref err) => Some(err),
            _ => None,
        }
    }
}

#[cfg(not(feature = "std"))]
impl core::error::Error for Error {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match *self {
            #[cfg(feature = "alloc")]
            Error::FromUtf8(ref err) => Some(err),
            _ => None,
        }
    }
}

#[cfg(feature = "alloc")]
impl From<FromUtf8Error> for Error {
    #[inline]
    fn from(err: FromUtf8Error) -> Error {
        Error::FromUtf8(err)
    }
}

/// Attempt to convert an arbitrary byte string to a more convenient display
/// form.
///
/// Essentially, try to decode the bytes as UTF-8 and show that. Failing that,
/// just show the sequence of bytes.
#[cfg(feature = "alloc")]
fn format_bytes(bytes: &[u8]) -> String {
    match str::from_utf8(bytes) {
        Ok(s) => s.to_owned(),
        Err(_) => format!("{:?}", bytes),
    }
}
