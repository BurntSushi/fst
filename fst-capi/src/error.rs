use std::ffi::CString;
use std::fmt;
use std::str;

use fst;
use libc;

#[derive(Debug)]
pub struct Error {
    message: Option<CString>,
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    None,
    Str(str::Utf8Error),
    Fst(fst::Error),
}

impl Error {
    pub fn new(kind: ErrorKind) -> Error {
        Error {
            message: None,
            kind: kind,
        }
    }

    pub fn is_err(&self) -> bool {
        match self.kind {
            ErrorKind::None => false,
            ErrorKind::Str(_) | ErrorKind::Fst(_) => true,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::None => write!(f, "no error"),
            ErrorKind::Str(ref e) => e.fmt(f),
            ErrorKind::Fst(ref e) => e.fmt(f),
        }
    }
}

ffi_fn! {
    fn fst_error_new() -> *mut Error {
        Box::into_raw(Box::new(Error::new(ErrorKind::None)))
    }
}

ffi_fn! {
    fn fst_error_free(err: *mut Error) {
        unsafe { Box::from_raw(err); }
    }
}

ffi_fn! {
    fn fst_error_message(err: *mut Error) -> *const libc::c_char {
        let err = unsafe { &mut *err };
        let cmsg = match CString::new(format!("{}", err)) {
            Ok(msg) => msg,
            Err(err) => {
                // A weird case. Just show as much as we can, I guess.
                let nul = err.nul_position();
                let msg = err.into_vec();
                CString::new(msg[0..nul].to_owned()).unwrap()
            }
        };
        let p = cmsg.as_ptr();
        err.message = Some(cmsg);
        p
    }
}
