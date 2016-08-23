use std::ptr;
use std::slice;

use fst::{IntoStreamer, Streamer};
use fst::raw as fst;
use libc;

use error::{Error, ErrorKind};

ffi_fn! {
    fn fst_free(fst: *mut fst::Fst) {
        unsafe { Box::from_raw(fst); }
    }
}

ffi_fn! {
    fn fst_get(
        fst: *mut fst::Fst,
        key: *const u8,
        key_len: libc::size_t,
        value: *mut libc::uint64_t,
    ) -> bool {
        let fst = unsafe { &mut *fst };
        let key = unsafe { slice::from_raw_parts(key, key_len) };
        match fst.get(key) {
            None => false,
            Some(output) => {
                unsafe {
                    *value = output.value();
                }
                true
            }
        }
    }
}

ffi_fn! {
    fn fst_contains_key(
        fst: *mut fst::Fst,
        key: *const u8,
        key_len: libc::size_t,
    ) -> bool {
        let fst = unsafe { &mut *fst };
        let key = unsafe { slice::from_raw_parts(key, key_len) };
        fst.contains_key(key)
    }
}

ffi_fn! {
    fn fst_stream_new(fst: *mut fst::Fst) -> *mut FstStream {
        let fst = unsafe { &mut *fst };
        let aut = Box::new(AlwaysMatch) as Automaton;
        Box::into_raw(Box::new(fst.search(aut).into_stream()))
    }
}

ffi_fn! {
    fn fst_stream_next(
        stream: *mut FstStream,
        key: *mut *const u8,
        key_len: *mut libc::size_t,
        value: *mut libc::uint64_t,
    ) -> bool {
        let stream = unsafe { &mut *stream };
        match stream.next() {
            None => false,
            Some((k, v)) => {
                unsafe {
                    *key = k.as_ptr();
                    *key_len = k.len();
                    *value = v.value();
                }
                true
            }
        }
    }
}

ffi_fn! {
    fn fst_stream_free(stream: *mut FstStream) {
        unsafe { Box::from_raw(stream); }
    }
}

ffi_fn! {
    fn fst_len(fst: *mut fst::Fst) -> libc::size_t {
        let fst = unsafe { &mut *fst };
        fst.len()
    }
}

ffi_fn! {
    fn fst_size(fst: *mut fst::Fst) -> libc::size_t {
        let fst = unsafe { &mut *fst };
        fst.size()
    }
}

pub type Automaton = Box<::fst::Automaton<State=Option<usize>>>;

pub type FstStream = fst::Stream<'static, Automaton>;

pub type FstRawBuilderMemory = fst::Builder<Vec<u8>>;

ffi_fn! {
    fn fst_builder_memory_new() -> *mut FstRawBuilderMemory {
        Box::into_raw(Box::new(fst::Builder::memory()))
    }
}

ffi_fn! {
    fn fst_builder_memory_add(
        builder: *mut FstRawBuilderMemory,
        key: *const u8,
        key_len: libc::size_t,
    ) -> bool {
        let builder = unsafe { &mut *builder };
        let key = unsafe { slice::from_raw_parts(key, key_len) };
        builder.add(key).unwrap();
        true
    }
}

ffi_fn! {
    fn fst_builder_memory_finish(
        builder: *mut FstRawBuilderMemory,
        error: *mut Error,
    ) -> *mut fst::Fst {
        let builder = unsafe { *Box::from_raw(builder) };
        match builder.into_inner().and_then(fst::Fst::from_bytes) {
            Ok(fst) => Box::into_raw(Box::new(fst)),
            Err(err) => {
                unsafe {
                    if !error.is_null() {
                        *error = Error::new(ErrorKind::Fst(From::from(err)));
                    }
                    ptr::null_mut()
                }
            }
        }
    }
}

/// An automaton that always matches.
///
/// This is just like fst::automaton::AlwaysMatch, except its state is set to
/// be the same representation as the automata for regexes and Levenshtein.
/// This gives us a uniform representation and makes the C API a bit more
/// palatable.
pub struct AlwaysMatch;

impl ::fst::Automaton for AlwaysMatch {
    type State = Option<usize>;

    fn start(&self) -> Option<usize> { None }
    fn is_match(&self, _: &Option<usize>) -> bool { true }
    fn can_match(&self, _: &Option<usize>) -> bool { true }
    fn will_always_match(&self, _: &Option<usize>) -> bool { true }
    fn accept(&self, _: &Option<usize>, _: u8) -> Option<usize> { None }
}
