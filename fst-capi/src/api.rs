use std::ffi::CStr;
use std::ptr;
use std::slice;
use std::str;

use fst::{IntoStreamer, Streamer};
use fst::raw as fst;
use libc;

use error::{Error, ErrorKind};

ffi_fn! {
    fn fst_new_from_bytes(
        buf: *const u8,
        buf_len: libc::size_t,
        error: *mut Error,
    ) -> *mut fst::Fst {
        let buf = unsafe { slice::from_raw_parts(buf, buf_len) };
        match fst::Fst::from_static_slice(buf) {
            Ok(fst) => Box::into_raw(Box::new(fst)),
            Err(err) => {
                unsafe {
                    if !error.is_null() {
                        *error = Error::new(ErrorKind::Fst(From::from(err)));
                    }
                }
                ptr::null_mut()
            }
        }
    }
}

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

ffi_fn! {
    fn fst_is_disjoint(fst: *mut fst::Fst, stream: *mut FstStream) -> bool {
        let fst = unsafe { &mut *fst };
        let stream = unsafe { *Box::from_raw(stream) };
        fst.is_disjoint(stream)
    }
}

ffi_fn! {
    fn fst_is_subset(fst: *mut fst::Fst, stream: *mut FstStream) -> bool {
        let fst = unsafe { &mut *fst };
        let stream = unsafe { *Box::from_raw(stream) };
        fst.is_subset(stream)
    }
}

ffi_fn! {
    fn fst_is_superset(fst: *mut fst::Fst, stream: *mut FstStream) -> bool {
        let fst = unsafe { &mut *fst };
        let stream = unsafe { *Box::from_raw(stream) };
        fst.is_superset(stream)
    }
}

pub type FstStream = fst::Stream<'static, Automaton>;

ffi_fn! {
    fn fst_stream_new(fst: *mut fst::Fst) -> *mut FstStream {
        let fst = unsafe { &mut *fst };
        let aut = Box::new(AlwaysMatch) as Automaton;
        Box::into_raw(Box::new(fst.search(aut).into_stream()))
    }
}

ffi_fn! {
    fn fst_stream_free(stream: *mut FstStream) {
        unsafe { Box::from_raw(stream); }
    }
}

ffi_fn! {
    fn fst_stream_next(
        stream: *mut FstStream,
        key: *mut *const u8,
        key_len: *mut libc::size_t,
        value: *mut u64,
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

pub type FstStreamBuilder = fst::StreamBuilder<'static, Automaton>;

ffi_fn! {
    fn fst_stream_builder_new(
        fst: *mut fst::Fst,
    ) -> *mut FstStreamBuilder {
        let fst = unsafe { &mut *fst };
        let sb = fst.range().automaton(Box::new(AlwaysMatch) as Automaton);
        Box::into_raw(Box::new(sb))
    }
}

ffi_fn! {
    fn fst_stream_builder_finish(
        sb: *mut FstStreamBuilder,
    ) -> *mut FstStream {
        let sb = unsafe { *Box::from_raw(sb) };
        Box::into_raw(Box::new(sb.into_stream()))
    }
}

ffi_fn! {
    fn fst_stream_builder_automaton(
        sb: *mut FstStreamBuilder,
        automaton: *const Automaton,
    ) {
        unsafe {
            let sb_ = ptr::read(sb);
            let aut = ptr::read(automaton);
            ptr::replace(sb, sb_.automaton(aut));
        }
    }
}

ffi_fn! {
    fn fst_stream_builder_ge(
        sb: *mut FstStreamBuilder,
        bound: *const u8,
        bound_len: libc::size_t,
    ) {
        unsafe {
            let sb_ = ptr::read(sb);
            let bound = slice::from_raw_parts(bound, bound_len);
            ptr::replace(sb, sb_.ge(bound));
        }
    }
}

ffi_fn! {
    fn fst_stream_builder_ge_nul(
        sb: *mut FstStreamBuilder,
        bound: *const libc::c_char,
    ) {
        let bound = unsafe { CStr::from_ptr(bound).to_bytes() };
        fst_stream_builder_ge(sb, bound.as_ptr(), bound.len())
    }
}

ffi_fn! {
    fn fst_stream_builder_gt(
        sb: *mut FstStreamBuilder,
        bound: *const u8,
        bound_len: libc::size_t,
    ) {
        unsafe {
            let sb_ = ptr::read(sb);
            let bound = slice::from_raw_parts(bound, bound_len);
            ptr::replace(sb, sb_.gt(bound));
        }
    }
}

ffi_fn! {
    fn fst_stream_builder_gt_nul(
        sb: *mut FstStreamBuilder,
        bound: *const libc::c_char,
    ) {
        let bound = unsafe { CStr::from_ptr(bound).to_bytes() };
        fst_stream_builder_gt(sb, bound.as_ptr(), bound.len())
    }
}

ffi_fn! {
    fn fst_stream_builder_le(
        sb: *mut FstStreamBuilder,
        bound: *const u8,
        bound_len: libc::size_t,
    ) {
        unsafe {
            let sb_ = ptr::read(sb);
            let bound = slice::from_raw_parts(bound, bound_len);
            ptr::replace(sb, sb_.le(bound));
        }
    }
}

ffi_fn! {
    fn fst_stream_builder_le_nul(
        sb: *mut FstStreamBuilder,
        bound: *const libc::c_char,
    ) {
        let bound = unsafe { CStr::from_ptr(bound).to_bytes() };
        fst_stream_builder_le(sb, bound.as_ptr(), bound.len())
    }
}

ffi_fn! {
    fn fst_stream_builder_lt(
        sb: *mut FstStreamBuilder,
        bound: *const u8,
        bound_len: libc::size_t,
    ) {
        unsafe {
            let sb_ = ptr::read(sb);
            let bound = slice::from_raw_parts(bound, bound_len);
            ptr::replace(sb, sb_.lt(bound));
        }
    }
}

ffi_fn! {
    fn fst_stream_builder_lt_nul(
        sb: *mut FstStreamBuilder,
        bound: *const libc::c_char,
    ) {
        let bound = unsafe { CStr::from_ptr(bound).to_bytes() };
        fst_stream_builder_lt(sb, bound.as_ptr(), bound.len())
    }
}

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
        value: u64,
        error: *mut Error,
    ) -> bool {
        let builder = unsafe { &mut *builder };
        let key = unsafe { slice::from_raw_parts(key, key_len) };
        match builder.insert(key, value) {
            Ok(()) => true,
            Err(err) => {
                unsafe {
                    if !error.is_null() {
                        *error = Error::new(ErrorKind::Fst(From::from(err)));
                    }
                }
                false
            }
        }
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

pub type Automaton = Box<::fst::Automaton<State=Option<usize>>>;

ffi_fn! {
    fn fst_automaton_free(
        automaton: *mut Automaton,
    ) {
        unsafe { Box::from_raw(automaton); }
    }
}

ffi_fn! {
    fn fst_automaton_regex_new(
        pattern: *const u8,
        pattern_len: libc::size_t,
        error: *mut Error,
    ) -> *const Automaton {
        let pat = unsafe { slice::from_raw_parts(pattern, pattern_len) };
        let pat = match str::from_utf8(pat) {
            Ok(pat) => pat,
            Err(err) => {
                unsafe {
                    if !error.is_null() {
                        *error = Error::new(ErrorKind::Str(err));
                    }
                    return ptr::null();
                }
            }
        };
        match ::fst::Regex::new(pat) {
            Ok(re) => Box::into_raw(Box::new(Box::new(re) as Automaton)),
            Err(err) => {
                unsafe {
                    if !error.is_null() {
                        *error = Error::new(ErrorKind::Fst(From::from(err)));
                    }
                }
                ptr::null()
            }
        }
    }
}

ffi_fn! {
    fn fst_automaton_regex_new_nul(
        pattern: *const libc::c_char,
        error: *mut Error,
    ) -> *const Automaton {
        let pattern = unsafe { CStr::from_ptr(pattern).to_bytes() };
        fst_automaton_regex_new(pattern.as_ptr(), pattern.len(), error)
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
