use std::ops;
use std::ptr;
use std::slice;

use fst::raw as fst;
use libc;

use error::{Error, ErrorKind};

pub struct Fst(fst::Fst);

impl ops::Deref for Fst {
    type Target = fst::Fst;
    fn deref(&self) -> &Self::Target { &self.0 }
}

ffi_fn! {
    fn fst_get(
        fst: *mut Fst,
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
        fst: *mut Fst,
        key: *const u8,
        key_len: libc::size_t,
    ) -> bool {
        let fst = unsafe { &mut *fst };
        let key = unsafe { slice::from_raw_parts(key, key_len) };
        fst.contains_key(key)
    }
}

ffi_fn! {
    fn fst_len(fst: *mut Fst) -> libc::size_t {
        let fst = unsafe { &mut *fst };
        fst.len()
    }
}

ffi_fn! {
    fn fst_size(fst: *mut Fst) -> libc::size_t {
        let fst = unsafe { &mut *fst };
        fst.size()
    }
}


ffi_fn! {
    fn fst_free(fst: *mut Fst) {
        unsafe { Box::from_raw(fst); }
    }
}

pub struct FstRawBuilderMemory(fst::Builder<Vec<u8>>);

impl ops::Deref for FstRawBuilderMemory {
    type Target = fst::Builder<Vec<u8>>;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl ops::DerefMut for FstRawBuilderMemory {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

ffi_fn! {
    fn fst_builder_memory_new() -> *mut FstRawBuilderMemory {
        Box::into_raw(Box::new(FstRawBuilderMemory(fst::Builder::memory())))
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
    ) -> *mut Fst {
        let builder = unsafe { *Box::from_raw(builder) };
        match builder.0.into_inner().and_then(fst::Fst::from_bytes) {
            Ok(fst) => Box::into_raw(Box::new(Fst(fst))),
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
