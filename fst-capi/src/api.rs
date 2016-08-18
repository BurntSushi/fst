use std::ops;
use std::slice;

use fst::raw::Builder;
use libc;

pub struct FstRawBuilderMemory {
    builder: Builder<Vec<u8>>,
}

impl ops::Deref for FstRawBuilderMemory {
    type Target = Builder<Vec<u8>>;
    fn deref(&self) -> &Self::Target { &self.builder }
}

impl ops::DerefMut for FstRawBuilderMemory {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.builder }
}


ffi_fn! {
    fn fst_raw_builder_memory_new() -> *mut FstRawBuilderMemory {
        Box::into_raw(Box::new(FstRawBuilderMemory {
            builder: Builder::memory(),
        }))
    }
}

ffi_fn! {
    fn fst_raw_builder_memory_add(
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
    fn fst_raw_builder_memory_finish(
        builder: *mut FstRawBuilderMemory,
    ) -> bool {
        let builder = unsafe { *Box::from_raw(builder) };
        builder.builder.finish().unwrap();
        true
    }
}
