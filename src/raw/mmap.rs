use std::fs;
use std::io;
use std::path::Path;
use std::sync::Arc;

use memmap::{Mmap, Protection};

/// A read only view into a memory map.
///
/// Reading a memory map is unsafe because we cannot guarantee that its
/// underlying memory is not mutated by external processes. This read only
/// memory map guarantees that consumers can at least never modify the
/// underlying data.
///
/// It is principally useful for controlling which region of a file an `Fst`
/// reads. Taking a slice from it is still considered unsafe.
pub struct MmapReadOnly {
    map: Arc<Mmap>,
    offset: usize,
    len: usize,
}

impl MmapReadOnly {
    /// Create a new memory map from an existing file handle.
    pub fn open(file: &fs::File) -> io::Result<MmapReadOnly> {
        Ok(try!(Mmap::open(file, Protection::Read)).into())
    }

    /// Open a new memory map from the path given.
    pub fn open_path<P: AsRef<Path>>(path: P) -> io::Result<MmapReadOnly> {
        MmapReadOnly::open(&try!(fs::File::open(path)))
    }

    /// Returns the size in byte of the memory map.
    ///
    /// If it is a range, the size is the size of the range.
    pub fn len(&self,) -> usize {
        self.len
    }

    /// Slice this memory map to a new `offset` and `len`.
    ///
    /// If the new range is outside the bounds of `self`, then this method
    /// panics.
    pub fn range(&self, offset: usize, len: usize) -> MmapReadOnly {
        assert!(offset + len <= self.len);
        MmapReadOnly {
            map: self.map.clone(),
            offset: self.offset + offset,
            len: len,
        }
    }

    /// Read the memory map as a `&[u8]`.
    pub unsafe fn as_slice(&self) -> &[u8] {
        self.map.as_slice()
    }
}

impl Clone for MmapReadOnly {
    fn clone(&self) -> MmapReadOnly {
        MmapReadOnly{
            map: self.map.clone(),
            offset: self.offset,
            len: self.len,
        }
    }
}

impl From<Mmap> for MmapReadOnly {
    fn from(mmap: Mmap) -> MmapReadOnly {
        let len = mmap.len();
        MmapReadOnly {
            map: Arc::new(mmap),
            offset: 0,
            len: len,
        }
    }
}
