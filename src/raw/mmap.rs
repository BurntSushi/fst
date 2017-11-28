use std::fs;
use std::io;
use std::path::Path;
use std::sync::Arc;

use memmap::Mmap;

/// A read only view into a memory map.
///
/// Opening a memory map is unsafe because we cannot guarantee that its
/// underlying memory is not mutated by external processes. This read only
/// memory map guarantees that consumers can at least never modify the
/// underlying data.
///
/// It is principally useful for controlling which region of a file an `Fst`
/// reads.
pub struct MmapReadOnly {
    map: Arc<Mmap>,
    offset: usize,
    len: usize,
}

impl MmapReadOnly {
    /// Create a new memory map from an existing file handle.
    ///
    /// This is unsafe because Rust programs cannot guarantee that memory
    /// backed by a memory mapped file won't be mutably aliased. It is up to
    /// the caller to enforce that the memory map is not modified while it is
    /// opened.
    pub unsafe fn open(file: &fs::File) -> io::Result<MmapReadOnly> {
        let mmap = Mmap::map(file)?;
        let len = mmap.len();
        Ok(MmapReadOnly {
            map: Arc::new(mmap),
            offset: 0,
            len: len,
        })
    }

    /// Open a new memory map from the path given.
    ///
    /// This is unsafe because Rust programs cannot guarantee that memory
    /// backed by a memory mapped file won't be mutably aliased. It is up to
    /// the caller to enforce that the memory map is not modified while it is
    /// opened.
    pub unsafe fn open_path<P: AsRef<Path>>(
        path: P,
    ) -> io::Result<MmapReadOnly> {
        MmapReadOnly::open(&fs::File::open(path)?)
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
    pub fn as_slice(&self) -> &[u8] {
        &self.map[self.offset..self.offset + self.len]
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
