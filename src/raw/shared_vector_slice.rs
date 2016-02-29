use std::sync::Arc;
use std::borrow::Cow;


/// A read only view on a Vec<u8>
///
/// It is the anonymous memory of the MmapReadOnly struct.
pub struct SharedVectorSlice {
    bytes: Arc<Cow<'static, [u8]>>,
    offset: usize,
    len: usize,
}

impl SharedVectorSlice {
    /// Slice this vector to a new `offset` and `len`.
    ///
    /// Slicing does not copy the data.
    /// If the new range is outside the bounds of `self`, then this method
    /// panics.
    pub fn range(&self, offset: usize, len: usize) -> SharedVectorSlice {
        assert!(offset + len <= self.len);
        SharedVectorSlice {
            bytes: self.bytes.clone(),
            offset: self.offset + offset,
            len: len,
        }
    }

    /// Read the memory map as a `&[u8]`.
    pub unsafe fn as_slice(&self) -> &[u8] {
        &(*self.bytes)[self.offset..self.offset + self.len]
    }
}

impl From<Vec<u8>> for SharedVectorSlice {
    fn from(bytes: Vec<u8>) -> SharedVectorSlice {
        let num_bytes = bytes.len();
        SharedVectorSlice {
            bytes: Arc::new(Cow::Owned(bytes)),
            offset: 0,
            len: num_bytes,
        }
    }
}

impl From<&'static [u8]> for SharedVectorSlice {
    fn from(bytes: &'static [u8]) -> SharedVectorSlice {
        let num_bytes = bytes.len();
        SharedVectorSlice {
            bytes: Arc::new(Cow::Borrowed(bytes)),
            offset: 0,
            len: num_bytes,
        }
    }
}

#[test]
fn test_shared_vector_slice() {
    let vals: Vec<u8> = (0..100).collect();
    let shared_vector_slice = SharedVectorSlice::from(vals);
    {
        let slice_3_5 = unsafe { &shared_vector_slice.as_slice()[3..5]};
        assert_eq!(slice_3_5.len(), 2);
        assert_eq!(slice_3_5[0], 3);
        assert_eq!(slice_3_5[1], 4);
    }
    {
        let range = shared_vector_slice.range(3, 2);
        let slice_3_5 = unsafe { range.as_slice() };
        assert_eq!(slice_3_5.len(), 2);
        assert_eq!(slice_3_5[0], 3);
        assert_eq!(slice_3_5[1], 4);
    }
}
