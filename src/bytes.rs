use std::convert::TryInto;
use std::io;

/// Read a u32 in little endian format from the beginning of the given slice.
/// This panics if the slice has length less than 4.
#[inline]
pub fn read_u32_le(slice: &[u8]) -> u32 {
    u32::from_le_bytes(slice[..4].try_into().unwrap())
}

/// Read a u64 in little endian format from the beginning of the given slice.
/// This panics if the slice has length less than 8.
#[inline]
pub fn read_u64_le(slice: &[u8]) -> u64 {
    u64::from_le_bytes(slice[..8].try_into().unwrap())
}

/// Write a u32 in little endian format to the beginning of the given slice.
/// This panics if the slice has length less than 4.
#[inline]
pub fn write_u32_le(n: u32, slice: &mut [u8]) {
    assert!(slice.len() >= 4);
    let bytes = n.to_le_bytes();
    slice[0] = bytes[0];
    slice[1] = bytes[1];
    slice[2] = bytes[2];
    slice[3] = bytes[3];
}

/// Like write_u32_le, but to an io::Write implementation. If every byte could
/// not be writen, then this returns an error.
#[inline]
pub fn io_write_u32_le<W: io::Write>(n: u32, mut wtr: W) -> io::Result<()> {
    let mut buf = [0; 4];
    write_u32_le(n, &mut buf);
    wtr.write_all(&buf)
}

/// Write a u64 in little endian format to the beginning of the given slice.
/// This panics if the slice has length less than 8.
#[inline]
pub fn write_u64_le(n: u64, slice: &mut [u8]) {
    assert!(slice.len() >= 8);
    let bytes = n.to_le_bytes();
    slice[0] = bytes[0];
    slice[1] = bytes[1];
    slice[2] = bytes[2];
    slice[3] = bytes[3];
    slice[4] = bytes[4];
    slice[5] = bytes[5];
    slice[6] = bytes[6];
    slice[7] = bytes[7];
}

/// Like write_u64_le, but to an io::Write implementation. If every byte could
/// not be writen, then this returns an error.
#[inline]
pub fn io_write_u64_le<W: io::Write>(n: u64, mut wtr: W) -> io::Result<()> {
    let mut buf = [0; 8];
    write_u64_le(n, &mut buf);
    wtr.write_all(&buf)
}

/// pack_uint packs the given integer in the smallest number of bytes possible,
/// and writes it to the given writer. The number of bytes written is returned
/// on success.
#[inline]
pub fn pack_uint<W: io::Write>(wtr: W, n: u64) -> io::Result<u8> {
    let nbytes = pack_size(n);
    pack_uint_in(wtr, n, nbytes).map(|_| nbytes)
}

/// pack_uint_in is like pack_uint, but always uses the number of bytes given
/// to pack the number given.
///
/// `nbytes` must be >= pack_size(n) and <= 8, where `pack_size(n)` is the
/// smallest number of bytes that can store the integer given.
#[inline]
pub fn pack_uint_in<W: io::Write>(
    mut wtr: W,
    mut n: u64,
    nbytes: u8,
) -> io::Result<()> {
    assert!(1 <= nbytes && nbytes <= 8);
    let mut buf = [0u8; 8];
    for i in 0..nbytes {
        buf[i as usize] = n as u8;
        n = n >> 8;
    }
    wtr.write_all(&buf[..nbytes as usize])?;
    Ok(())
}

/// unpack_uint is the dual of pack_uint. It unpacks the integer at the current
/// position in `slice` after reading `nbytes` bytes.
///
/// `nbytes` must be >= 1 and <= 8.
#[inline]
pub fn unpack_uint(slice: &[u8], nbytes: u8) -> u64 {
    assert!(1 <= nbytes && nbytes <= 8);

    let mut n = 0;
    for (i, &b) in slice[..nbytes as usize].iter().enumerate() {
        n = n | ((b as u64) << (8 * i));
    }
    n
}

/// pack_size returns the smallest number of bytes that can encode `n`.
#[inline]
pub fn pack_size(n: u64) -> u8 {
    if n < 1 << 8 {
        1
    } else if n < 1 << 16 {
        2
    } else if n < 1 << 24 {
        3
    } else if n < 1 << 32 {
        4
    } else if n < 1 << 40 {
        5
    } else if n < 1 << 48 {
        6
    } else if n < 1 << 56 {
        7
    } else {
        8
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{QuickCheck, StdGen};
    use std::io;

    #[test]
    fn prop_pack_in_out() {
        fn p(num: u64) -> bool {
            let mut buf = io::Cursor::new(vec![]);
            let size = pack_uint(&mut buf, num).unwrap();
            buf.set_position(0);
            num == unpack_uint(buf.get_ref(), size)
        }
        QuickCheck::new()
            .gen(StdGen::new(::rand::thread_rng(), 257)) // pick byte boundary
            .quickcheck(p as fn(u64) -> bool);
    }
}
