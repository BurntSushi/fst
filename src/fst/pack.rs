use std::io;
use std::mem;

use byteorder::{ReadBytesExt, LittleEndian};

/// pack_uint packs the given integer in the smallest number of bytes possible,
/// and writes it to the given writer. The number of bytes written is returned
/// on success.
pub fn pack_uint<W: io::Write>(wtr: W, n: u64) -> io::Result<u8> {
    let nbytes = pack_size(n);
    pack_uint_in(wtr, n, nbytes).map(|_| nbytes)
}

/// pack_uint_in is like pack_uint, but always uses the number of bytes given
/// to pack the number given.
///
/// `nbytes` must be >= pack_size(n) and <= 8, where `pack_size(n)` is the
/// smallest number of bytes that can store the integer given.
pub fn pack_uint_in<W: io::Write>(
    mut wtr: W,
    n: u64,
    nbytes: u8,
) -> io::Result<()> {
    assert!(pack_size(n) <= nbytes && nbytes <= 8);
    let n: [u8; 8] = unsafe { mem::transmute(n.to_le()) };
    wtr.write_all(&n[0..nbytes as usize])
}

/// unpack_uint is the dual of pack_uint. It unpacks the integer at the current
/// position in `rdr` after reading `nbytes` bytes.
///
/// `nbytes` must be >= 1 and <= 8.
pub fn unpack_uint<R: io::Read>(mut rdr: R, nbytes: u8) -> io::Result<u64> {
    assert!(1 <= nbytes && nbytes <= 8);
    rdr.read_uint::<LittleEndian>(nbytes as usize).map_err(From::from)
}

/// pack_size returns the smallest number of bytes that can encode `n`.
pub fn pack_size(n: u64) -> u8 {
    if n < 1 << 8 as u64 {
        1
    } else if n < 1 << 16 as u64 {
        2
    } else if n < 1 << 24 as u64 {
        3
    } else if n < 1 << 32 as u64 {
        4
    } else if n < 1 << 40 as u64 {
        5
    } else if n < 1 << 48 as u64 {
        6
    } else if n < 1 << 56 as u64 {
        7
    } else {
        8
    }
}

#[cfg(test)]
mod tests {
    use std::io;
    use quickcheck::{QuickCheck, StdGen};
    use super::*;

    #[test]
    fn prop_pack_in_out() {
        fn p(num: u64) -> bool {
            let mut buf = io::Cursor::new(vec![]);
            let size = pack_uint(&mut buf, num).unwrap();
            buf.set_position(0);
            num == unpack_uint(&mut buf, size).unwrap()
        }
        QuickCheck::new()
            .gen(StdGen::new(::rand::thread_rng(), 257)) // pick byte boundary
            .quickcheck(p as fn(u64) -> bool);
    }
}
