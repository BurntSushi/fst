use crate::bytes;
use crate::raw::crc32_table::{TABLE, TABLE16};

/// Provides a simple API to perform a rolling CRC32C checksum.
#[derive(Clone, Copy, Debug)]
pub struct CheckSummer {
    sum: u32,
}

impl CheckSummer {
    /// Create a new checksummer that can compute CRC32C checksums on arbitrary
    /// bytes.
    pub fn new() -> CheckSummer {
        CheckSummer { sum: 0 }
    }

    /// Returns the "masked" CRC32 checksum of the data so far using the
    /// Castagnoli polynomial. This "masked" checksum is the same one used
    /// by the Snappy frame format. Masking is supposed to make the checksum
    /// robust with respect to data that contains the checksum itself.
    pub fn masked(&self) -> u32 {
        let sum = self.sum;
        (sum.wrapping_shr(15) | sum.wrapping_shl(17)).wrapping_add(0xA282EAD8)
    }

    /// Update the current checksum with the checksum for the given bytes.
    pub fn update(&mut self, buf: &[u8]) {
        self.sum = crc32c_slice16(self.sum, buf);
    }
}

/// Returns the CRC32 checksum of `buf` using the Castagnoli polynomial.
fn crc32c_slice16(prev: u32, mut buf: &[u8]) -> u32 {
    let mut crc: u32 = !prev;
    while buf.len() >= 16 {
        crc ^= bytes::read_u32_le(buf);
        crc = TABLE16[0][buf[15] as usize]
            ^ TABLE16[1][buf[14] as usize]
            ^ TABLE16[2][buf[13] as usize]
            ^ TABLE16[3][buf[12] as usize]
            ^ TABLE16[4][buf[11] as usize]
            ^ TABLE16[5][buf[10] as usize]
            ^ TABLE16[6][buf[9] as usize]
            ^ TABLE16[7][buf[8] as usize]
            ^ TABLE16[8][buf[7] as usize]
            ^ TABLE16[9][buf[6] as usize]
            ^ TABLE16[10][buf[5] as usize]
            ^ TABLE16[11][buf[4] as usize]
            ^ TABLE16[12][(crc >> 24) as u8 as usize]
            ^ TABLE16[13][(crc >> 16) as u8 as usize]
            ^ TABLE16[14][(crc >> 8) as u8 as usize]
            ^ TABLE16[15][(crc) as u8 as usize];
        buf = &buf[16..];
    }
    for &b in buf {
        crc = TABLE[((crc as u8) ^ b) as usize] ^ (crc >> 8);
    }
    !crc
}
