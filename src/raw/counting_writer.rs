use std::io;

use crate::raw::crc32::CheckSummer;

/// Wraps any writer that counts and checksums bytes written.
pub struct CountingWriter<W> {
    wtr: W,
    cnt: u64,
    summer: CheckSummer,
}

impl<W: io::Write> CountingWriter<W> {
    /// Wrap the given writer with a counter.
    pub fn new(wtr: W) -> CountingWriter<W> {
        CountingWriter { wtr, cnt: 0, summer: CheckSummer::new() }
    }

    /// Return the total number of bytes written to the underlying writer.
    ///
    /// The count returned is the sum of all counts resulting from a call
    /// to `write`.
    pub fn count(&self) -> u64 {
        self.cnt
    }

    /// Returns the masked CRC32C checksum of the bytes written so far.
    ///
    /// This "masked" checksum is the same one used by the Snappy frame format.
    /// Masking is supposed to make the checksum robust with respect to data
    /// that contains the checksum itself.
    pub fn masked_checksum(&self) -> u32 {
        self.summer.masked()
    }

    /// Unwrap the counting writer and return the inner writer.
    pub fn into_inner(self) -> W {
        self.wtr
    }

    /// Gets a reference to the underlying writer.
    pub fn get_ref(&self) -> &W {
        &self.wtr
    }
}

impl<W: io::Write> io::Write for CountingWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.summer.update(buf);
        let n = self.wtr.write(buf)?;
        self.cnt += n as u64;
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.wtr.flush()
    }
}

#[cfg(test)]
mod tests {
    use super::CountingWriter;
    use std::io::Write;

    #[test]
    fn counts_bytes() {
        let mut wtr = CountingWriter::new(vec![]);
        wtr.write_all(b"foobar").unwrap();
        assert_eq!(wtr.count(), 6);
    }
}
