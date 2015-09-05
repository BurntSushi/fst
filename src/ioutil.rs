use std::io;

pub trait ReadExt: io::Read {
    fn read_full(&mut self, buf: &mut [u8]) -> io::Result<()> {
        let mut nread = 0usize;
        while nread < buf.len() {
            match self.read(&mut buf[nread..]) {
                Ok(0) => return Err(io::Error::new(io::ErrorKind::Other,
                                                   "unexpected EOF")),
                Ok(n) => nread += n,
                Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {},
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }
}

impl<T: io::Read> ReadExt for T {}

pub struct CountingWriter<W> {
    wtr: W,
    cnt: u64,
}

impl<W: io::Write> CountingWriter<W> {
    pub fn new(wtr: W) -> CountingWriter<W> {
        CountingWriter {
            wtr: wtr,
            cnt: 0,
        }
    }

    pub fn count(&self) -> u64 {
        self.cnt
    }

    pub fn into_inner(self) -> W {
        self.wtr
    }
}

impl<W: io::Write> io::Write for CountingWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = try!(self.wtr.write(buf));
        self.cnt += n as u64;
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.wtr.flush()
    }
}
