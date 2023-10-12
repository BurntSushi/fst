use std::ascii;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use bstr::{io::BufReadExt, BString};
use csv;
use fst::raw::{Fst, Output};
use fst::{IntoStreamer, Streamer};
use memmap2::Mmap;

use crate::Error;

pub unsafe fn mmap_fst<P: AsRef<Path>>(path: P) -> Result<Fst<Mmap>, Error> {
    let mmap = Mmap::map(&File::open(path)?)?;
    let fst = Fst::new(mmap)?;
    Ok(fst)
}

pub fn escape_input(b: u8) -> String {
    String::from_utf8(ascii::escape_default(b).collect::<Vec<_>>()).unwrap()
}

pub fn get_buf_reader<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<io::BufReader<Box<dyn io::Read + Send + Sync + 'static>>> {
    Ok(io::BufReader::new(get_reader(path)?))
}

pub fn get_buf_writer<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<io::BufWriter<Box<dyn io::Write + Send + Sync + 'static>>> {
    Ok(io::BufWriter::new(get_writer(path)?))
}

pub fn get_reader<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<Box<dyn io::Read + Send + Sync + 'static>> {
    Ok(match to_stdio(path) {
        None => Box::new(io::stdin()),
        Some(path) => Box::new(File::open(path)?),
    })
}

pub fn get_writer<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<Box<dyn io::Write + Send + Sync + 'static>> {
    Ok(match to_stdio(path) {
        None => Box::new(io::stdout()),
        Some(path) => Box::new(File::create(path)?),
    })
}

fn to_stdio<T: AsRef<Path>>(path: Option<T>) -> Option<PathBuf> {
    match path {
        None => None,
        Some(s) => {
            if s.as_ref().to_string_lossy() == "-" {
                None
            } else {
                Some(s.as_ref().to_path_buf())
            }
        }
    }
}

pub fn print_stream<'f, W, I, S>(
    mut wtr: W,
    outputs: bool,
    stream: I,
) -> Result<(), Error>
where
    W: io::Write,
    I: for<'a> IntoStreamer<'a, Into = S, Item = (&'a [u8], Output)>,
    S: 'f + for<'a> Streamer<'a, Item = (&'a [u8], Output)>,
{
    let mut stream = stream.into_stream();
    if outputs {
        let mut wtr = csv::Writer::from_writer(wtr);
        while let Some((k, v)) = stream.next() {
            let v = v.value().to_string();
            wtr.write_record((&[k, v.as_bytes()]).iter())?;
        }
        wtr.flush().map_err(From::from)
    } else {
        while let Some((k, _)) = stream.next() {
            wtr.write_all(k)?;
            wtr.write_all(b"\n")?;
        }
        wtr.flush().map_err(From::from)
    }
}

pub struct ConcatLines {
    inputs: Vec<PathBuf>,
    cur: Option<Lines>,
}

type Lines = bstr::io::ByteLines<
    io::BufReader<Box<dyn io::Read + Send + Sync + 'static>>,
>;

impl ConcatLines {
    pub fn new(mut inputs: Vec<PathBuf>) -> ConcatLines {
        inputs.reverse(); // treat it as a stack
        ConcatLines { inputs, cur: None }
    }
}

impl Iterator for ConcatLines {
    type Item = io::Result<BString>;

    fn next(&mut self) -> Option<io::Result<BString>> {
        loop {
            if self.cur.is_none() {
                match self.inputs.pop() {
                    None => return None,
                    Some(path) => {
                        let rdr = match get_buf_reader(Some(path)) {
                            Err(err) => return Some(Err(err)),
                            Ok(rdr) => rdr,
                        };
                        self.cur = Some(rdr.byte_lines());
                    }
                }
            }
            match self.cur.as_mut().and_then(|lines| lines.next()) {
                None => self.cur = None,
                Some(r) => return Some(r.map(BString::from)),
            }
        }
    }
}

pub struct ConcatCsv {
    inputs: Vec<PathBuf>,
    cur: Option<Rows>,
    delimiter: u8,
}

type Reader = Box<dyn io::Read + Send + Sync + 'static>;
type Rows = csv::DeserializeRecordsIntoIter<Reader, (BString, u64)>;

impl ConcatCsv {
    pub fn new(mut inputs: Vec<PathBuf>, delimiter: u8) -> ConcatCsv {
        inputs.reverse(); // treat it as a stack
        ConcatCsv { inputs, cur: None, delimiter }
    }

    fn read_row(&mut self) -> Option<Result<(BString, u64), Error>> {
        let rdr = match self.cur {
            None => return None,
            Some(ref mut rdr) => rdr,
        };
        match rdr.next() {
            Some(Ok((k, v))) => Some(Ok((k, v))),
            Some(Err(err)) => Some(Err(From::from(err))),
            None => None,
        }
    }
}

impl Iterator for ConcatCsv {
    type Item = Result<(BString, u64), Error>;

    fn next(&mut self) -> Option<Result<(BString, u64), Error>> {
        loop {
            if self.cur.is_none() {
                match self.inputs.pop() {
                    None => return None,
                    Some(path) => {
                        let rdr = match get_reader(Some(path)) {
                            Err(err) => return Some(Err(From::from(err))),
                            Ok(rdr) => rdr,
                        };
                        let csvrdr = csv::ReaderBuilder::new()
                            .delimiter(self.delimiter)
                            .has_headers(false)
                            .from_reader(rdr);
                        self.cur = Some(csvrdr.into_deserialize());
                    }
                }
            }
            match self.read_row() {
                None => self.cur = None,
                Some(r) => return Some(r),
            }
        }
    }
}
