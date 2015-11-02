use std::fs::File;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};

use csv;
use fst::{IntoStreamer, Streamer};
use fst::raw::Output;

use Error;

pub fn get_buf_reader<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<io::BufReader<Box<io::Read + Send + Sync + 'static>>> {
    Ok(io::BufReader::new(try!(get_reader(path))))
}

pub fn get_buf_writer<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<io::BufWriter<Box<io::Write + Send + Sync + 'static>>> {
    Ok(io::BufWriter::new(try!(get_writer(path))))
}

pub fn get_reader<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<Box<io::Read + Send + Sync + 'static>> {
    Ok(match to_stdio(path) {
        None => Box::new(io::stdin()),
        Some(path) => Box::new(try!(File::open(path))),
    })
}

pub fn get_writer<T: AsRef<Path>>(
    path: Option<T>,
) -> io::Result<Box<io::Write + Send + Sync + 'static>> {
    Ok(match to_stdio(path) {
        None => Box::new(io::stdout()),
        Some(path) => Box::new(try!(File::create(path))),
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
where W: io::Write,
      I: for<'a> IntoStreamer<'a, Into=S, Item=(&'a [u8], Output)>,
      S: 'f + for<'a> Streamer<'a, Item=(&'a [u8], Output)> {
    let mut stream = stream.into_stream();
    if outputs {
        let mut wtr = csv::Writer::from_writer(wtr);
        while let Some((k, v)) = stream.next() {
            let v = v.value().to_string();
            try!(wtr.write((&[k, v.as_bytes()]).iter()));
        }
        wtr.flush().map_err(From::from)
    } else {
        while let Some((k, _)) = stream.next() {
            try!(wtr.write_all(k));
            try!(wtr.write_all(b"\n"));
        }
        wtr.flush().map_err(From::from)
    }
}

pub struct ConcatLines {
    inputs: Vec<PathBuf>,
    cur: Option<Lines>,
}

type Lines = io::Lines<io::BufReader<Box<io::Read + Send + Sync + 'static>>>;

impl ConcatLines {
    pub fn new(mut inputs: Vec<PathBuf>) -> ConcatLines {
        inputs.reverse(); // treat it as a stack
        ConcatLines { inputs: inputs, cur: None }
    }
}

impl Iterator for ConcatLines {
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cur.is_none() {
                match self.inputs.pop() {
                    None => return None,
                    Some(path) => {
                        let rdr = match get_buf_reader(Some(path)) {
                            Err(err) => return Some(Err(err)),
                            Ok(rdr) => rdr,
                        };
                        self.cur = Some(rdr.lines());
                    }
                }
            }
            match self.cur.as_mut().and_then(|lines| Iterator::next(lines)) {
                None => self.cur = None,
                Some(r) => return Some(r),
            }
        }
    }
}

pub struct ConcatCsv {
    inputs: Vec<PathBuf>,
    cur: Option<Rows>,
}

type Rows = csv::Reader<Box<io::Read + Send + Sync + 'static>>;

impl ConcatCsv {
    pub fn new(mut inputs: Vec<PathBuf>) -> ConcatCsv {
        inputs.reverse(); // treat it as a stack
        ConcatCsv { inputs: inputs, cur: None }
    }

    fn read_row(&mut self) -> Option<Result<(String, u64), Error>> {
        let mut rdr = match self.cur {
            None => return None,
            Some(ref mut rdr) => rdr,
        };
        // This is soooo painful. The CSV crate needs to grow owned iterators.
        let key = match rdr.next_str().into_iter_result() {
            Some(Ok(field)) => field.to_owned(),
            Some(Err(err)) => return Some(Err(From::from(err))),
            None => return None, // This is OK
        };
        let val = match rdr.next_str().into_iter_result() {
            Some(Ok(field)) => match field.parse::<u64>() {
                Err(err) => return Some(Err(From::from(err))),
                Ok(val) => val,
            },
            Some(Err(err)) => return Some(Err(From::from(err))),
            None => return Some(Err(From::from(format!(
                "Expected row of length 2, but found row of length 1.")))),
        };
        match rdr.next_str().into_iter_result() {
            None => Some(Ok((key, val))),
            Some(_) => Some(Err(From::from(format!(
                "Expected row of length 2, \
                 but found row of length at least 3.")))),
        }
    }
}

impl Iterator for ConcatCsv {
    type Item = Result<(String, u64), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cur.is_none() {
                match self.inputs.pop() {
                    None => return None,
                    Some(path) => {
                        let rdr = match get_reader(Some(path)) {
                            Err(err) => return Some(Err(From::from(err))),
                            Ok(rdr) => rdr,
                        };
                        self.cur = Some(csv::Reader::from_reader(rdr));
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
