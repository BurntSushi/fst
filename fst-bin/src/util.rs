use std::fs::File;
use std::io;

use csv;
use fst::{IntoStreamer, Streamer};
use fst::raw::Output;

use Error;

pub fn get_buf_reader<T: AsRef<str>>(
    path: Option<T>,
) -> io::Result<io::BufReader<Box<io::Read>>> {
    Ok(io::BufReader::new(try!(get_reader(path))))
}

pub fn get_buf_writer<T: AsRef<str>>(
    path: Option<T>,
) -> io::Result<io::BufWriter<Box<io::Write>>> {
    Ok(io::BufWriter::new(try!(get_writer(path))))
}

pub fn get_reader<T: AsRef<str>>(
    path: Option<T>,
) -> io::Result<Box<io::Read>> {
    Ok(match to_stdio(path) {
        None => Box::new(io::stdin()),
        Some(path) => Box::new(try!(File::open(path))),
    })
}

pub fn get_writer<T: AsRef<str>>(
    path: Option<T>,
) -> io::Result<Box<io::Write>> {
    Ok(match to_stdio(path) {
        None => Box::new(io::stdout()),
        Some(path) => Box::new(try!(File::create(path))),
    })
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

fn to_stdio<T: AsRef<str>>(path: Option<T>) -> Option<String> {
    match path {
        None => None,
        Some(s) => {
            if "-" == s.as_ref() {
                None
            } else {
                Some(s.as_ref().to_owned())
            }
        }
    }
}
