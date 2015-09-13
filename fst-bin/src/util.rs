use std::fs::File;
use std::io;

pub fn get_buf_reader(
    path: Option<String>,
) -> io::Result<io::BufReader<Box<io::Read>>> {
    Ok(io::BufReader::new(try!(get_reader(path))))
}

pub fn get_buf_writer(
    path: Option<String>,
) -> io::Result<io::BufWriter<Box<io::Write>>> {
    Ok(io::BufWriter::new(try!(get_writer(path))))
}

pub fn get_reader(path: Option<String>) -> io::Result<Box<io::Read>> {
    Ok(if path == None || path == Some("-".to_owned()) {
        Box::new(io::stdin())
    } else {
        Box::new(try!(File::open(path.unwrap())))
    })
}

pub fn get_writer(path: Option<String>) -> io::Result<Box<io::Write>> {
    Ok(if path == None || path == Some("-".to_owned()) {
        Box::new(io::stdout())
    } else {
        Box::new(try!(File::create(path.unwrap())))
    })
}
