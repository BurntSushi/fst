use std::io::{Read, Write};
use std::path::PathBuf;

use crate::{util, Error};

pub fn run(matches: &clap::ArgMatches) -> Result<(), Error> {
    Args::new(matches).and_then(|args| args.run())
}

#[derive(Debug)]
struct Args {
    input: PathBuf,
    name: String,
}

impl Args {
    fn new(m: &clap::ArgMatches) -> Result<Args, Error> {
        Ok(Args {
            input: m.value_of_os("input").map(PathBuf::from).unwrap(),
            name: m
                .value_of_os("name")
                .map(|v| v.to_string_lossy().into_owned())
                .unwrap(),
        })
    }

    fn run(&self) -> Result<(), Error> {
        let mut wtr = util::get_buf_writer::<&str>(None)?;
        let mut rdr = util::get_buf_reader(Some(&self.input))?;

        let mut bytes = vec![];
        rdr.read_to_end(&mut bytes)?;

        w!(wtr, "lazy_static! {{");
        w!(wtr, "    pub static ref {}: ::fst::raw::Fst = ", self.name);
        w!(wtr, "        ::fst::raw::Fst::new({}_BYTES).unwrap();", self.name);
        w!(wtr, "}}\n");

        w!(wtr, "const {}_BYTES: &'static [u8] = b\"\\", self.name);
        let mut column = 0;
        for b in bytes {
            let escaped = if (b as char).is_whitespace() {
                format!("\\x{:02x}", b)
            } else {
                util::escape_input(b)
            };
            if column + escaped.len() >= 79 {
                column = 0;
                write!(wtr, "\\\n")?;
            }
            column += escaped.len();
            write!(wtr, "{}", escaped)?;
        }
        w!(wtr, "\\\n\";");

        Ok(())
    }
}
