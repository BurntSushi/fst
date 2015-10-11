pub mod csv;
pub mod dot;
pub mod dupes;

pub mod mkset {
    use std::io::{BufRead, Write};

    use docopt::Docopt;
    use fst::fst;

    use util;
    use Error;

    const USAGE: &'static str = "
Usage: fst mkset [options] [<input>] [<output>]
";

    #[derive(Debug, RustcDecodable)]
    struct Args {
        arg_input: Option<String>,
        arg_output: Option<String>,
    }

    pub fn run(argv: Vec<String>) -> Result<(), Error> {
        let args: Args = Docopt::new(USAGE)
                                .and_then(|d| d.argv(&argv).decode())
                                .unwrap_or_else(|e| e.exit());
        let rdr = try!(util::get_buf_reader(args.arg_input));
        let wtr = try!(util::get_buf_writer(args.arg_output));
        let mut bfst = try!(fst::Builder::new(wtr));
        for line in rdr.lines() {
            try!(bfst.add(try!(line)));
        }
        try!(try!(bfst.into_inner()).flush());
        Ok(())
    }
}

pub mod words {
    use std::io::Write;

    use docopt::Docopt;
    use fst::fst;

    use util;
    use Error;

    const USAGE: &'static str = "
Usage: fst words [options] <input> [<output>]
";

    #[derive(Debug, RustcDecodable)]
    struct Args {
        arg_input: String,
        arg_output: Option<String>,
    }

    pub fn run(argv: Vec<String>) -> Result<(), Error> {
        let args: Args = Docopt::new(USAGE)
                                .and_then(|d| d.argv(&argv).decode())
                                .unwrap_or_else(|e| e.exit());

        let mut wtr = try!(util::get_buf_writer(args.arg_output));
        let fst = try!(fst::Fst::from_file_path(args.arg_input));
        let mut rdr = fst.reader();
        while let Some((word, _)) = rdr.next() {
            try!(wtr.write_all(word));
            try!(wtr.write_all(b"\n"));
        }
        try!(wtr.flush());
        Ok(())
    }
}

pub mod find {
    use std::fs;
    use std::io::{self, BufRead, Write};

    use docopt::Docopt;
    use fst::fst;

    use util;
    use Error;

    const USAGE: &'static str = "
Usage: fst find [options] <fst> [<query>]

Options:
    -f ARG   File containing queries, one per line.
";

    #[derive(Debug, RustcDecodable)]
    struct Args {
        arg_fst: String,
        arg_query: Option<String>,
        flag_f: Option<String>,
    }

    pub fn run(argv: Vec<String>) -> Result<(), Error> {
        let args: Args = Docopt::new(USAGE)
                                .and_then(|d| d.argv(&argv).decode())
                                .unwrap_or_else(|e| e.exit());

        let mut wtr = try!(util::get_buf_writer(None));
        let fst = try!(fst::Fst::from_file_path(args.arg_fst));
        let qpath = args.flag_f.clone().expect("-f is required for now");
        let qrdr = io::BufReader::new(fs::File::open(qpath).unwrap());

        let mut hits = 0;
        for query in qrdr.lines().map(|line| line.unwrap()) {
            if fst.find(query).is_some() {
                hits += 1;
            }
        }

        try!(writeln!(wtr, "found: {}", hits));
        Ok(())
    }
}
