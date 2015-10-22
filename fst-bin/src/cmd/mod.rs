pub mod csv;
pub mod dot;
pub mod dupes;

pub mod mkset {
    use std::io::BufRead;

    use docopt::Docopt;
    use fst::SetBuilder;

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
        let mut set = try!(SetBuilder::new(wtr));
        for line in rdr.lines() {
            try!(set.insert(try!(line)));
        }
        try!(set.finish());
        Ok(())
    }
}

pub mod words {
    use std::io::Write;

    use docopt::Docopt;
    use fst::raw as fst;
    use fst::Stream;

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
        let mut rdr = fst.stream();
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
    use fst::{Regex, IntoStream, Stream};
    use fst::raw as fst;

    use util;
    use Error;

    const USAGE: &'static str = "
Usage: fst find [options] <fst> [<query>]
       fst find --help

Options:
    -h, --help          Show this help message.
    -f ARG, --file ARG  File containing queries, one per line, no ranges.
    -c, --count         Only show a count of hits instead of printing them.
    -s, --start ARG     Start of range query.
    -e, --end ARG       Start of range query.
    --regex             Interpret query as a regex.
";

    #[derive(Debug, RustcDecodable)]
    struct Args {
        arg_fst: String,
        arg_query: Option<String>,
        flag_f: Option<String>,
        flag_count: bool,
        flag_start: Option<String>,
        flag_end: Option<String>,
        flag_regex: bool,
    }

    pub fn run(argv: Vec<String>) -> Result<(), Error> {
        let args: Args = Docopt::new(USAGE)
                                .and_then(|d| d.argv(&argv).decode())
                                .unwrap_or_else(|e| e.exit());

        let mut wtr = try!(util::get_buf_writer(None));
        let fst = try!(fst::Fst::from_file_path(args.arg_fst));
        let mut hits = 0;

        if let Some(ref qpath) = args.flag_f {
            let qrdr = io::BufReader::new(fs::File::open(qpath).unwrap());
            for query in qrdr.lines().map(|line| line.unwrap()) {
                if fst.find(&query).is_some() {
                    hits += 1;
                    if !args.flag_count {
                        try!(writeln!(wtr, "{}", query));
                    }
                }
            }
        } else if !args.flag_regex && args.arg_query.is_some() {
            let query = args.arg_query.as_ref().unwrap();
            if fst.find(query).is_some() {
                hits += 1;
                if !args.flag_count {
                    try!(writeln!(wtr, "{}", query));
                }
            }
        } else if args.flag_regex && args.arg_query.is_some() {
            let re = try!(Regex::new(args.arg_query.as_ref().unwrap()));
            let mut range = fst.search(re);
            if let Some(ref min) = args.flag_start {
                range = range.ge(min);
            }
            if let Some(ref max) = args.flag_end {
                range = range.le(max);
            }
            let mut it = range.into_stream();
            while let Some((term, _)) = it.next() {
                hits += 1;
                if !args.flag_count {
                    try!(wtr.write_all(term));
                    try!(wtr.write_all(b"\n"));
                }
            }
        } else {
            let mut range = fst.range();
            if let Some(ref min) = args.flag_start {
                range = range.ge(min);
            }
            if let Some(ref max) = args.flag_end {
                range = range.le(max);
            }
            let mut it = range.into_stream();
            while let Some((term, _)) = it.next() {
                hits += 1;
                if !args.flag_count {
                    try!(wtr.write_all(term));
                    try!(wtr.write_all(b"\n"));
                }
            }
        }
        if args.flag_count {
            try!(writeln!(wtr, "{}", hits));
        }
        Ok(())
    }
}

pub mod union {
    use docopt::Docopt;
    use fst;

    use util;
    use Error;

    const USAGE: &'static str = "
Usage: fst union [options] <input>...
       fst union --help

Options:
    -h, --help              Show this help message.
    -o PATH, --output PATH  Write merged FST to given path.
";

    #[derive(Debug, RustcDecodable)]
    struct Args {
        arg_input: Vec<String>,
        flag_output: Option<String>,
    }

    pub fn run(argv: Vec<String>) -> Result<(), Error> {
        let args: Args = Docopt::new(USAGE)
                                .and_then(|d| d.argv(&argv).decode())
                                .unwrap_or_else(|e| e.exit());

        let wtr = try!(util::get_buf_writer(args.flag_output));
        let mut merged = try!(fst::SetBuilder::new(wtr));

        let mut sets = vec![];
        for set_path in &args.arg_input {
            sets.push(try!(fst::Set::from_file_path(set_path)));
        }
        let union = sets.iter().collect::<fst::set::SetOpBuilder>().union();
        try!(merged.extend_stream(union));
        try!(merged.finish());
        Ok(())
    }
}
