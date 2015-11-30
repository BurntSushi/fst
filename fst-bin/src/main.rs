#![allow(dead_code)]

extern crate bit_set;
extern crate chan;
extern crate csv;
extern crate docopt;
extern crate fst;
extern crate lines;
extern crate num_cpus;
extern crate rustc_serialize;
extern crate tempdir;

use std::env;
use std::error;
use std::process;
use std::io::{self, Write};

use docopt::Docopt;

macro_rules! w {
    ($wtr:expr, $($tt:tt)*) => {{
        use std::io::Write;
        let _ = writeln!(&mut $wtr, $($tt)*);
    }}
}

macro_rules! fail {
    ($($tt:tt)*) => { return Err(From::from(format!($($tt)*))); }
}

mod cmd;
mod merge;
mod util;

pub type Error = Box<error::Error + Send + Sync>;

const USAGE: &'static str = "
Usage:
    fst <command> [<args>...]
    fst --help
    fst --version

Commands:
    csv     Emit statistics about nodes or edges.
    dot     Emit a dot representation of an FST.
    dupes   Emit diagnostic info about frequency of duplicate nodes.
    fuzzy   Run fuzzy queries based on edit distance.
    hamming Run fuzzy queries based on Hamming distance.
    grep    Search an FST with a regex.
    map     Create a new map of key-value pairs.
    node    Show a single node.
    range   Run range queries.
    rust    Emit a Rust source code for this FST.
    set     Create a new set of keys.
    union   Union two or more FSTs.

Options:
    -h, --help     Show this help message.
    -v, --version  Show version.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_command: Option<Command>,
}

#[derive(Debug, RustcDecodable)]
enum Command {
    Csv,
    Dot,
    Dupes,
    Fuzzy,
    Hamming,
    Grep,
    Map,
    Node,
    Range,
    Rust,
    Set,
    Union,
}

impl Command {
    fn run(self) -> Result<(), Error> {
        use self::Command::*;

        let argv: Vec<String> = env::args().collect();
        match self {
            Csv => cmd::csv::run(argv),
            Dot => cmd::dot::run(argv),
            Dupes => cmd::dupes::run(argv),
            Fuzzy => cmd::fuzzy::run(argv),
            Hamming => cmd::hamming::run(argv),
            Grep => cmd::grep::run(argv),
            Map => cmd::map::run(argv),
            Node => cmd::node::run(argv),
            Range => cmd::range::run(argv),
            Rust => cmd::rust::run(argv),
            Set => cmd::set::run(argv),
            Union => cmd::union::run(argv),
        }
    }
}

fn main() {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.options_first(true)
                                           .version(Some(version()))
                                           .decode())
                            .unwrap_or_else(|e| e.exit());
    let cmd = args.arg_command.expect("BUG: expected a command");
    if let Err(err) = cmd.run() {
        writeln!(&mut io::stderr(), "{}", err).unwrap();
        process::exit(1);
    }
}

fn version() -> String {
    let (maj, min, pat) = (
        option_env!("CARGO_PKG_VERSION_MAJOR"),
        option_env!("CARGO_PKG_VERSION_MINOR"),
        option_env!("CARGO_PKG_VERSION_PATCH"),
    );
    match (maj, min, pat) {
        (Some(maj), Some(min), Some(pat)) =>
            format!("{}.{}.{}", maj, min, pat),
        _ => "N/A".to_owned(),
    }
}
