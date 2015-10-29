extern crate bit_set;
extern crate csv;
extern crate docopt;
extern crate fst;
extern crate rustc_serialize;

use std::env;
use std::error;
use std::process;
use std::io::{self, Write};

use docopt::Docopt;

macro_rules! w {
    ($($tt:tt)*) => {{ let _ = writeln!($($tt)*); }}
}

mod cmd;
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
    dupes   Emit info about frequency of duplicate nodes.
    find    Search an FST.
    mkmap   Create a new map of key-value pairs.
    mkset   Create a new set of words.
    union   Union two or more FSTs.
    words   Emit all words in given set/map.

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
    Find,
    MkMap,
    MkSet,
    Union,
    Words,
}

impl Command {
    fn run(self) -> Result<(), Error> {
        let argv: Vec<String> = env::args().collect();
        match self {
            Command::Csv => cmd::csv::run(argv),
            Command::Dot => cmd::dot::run(argv),
            Command::Dupes => cmd::dupes::run(argv),
            Command::Find => cmd::find::run(argv),
            Command::MkMap => cmd::mkmap::run(argv),
            Command::MkSet => cmd::mkset::run(argv),
            Command::Union => cmd::union::run(argv),
            Command::Words => cmd::words::run(argv),
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
