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
    mkset   Create a new set of words.
    words   Emit all words in given set/map.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_command: Option<Command>,
}

#[derive(Debug, RustcDecodable)]
enum Command {
    Csv,
    MkSet,
    Words,
}

impl Command {
    fn run(self) -> Result<(), Error> {
        let argv: Vec<String> = env::args().collect();
        match self {
            Command::Csv => cmd::csv::run(argv),
            Command::MkSet => cmd::mkset::run(argv),
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
