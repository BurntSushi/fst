use anyhow::Error;

macro_rules! w {
    ($wtr:expr, $($tt:tt)*) => {{
        use std::io::Write;
        let _ = writeln!(&mut $wtr, $($tt)*);
    }}
}

mod app;
mod cmd;
mod merge;
mod util;

fn main() -> Result<(), Error> {
    match crate::app::app().get_matches().subcommand() {
        ("csv", Some(m)) => cmd::csv::run(m),
        ("dot", Some(m)) => cmd::dot::run(m),
        ("dupes", Some(m)) => cmd::dupes::run(m),
        ("fuzzy", Some(m)) => cmd::fuzzy::run(m),
        ("grep", Some(m)) => cmd::grep::run(m),
        ("map", Some(m)) => cmd::map::run(m),
        ("node", Some(m)) => cmd::node::run(m),
        ("range", Some(m)) => cmd::range::run(m),
        ("rust", Some(m)) => cmd::rust::run(m),
        ("set", Some(m)) => cmd::set::run(m),
        ("union", Some(m)) => cmd::union::run(m),
        ("", _) => {
            app::app().print_help()?;
            println!("");
            Ok(())
        }
        (unknown, _) => {
            Err(anyhow::anyhow!("unrecognized command: {}", unknown))
        }
    }
}
