use std::fmt;

use regex_syntax;

use {Automaton, Result};

pub use regex::error::Error;

mod compile;
mod dfa;
mod error;
mod sparse;

pub struct Regex {
    original: String,
    dfa: dfa::Dfa,
}

#[derive(Eq, PartialEq)]
pub enum Inst {
    Match,
    Jump(usize),
    Split(usize, usize),
    Range(u8, u8),
}

impl Regex {
    pub fn new(re: &str) -> Result<Regex> {
        Regex::with_size_limit(10 * (1 << 20), re)
    }

    pub fn with_size_limit(size: usize, re: &str) -> Result<Regex> {
        let expr = try!(regex_syntax::Expr::parse(re));
        let insts = try!(compile::Compiler::new(size).compile(&expr));
        let dfa = try!(dfa::DfaBuilder::new(insts).build());
        Ok(Regex { original: re.to_owned(), dfa: dfa })
    }
}

impl Automaton for Regex {
    type State = Option<usize>;

    fn start(&self) -> Option<usize> { Some(0) }

    fn is_match(&self, state: Option<usize>) -> bool {
        state.map(|state| self.dfa.is_match(state)).unwrap_or(false)
    }

    fn can_match(&self, state: Option<usize>) -> bool {
        state.is_some()
    }

    fn accept(&self, state: Option<usize>, byte: u8) -> Option<usize> {
        state.and_then(|state| self.dfa.accept(state, byte))
    }
}

impl fmt::Debug for Regex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "Regex({:?})", self.original));
        self.dfa.fmt(f)
    }
}

impl fmt::Debug for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Inst::*;
        match *self {
            Match => write!(f, "Match"),
            Jump(ip) => write!(f, "JUMP {}", ip),
            Split(ip1, ip2) => write!(f, "SPLIT {}, {}", ip1, ip2),
            Range(s, e) => write!(f, "RANGE {:X}-{:X}", s, e),
        }
    }
}

// #[cfg(test)]
// mod tests {
    // use regex::Regex;
//
    // #[test]
    // fn scratch() {
        // // let re = Regex::new("[\u{0}-\u{10FFFF}]").unwrap();
        // let re = Regex::new(r"[a-z0-9]").unwrap();
        // println!("{:?}", re.dfa);
    // }
// }
