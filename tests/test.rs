extern crate fst;
extern crate fst_regex;

use fst_regex::Regex;

use fst::{Automaton, IntoStreamer, Streamer};
use fst::automaton::Subsequence;
use fst::raw::{Builder, Fst, Output};

static WORDS: &'static str = include_str!("../data/words-10000");

