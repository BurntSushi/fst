extern crate fst;
extern crate utf8_ranges;

use std::cmp;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::fmt;

use utf8_ranges::{Utf8Range, Utf8Sequences};

use fst::automaton::Automaton;

pub use self::error::Error;

mod error;

const STATE_LIMIT: usize = 10_000; // currently at least 20MB >_<

/// A Unicode aware Levenshtein automaton for running efficient fuzzy queries.
///
/// A Levenshtein automata is one way to search any finite state transducer
/// for keys that *approximately* match a given query. A Levenshtein automaton
/// approximates this by returning all keys within a certain edit distance of
/// the query. The edit distance is defined by the number of insertions,
/// deletions and substitutions required to turn the query into the key.
/// Insertions, deletions and substitutions are based on
/// **Unicode characters** (where each character is a single Unicode scalar
/// value).
///
/// # Example
///
/// This example shows how to find all keys within an edit distance of `1`
/// from `foo`.
///
/// ```rust
/// extern crate fst;
/// extern crate fst_levenshtein;
///
/// use fst::{IntoStreamer, Streamer, Set};
/// use fst_levenshtein::Levenshtein;
///
/// fn main() {
///     let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
///     let set = Set::from_iter(keys).unwrap();
///
///     let lev = Levenshtein::new("foo", 1).unwrap();
///     let mut stream = set.search(&lev).into_stream();
///
///     let mut keys = vec![];
///     while let Some(key) = stream.next() {
///         keys.push(key.to_vec());
///     }
///     assert_eq!(keys, vec![
///         "fo".as_bytes(),   // 1 deletion
///         "fob".as_bytes(),  // 1 substitution
///         "foo".as_bytes(),  // 0 insertions/deletions/substitutions
///         "food".as_bytes(), // 1 insertion
///     ]);
/// }
/// ```
///
/// This example only uses ASCII characters, but it will work equally well
/// on Unicode characters.
///
/// # Warning: experimental
///
/// While executing this Levenshtein automaton against a finite state
/// transducer will be very fast, *constructing* an automaton may not be.
/// Namely, this implementation is a proof of concept. While I believe the
/// algorithmic complexity is not exponential, the implementation is not speedy
/// and it can use enormous amounts of memory (tens of MB before a hard-coded
/// limit will cause an error to be returned).
///
/// This is important functionality, so one should count on this implementation
/// being vastly improved in the future.
pub struct Levenshtein {
    prog: DynamicLevenshtein,
    dfa: Dfa,
}

impl Levenshtein {
    /// Create a new Levenshtein query.
    ///
    /// The query finds all matching terms that are at most `distance`
    /// edit operations from `query`. (An edit operation may be an insertion,
    /// a deletion or a substitution.)
    ///
    /// If the underlying automaton becomes too big, then an error is returned.
    ///
    /// A `Levenshtein` value satisfies the `Automaton` trait, which means it
    /// can be used with the `search` method of any finite state transducer.
    #[inline]
    pub fn new(query: &str, distance: u32) -> Result<Self, Error> {
        let lev = DynamicLevenshtein {
            query: query.to_owned(),
            dist: distance as usize,
        };
        let dfa = DfaBuilder::new(lev.clone()).build()?;
        Ok(Levenshtein {
            prog: lev,
            dfa: dfa,
        })
    }
}

impl fmt::Debug for Levenshtein {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Levenshtein(query: {:?}, distance: {:?})",
               self.prog.query, self.prog.dist)
    }
}

#[derive(Clone)]
struct DynamicLevenshtein {
    query: String,
    dist: usize,
}

impl DynamicLevenshtein {
    fn start(&self) -> Vec<usize> {
        (0..self.query.chars().count()+1).collect()
    }

    fn is_match(&self, state: &[usize]) -> bool {
        state.last().map(|&n| n <= self.dist).unwrap_or(false)
    }

    fn can_match(&self, state: &[usize]) -> bool {
        state.iter().min().map(|&n| n <= self.dist).unwrap_or(false)
    }

    fn accept(&self, state: &[usize], chr: Option<char>) -> Vec<usize> {
        let mut next = vec![state[0]+1];
        for (i, c) in self.query.chars().enumerate() {
            let cost = if Some(c) == chr { 0 } else { 1 };
            let v = cmp::min(cmp::min(next[i]+1, state[i+1]+1), state[i]+cost);
            next.push(cmp::min(v, self.dist + 1));
        }
        next
    }
}

impl Automaton for Levenshtein {
    type State = Option<usize>;

    #[inline]
    fn start(&self) -> Option<usize> { Some(0) }

    #[inline]
    fn is_match(&self, state: &Option<usize>) -> bool {
        state.map(|state| self.dfa.states[state].is_match).unwrap_or(false)
    }

    #[inline]
    fn can_match(&self, state: &Option<usize>) -> bool {
        state.is_some()
    }

    #[inline]
    fn accept(&self, state: &Option<usize>, byte: u8) -> Option<usize> {
        state.and_then(|state| self.dfa.states[state].next[byte as usize])
    }
}

#[derive(Debug)]
pub struct Dfa {
    states: Vec<State>,
}

struct State {
    next: [Option<usize>; 256],
    is_match: bool,
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "State {{")?;
        writeln!(f, "  is_match: {:?}", self.is_match)?;
        for i in 0..256 {
            if let Some(si) = self.next[i] {
                writeln!(f, "  {:?}: {:?}", i, si)?;
            }
        }
        write!(f, "}}")
    }
}

struct DfaBuilder {
    dfa: Dfa,
    lev: DynamicLevenshtein,
    cache: HashMap<Vec<usize>, usize>,
}

impl DfaBuilder {
    fn new(lev: DynamicLevenshtein) -> Self {
        DfaBuilder {
            dfa: Dfa {
                states: Vec::with_capacity(16),
            },
            lev: lev,
            cache: HashMap::with_capacity(1024),
        }
    }

    fn build(mut self) -> Result<Dfa, Error> {
        let mut stack = vec![self.lev.start()];
        let mut seen = HashSet::new();
        let query = self.lev.query.clone(); // temp work around of borrowck
        while let Some(lev_state) = stack.pop() {
            let dfa_si = self.cached_state(&lev_state).unwrap();
            let mismatch = self.add_mismatch_utf8_states(dfa_si, &lev_state);
            if let Some((next_si, lev_next)) = mismatch {
                if !seen.contains(&next_si) {
                    seen.insert(next_si);
                    stack.push(lev_next);
                }
            }
            for (i, c) in query.chars().enumerate() {
                if lev_state[i] > self.lev.dist {
                    continue;
                }
                let lev_next = self.lev.accept(&lev_state, Some(c));
                let next_si = self.cached_state(&lev_next);
                if let Some(next_si) = next_si {
                    self.add_utf8_sequences(true, dfa_si, next_si, c, c);
                    if !seen.contains(&next_si) {
                        seen.insert(next_si);
                        stack.push(lev_next);
                    }
                }
            }
            if self.dfa.states.len() > STATE_LIMIT {
                return Err(Error::TooManyStates(STATE_LIMIT));
            }
        }
        Ok(self.dfa)
    }

    fn cached_state(&mut self, lev_state: &[usize]) -> Option<usize> {
        self.cached(lev_state).map(|(si, _)| si)
    }

    fn cached(&mut self, lev_state: &[usize]) -> Option<(usize, bool)> {
        if !self.lev.can_match(lev_state) {
            return None;
        }
        Some(match self.cache.entry(lev_state.to_vec()) {
            Entry::Occupied(v) => (*v.get(), true),
            Entry::Vacant(v) => {
                let is_match = self.lev.is_match(lev_state);
                self.dfa.states.push(State {
                    next: [None; 256],
                    is_match: is_match,
                });
                (*v.insert(self.dfa.states.len() - 1), false)
            }
        })
    }

    fn add_mismatch_utf8_states(
        &mut self,
        from_si: usize,
        lev_state: &[usize],
    ) -> Option<(usize, Vec<usize>)> {
        let mismatch_state = self.lev.accept(lev_state, None);
        let to_si = match self.cached(&mismatch_state) {
            None => return None,
            Some((si, _)) => si,
            // Some((si, true)) => return Some((si, mismatch_state)),
            // Some((si, false)) => si,
        };
        self.add_utf8_sequences(false, from_si, to_si, '\u{0}', '\u{10FFFF}');
        return Some((to_si, mismatch_state));
    }

    fn add_utf8_sequences(
        &mut self,
        overwrite: bool,
        from_si: usize,
        to_si: usize,
        from_chr: char,
        to_chr: char,
    ) {
        for seq in Utf8Sequences::new(from_chr, to_chr) {
            let mut fsi = from_si;
            for range in &seq.as_slice()[0..seq.len()-1] {
                let tsi = self.new_state(false);
                self.add_utf8_range(overwrite, fsi, tsi, range);
                fsi = tsi;
            }
            self.add_utf8_range(
                overwrite, fsi, to_si, &seq.as_slice()[seq.len()-1]);
        }
    }

    fn add_utf8_range(
        &mut self,
        overwrite: bool,
        from: usize,
        to: usize,
        range: &Utf8Range,
    ) {
        for b in range.start as usize..range.end as usize + 1 {
            if overwrite || self.dfa.states[from].next[b].is_none() {
                self.dfa.states[from].next[b] = Some(to);
            }
        }
    }

    fn new_state(&mut self, is_match: bool) -> usize {
        self.dfa.states.push(State {
            next: [None; 256],
            is_match: is_match,
        });
        self.dfa.states.len() - 1
    }
}
