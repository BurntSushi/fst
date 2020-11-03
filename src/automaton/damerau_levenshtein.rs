use crate::automaton::{DamerauDynLevenshtein, Dfa, LevenshteinError, DfaBuilder, DynLevenshteinTrait};
use std::cmp::min;
use std::fmt;
use crate::Automaton;
use crate::inner_automaton::levenshtein_dfa::{substr2, substr, compute_levenshtein_similarity};
use std::collections::HashMap;
use crate::inner_automaton::DamerauDynLevState;

/// A Unicode aware Levenshtein automaton for running efficient fuzzy queries.
///
/// This is only defined when the `levenshtein` crate feature is enabled.
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
/// use fst::automaton::DamerauLevenshtein;
/// use fst::{IntoStreamer, Streamer, Set};
///
/// fn main() {
///     let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
///     let set = Set::from_iter(keys).unwrap();
///
///     let lev = DamerauLevenshtein::new("foo", 1,0,0).unwrap();
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
pub struct DamerauLevenshtein {
    /// Damerau dynamic levenshtein
    pub prog: DamerauDynLevenshtein,

    /// dfa
    pub dfa: Dfa,
}

impl DamerauLevenshtein {
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
    ///
    ///
    /// under transpostions case, the last two characters are transpositions equically, it means '1' edit distance, else it means '2' edit distance.
    ///
    /// params -- prefixlen_non_similarity:
    ///                                   it may be the field name prefix for query term,such as length of 'title_' for term:'titile_望京花园西区', it can be 0.
    /// params -- prefixlen_similarity:
    ///                                  the prefix substr  for query term value,such as length of '望京' for term:'titile_望京花园西区',
    ///                                  it also can be 0.
    ///                                  NOTE that  only prefixlen_similarity will be used to compute levenshtein similarity
    #[inline]
    pub fn new(query: &str, distance: u32, prefixlen_non_similarity: u32, prefixlen_similarity: u32)
        -> Result<DamerauLevenshtein, LevenshteinError> {

        let mut prefixlen_nonsim = prefixlen_non_similarity;
        let mut prefixlen_sim = prefixlen_similarity;
        let char_count = query.chars().count() as u32;

        if prefixlen_nonsim > char_count {
            prefixlen_nonsim  = 0;
        }
        if prefixlen_nonsim + prefixlen_sim > char_count {
            prefixlen_sim = char_count - prefixlen_nonsim;
        }
        let prefixlen: usize = prefixlen_nonsim as usize + prefixlen_sim as usize;

        let lev = DamerauDynLevenshtein::new(
            substr2(&query, prefixlen),
            min(distance as usize, 2 ),
            substr(&query, 0,prefixlen),
            prefixlen_nonsim,
            prefixlen_sim,
        );
        let dfa = DfaBuilder::new(lev.clone()).build()?;
        Ok(DamerauLevenshtein{ prog: lev, dfa })
    }



    /// new by draw dot drawing file
    #[inline]
    pub fn new_by_dot(query: &str, distance: u32, prefixlen_non_similarity: u32, prefixlen_similarity: u32, output_dot_path: String) -> Result<DamerauLevenshtein, LevenshteinError> {

        let mut prefixlen_nonsim = prefixlen_non_similarity;
        let mut prefixlen_sim = prefixlen_similarity;
        let char_count = query.chars().count() as u32;

        if prefixlen_nonsim > char_count {
            prefixlen_nonsim  = 0;
        }
        if prefixlen_nonsim + prefixlen_sim > char_count {
            prefixlen_sim = char_count - prefixlen_nonsim;
        }
        let prefixlen: usize = prefixlen_nonsim as usize + prefixlen_sim as usize;

        let lev = DamerauDynLevenshtein::new(
            substr2(&query, prefixlen),
            min(distance as usize, 2 ),
            substr(&query, 0,prefixlen),
            prefixlen_nonsim,
            prefixlen_sim,
        );
        let dfa = DfaBuilder::new_by_dot(lev.clone(),output_dot_path).build_by_dot()?;
        Ok(DamerauLevenshtein{ prog: lev, dfa })
    }

}

impl fmt::Debug for DamerauLevenshtein {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Levenshtein(query: {:?}, edit_distance: {:?})",
            self.prog.get_query_ref(), self.prog.get_edit_distance(),
        )
    }
}


impl Automaton for DamerauLevenshtein {
    /// (状态机的状态序号，莱温斯坦编辑距离)
    type State = Option<(usize, Option<usize>)>;


    /// overwrite to implement to compute levenshtein similarity
    #[inline]
    fn levenshtein_compute_similarity(&self, state: &Self::State, matched_term_char_count: i32) -> Option<(usize,f64)> {
        if state.is_none() || state.unwrap().1.is_none() {
            None
        } else {
            let edit_dist = state.unwrap().1.unwrap();

            let query_len = self.prog.query.chars().count() as i32;
            let min_len = min(query_len + self.prog.prefixlen_similarity as i32, matched_term_char_count - self.prog.prefixlen_non_similarity as i32);
            let similarity = compute_levenshtein_similarity(edit_dist, min_len);

            Some((edit_dist, similarity))
        }
    }

    #[inline]
    fn start(&self) -> Self::State {
        Some((0,self.dfa.states[0].edit_distance))
    }

    #[inline]
    fn is_match(&self, state: &Self::State) -> bool {
        state.map(|state| self.dfa.states[state.0].is_match).unwrap_or(false)
    }

    #[inline]
    fn can_match(&self, state: &Self::State) -> bool {
        state.is_some()
    }

    #[inline]
    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        if state.is_none() {
            return None;
        }
        let next_value = self.dfa.states[state.unwrap().0].next[byte as usize];
        if next_value.is_none() {
            return None;
        }
        let edit_distance = self.dfa.states[next_value.unwrap()].edit_distance;
        Some(  (next_value.unwrap(), edit_distance)  )
    }
}




