use std::collections::{HashSet, HashMap, BTreeSet};
use std::cmp;
use std::hash::{Hash,Hasher};
use std::collections::hash_map::Entry;

/// Dynamic Levenshtein State Trait
pub trait DynLevenshteinStateTrait {
    /// obtain the edit distance of current state
    fn current_edit_distance(&self) -> Option<usize>;
}

/// Common Dynamic Levenshtein State Trait, that is not damerau levenshtein
#[derive(Clone,Hash,Eq,PartialEq)]
pub struct CommonDynLevState {
    /// current state
    pub cur_state:  Vec<usize>,
}

///implementation DynLevenshteinStateTrait  for CommonDynLevState
impl DynLevenshteinStateTrait for CommonDynLevState {
    /// obtain the edit distance of current state
    fn current_edit_distance(&self) -> Option<usize>{
        self.cur_state.last().map(|&n|n)
    }
}

///Dynamic Levenshtein Trait
pub trait DynLevenshteinTrait {
    /// State
    type DynLevState : Eq + PartialEq + Hash + DynLevenshteinStateTrait + Clone;

    /// query reference
    fn get_query_ref(&self) ->&String;
    ///edit distance
    fn get_edit_distance(&self) -> usize;

    ///prefix string
    fn get_prefix_ref(&self) ->&String;

    ///length of prefix string non-related to similarity
    fn get_prefixlen_non_similarity(&self) -> u32;

    ///length of prefix string related to similarity
    fn get_prefixlen_similarity(&self) -> u32;

    ///initial state
    fn start(&self) -> Self::DynLevState;

    /// check the state whether match
    fn is_match(&self, state: &Self::DynLevState) -> bool;
    /// check current path whether can continue to seek match
    fn can_match(&self, state: &Self::DynLevState) -> bool;
    /// step function
    fn step(&self, state: &Self::DynLevState, chr: Option<char> ) -> Self::DynLevState;

    /// obtain possible character set from the state
    fn transitions(&self, state: &Self::DynLevState) -> BTreeSet<char>;
}



/// Common Dynamic Levenshtein ,not damerau levenshtein
#[derive(Clone)]
pub struct CommonDynLevenshtein {
    ///query string
    pub query: String,
    /// max edit distance
    pub edit_distance: usize,
    ///prefix string
    pub prefix: String,

    ///length of prefix string non-related to similarity
    pub prefixlen_non_similarity: u32,

    ///length of prefix string related to similarity
    pub prefixlen_similarity: u32,
}

/// CommonDynLevenshtein implementation
impl CommonDynLevenshtein {
    ///construct function
    pub fn new(query: String, edit_distance:usize, prefix: String, prefixlen_non_similarity: u32, prefixlen_similarity: u32,) -> Self {
        CommonDynLevenshtein {
            query,
            edit_distance,
            prefix,
            prefixlen_non_similarity,
            prefixlen_similarity,
        }
    }
}

/// impl DynLevenshteinTrait for CommonDynLevenshtein
impl DynLevenshteinTrait for CommonDynLevenshtein {
    ///State
    type DynLevState = CommonDynLevState;

    /// query reference
    fn get_query_ref(&self) ->&String {
        &self.query
    }
    ///edit distance
    fn get_edit_distance(&self) -> usize {
        self.edit_distance
    }
    ///prefix string
    fn get_prefix_ref(&self) ->&String {
        &self.prefix
    }

    ///length of prefix string related to similarity
    fn get_prefixlen_similarity(&self) -> u32 {
        self.prefixlen_similarity
    }

    ///length of prefix string non-related to similarity
    fn get_prefixlen_non_similarity(&self) -> u32 {
        self.prefixlen_non_similarity
    }

    ///initial state
    #[inline]
    fn start(&self) -> Self::DynLevState {
        CommonDynLevState {
            cur_state: (0..self.query.chars().count() + 1).into_iter().map(|n| cmp::min(n,self.edit_distance+1)).collect()
        }
    }

    /// check the state whether match
    #[inline]
    fn is_match(&self, state: &Self::DynLevState) -> bool {
        state.cur_state.last().map(|&n| n <= self.edit_distance).unwrap_or(false)
    }

    /// check current path whether can continue to seek match
    #[inline]
    fn can_match(&self, state: &Self::DynLevState) -> bool {
        state.cur_state.iter().min().map(|&n| n <= self.edit_distance).unwrap_or(false)
    }

    /// step function
    #[inline]
    fn step(&self, state: &Self::DynLevState, chr: Option<char> ) -> Self::DynLevState {
        let mut next = vec![cmp::min(state.cur_state[0] + 1, self.edit_distance+1)];
        for(i, c) in self.query.chars().enumerate() {
            let cost = if Some(c) == chr { 0 } else { 1 };
            let v = cmp::min( cmp::min(next[i]+1, state.cur_state[i+1] + 1), state.cur_state[i] + cost );
            next.push(cmp::min(v, self.edit_distance + 1));
        }
        CommonDynLevState {
            cur_state: next
        }
    }

    /// obtain possible character set from the state
    #[inline]
    fn transitions(&self, state: &Self::DynLevState) -> BTreeSet<char> {
        let mut result_chars = BTreeSet::new();
        for(i, c) in self.query.chars().enumerate() {
            if state.cur_state[i] <= self.edit_distance {
                result_chars.insert(c);
            }
        }
        result_chars
    }
}


/// Damerau levenshtein, which support transpositions
#[derive(Clone)]
pub struct DamerauDynLevState {
    query: String,
    edit_distance:  usize,
    cur_state:  Vec<usize>,
    prev_char:  Option<char>,
    prev_state: Option<Vec<usize> >,
}


/// implement DamerauDynLeveState
impl DamerauDynLevState {
    fn _is_possible_transposition_(&self) -> bool {
        let mut need_transpositions = false;
        let mut last_char: char = 'a';
        for(i, c) in self.query.chars().enumerate() {
            if (i > 0 ) && (c != last_char) && (self.prev_char.map(|ch| ch == c).unwrap_or(false) ) {
                let prev_llast_state = self.prev_state.as_ref().map(|st| st[i-1]).unwrap_or(0);
                let cur_last_state = self.cur_state[i];
                let cur_cur_state = self.cur_state[i+1];

                if (prev_llast_state < self.edit_distance) && (prev_llast_state < cur_last_state) && (prev_llast_state < cur_cur_state) {
                    need_transpositions = true;
                    break;
                }
            }
            last_char = c;
        }
        need_transpositions
    }

    fn _possible_transposition_chars_(&self) -> HashSet<char> {
        let mut result_chars : HashSet<char> = HashSet::with_capacity(16);
        let mut last_char: char = 'a';
        for(i, c) in self.query.chars().enumerate() {
            if (i > 0 ) && (c != last_char) && (self.prev_char.map(|ch| ch == c).unwrap_or(false) ) {
                let prev_llast_state = self.prev_state.as_ref().map(|st| st[i-1]).unwrap_or(0);
                let cur_last_state = self.cur_state[i];
                let cur_cur_state = self.cur_state[i+1];

                if (prev_llast_state < self.edit_distance) && (prev_llast_state < cur_last_state) && (prev_llast_state < cur_cur_state) {
                    result_chars.insert(last_char);
                }
            }
            last_char = c;
        }
        result_chars
    }
}

impl DynLevenshteinStateTrait for DamerauDynLevState {
    fn current_edit_distance(&self) -> Option<usize>{
        self.cur_state.last().map(|&n|n)
    }
}



impl Eq for DamerauDynLevState {

}


impl PartialEq for DamerauDynLevState {
    fn eq(&self, other: &DamerauDynLevState) -> bool {
        if self.cur_state != other.cur_state {
            return false;
        }
        let in1 = self.prev_char.map(|ch| self.query.contains(ch) ).unwrap_or(false);
        let in2 = other.prev_char.map(|ch| self.query.contains(ch) ).unwrap_or(false);
        return if in1 != in2 {
            false
        } else if false == in1 {
            true
        } else if self.prev_char.unwrap() != other.prev_char.unwrap() {
            false
        } else if self._is_possible_transposition_() || other._is_possible_transposition_() {
            self.prev_state.as_ref().unwrap_or(&vec![]) == other.prev_state.as_ref().unwrap_or(&vec![])
        } else {
            true
        }
    }
}

impl Hash for DamerauDynLevState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cur_state.hash(state);
    }
}



/// Damerau Dynamic Levenshtein
#[derive(Clone)]
pub struct DamerauDynLevenshtein {
    ///query string
    pub query: String,
    /// max edit distance
    pub edit_distance: usize,
    ///prefix string
    pub prefix: String,

    ///length of prefix string non-related to similarity
    pub prefixlen_non_similarity: u32,

    ///length of prefix string related to similarity
    pub prefixlen_similarity: u32,
}

///impl DamerauDynLevenshtein
impl DamerauDynLevenshtein {
    ///construct function
    pub fn new(query: String, edit_distance:usize, prefix: String,  prefixlen_non_similarity: u32, prefixlen_similarity: u32) -> Self {
        DamerauDynLevenshtein {
            query,
            edit_distance,
            prefix,
            prefixlen_non_similarity,
            prefixlen_similarity,
        }
    }
}

impl DynLevenshteinTrait for DamerauDynLevenshtein {
    type DynLevState = DamerauDynLevState;

    /// query reference
    fn get_query_ref(&self) ->&String {
        &self.query
    }
    ///edit distance
    fn get_edit_distance(&self) -> usize {
        self.edit_distance
    }
    ///prefix string
    fn get_prefix_ref(&self) ->&String {
        &self.prefix
    }

    ///length of prefix string non-related to similarity
    fn get_prefixlen_non_similarity(&self) -> u32 {
        self.prefixlen_non_similarity
    }


    ///length of prefix string related to similarity
    fn get_prefixlen_similarity(&self) -> u32 {
        self.prefixlen_similarity
    }


    ///initial state
    #[inline]
    fn start(&self) -> DamerauDynLevState {
        let st : Vec<usize> = (0..self.query.chars().count() + 1).into_iter().map(|n| cmp::min(n,self.edit_distance+1)).collect();
        DamerauDynLevState {
            query:  self.query.clone(),
            edit_distance: self.edit_distance,
            cur_state: st,
            prev_char:  None,
            prev_state: None,
        }
    }

    /// check the state whether match
    #[inline]
    fn is_match(&self, state: &DamerauDynLevState) -> bool {
        state.cur_state.last().map(|&n| n <= self.edit_distance).unwrap_or(false)
    }

    /// check current path whether can continue to seek match
    #[inline]
    fn can_match(&self, state: &DamerauDynLevState) -> bool {
        state.cur_state.iter().min().map(|&n| n <= self.edit_distance).unwrap_or(false)
    }

    /// step function
    #[inline]
    fn step(&self, state: &DamerauDynLevState, chr: Option<char> ) -> DamerauDynLevState {
        let mut next = vec![cmp::min(state.cur_state[0] + 1, self.edit_distance + 1)];

        let mut last_char: char = 'a';
        for(i, c) in self.query.chars().enumerate() {
            let cost = if Some(c) == chr { 0 } else { 1 };
            let mut cur_min = cmp::min( cmp::min(next[i]+1, state.cur_state[i+1] + 1), state.cur_state[i] + cost );

            if (i >0) && (state.prev_char.is_some()) && (Some(last_char) == chr) && (c == state.prev_char.unwrap_or('a')) {
                cur_min = cmp::min(cur_min, state.prev_state.as_ref().map(|st|st[i-1]).unwrap_or(self.edit_distance) + 1);
            }
            next.push(cmp::min(cur_min, self.edit_distance + 1));

            last_char = c;
        }

        DamerauDynLevState {
            query:  self.query.clone(),
            edit_distance:  self.edit_distance,
            cur_state:  next,
            prev_char:  chr,
            prev_state: Some(state.cur_state.clone()),
        }
    }

    /// obtain possible character set from the state
    #[inline]
    fn transitions(&self, state: &DamerauDynLevState) -> BTreeSet<char> {
        let mut result_chars = BTreeSet::new();
        let possible_trans_chars = state.prev_state.as_ref().map(|_|state._possible_transposition_chars_()).unwrap_or(HashSet::with_capacity(1));

        for(i, c) in self.query.chars().enumerate() {
            if (possible_trans_chars.contains(&c)) || (state.cur_state[i] <= self.edit_distance) {
                result_chars.insert(c);
            }
        }
        result_chars
    }
}
