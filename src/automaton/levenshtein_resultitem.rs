use std::cmp::{Ord,Ordering};
use libc::{c_char, c_uint};
use std::ptr::null_mut;
use std::ffi::CString;
use crate::raw::Output;
use std::{fmt, cmp};


/// define Levenshtein result item to compare
#[derive(Debug)]
pub struct LevenshteinResultItem {
    input: String,
    output: Output,
    edit_distance: usize,
    similarity: f64,
}

/// Levenshtein result time to compare
impl LevenshteinResultItem {
    /// construct LevenshteinResultItem object from params
    pub fn new(input: &String, output: &Output, edit_distance:usize,  similarity: f64) -> Self {
        LevenshteinResultItem {input: input.to_owned() , output: output.to_owned(), edit_distance, similarity}
    }

    /// obtain input member
    pub fn get_input(&self) -> String {
        self.input.to_owned()
    }

    /// obtain output member
    pub fn get_output(&self) -> Output{
        self.output.to_owned()
    }

    /// obtain edit_distance member
    pub fn get_edit_distance(&self) -> usize {
        self.edit_distance
    }

    /// obtain similarity member
    pub fn get_similarity(&self) -> f64{
        self.similarity
    }
}

/// judge whether two float64 numbers are nearly equal within tolerance
#[inline]
fn is_float_nearly_equal(v1: f64, v2: f64, toler: f64 ) -> bool {
    let diff: f64 = v1 - v2;
    if diff <= toler  && diff >= -1.0 * toler {
        return true;
    }
    return false;
}

impl Ord for LevenshteinResultItem{
    fn cmp(&self, other: &LevenshteinResultItem) -> cmp::Ordering {
        return if is_float_nearly_equal(other.similarity, self.similarity, 0.000001) {
            self.input.cmp(&other.input)
        } else if self.similarity < other.similarity {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}
impl PartialOrd for LevenshteinResultItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for LevenshteinResultItem {
}
impl PartialEq for LevenshteinResultItem {
    fn eq(&self, other: &LevenshteinResultItem) -> bool {
        self.input.eq(&other.input)
    }
}


/// define Levenshtein results as search results for fuzzy search
pub struct FstLevenshteinFuzzySearchResults {
    search_results: Vec<LevenshteinResultItem>,
}

/// implementation for FstLevenshteinFuzzySearchResults used to return for fuzzy search
impl FstLevenshteinFuzzySearchResults {

    /// construct method
    #[inline]
    pub fn new(search_results: Vec<LevenshteinResultItem>)-> Self {
        FstLevenshteinFuzzySearchResults { search_results }
    }

    /// length or search_results
    #[inline]
    pub fn len(&self)-> usize {
        self.search_results.len()
    }

    /// get search_results reference
    #[inline]
    pub fn get_search_results(&self) -> &Vec<LevenshteinResultItem> {
        return &self.search_results;
    }

    /// get input of search results on giving index
    #[inline]
    pub fn get_input(&self, index: usize) ->*mut c_char {
        match self.search_results.get(index) {
            Some(x) => {
                let c_str = CString::new(x.input.clone()).unwrap();
                c_str.into_raw()
            },
            None => null_mut(),
        }
    }

    /// get output of search results on giving index
    #[inline]
    pub fn get_output(&self, index: usize) -> u64 {
        match self.search_results.get(index) {
            Some(x) => x.output.value(),
            None => 0 ,
        }
    }

    /// get edit_distance of search results on giving index
    #[inline]
    pub fn get_edit_distance(&self, index: usize) -> usize {
        match self.search_results.get(index) {
            Some(x) => x.edit_distance,
            None => 0 ,
        }
    }

    /// get similarity of search results on giving index
    #[inline]
    pub fn get_similarity(&self, index: usize) -> f64 {
        match self.search_results.get(index) {
            Some(x) => x.similarity,
            None => 0.0 as f64,
        }
    }
}

impl fmt::Debug for FstLevenshteinFuzzySearchResults {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "FstLevenshteinFuzzySearchResults {{")?;
        writeln!(f, "  search_results length: {:?}", self.len())?;
        writeln!(f, "  search_results:")?;
        for x in &self.search_results {
            writeln!(f, "              input: {:?} /output: {:?} /edit_distance: {:?} /similarity: {:?}", &x.input, &x.output, &x.edit_distance, &x.similarity)?;
        }
        write!(f, "}}")
    }
}
