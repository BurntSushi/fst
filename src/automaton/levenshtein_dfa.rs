use std::fmt;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash,Hasher};
use utf8_ranges::{Utf8Range, Utf8Sequences};
use std::collections::hash_map::Entry;
use crate::automaton::{DynLevenshteinTrait, DynLevenshteinStateTrait};
use std::ops::Deref;
use std::io::{BufWriter, Write};
use std::fs::File;

const STATE_LIMIT: usize = 100_000; // currently at least 20MB >_<

pub fn substr(s: &str, start: usize, length: usize) -> String {
    s.chars().skip(start).take(length).collect()
}

pub fn substr2(s: &str, start: usize) -> String {
    s.chars().skip(start).collect()
}


fn get_byte_name_for_dot_display(slice:&[Utf8Range], cur_index: usize) -> String {
    let mut ret = String::default();
    for i in 0..slice.len() {
        if cur_index == i {
            ret += "[";
            if (slice[i].start == slice[i].end) {
                ret += &*hex::encode_upper(&[slice[i].start]);
            }
            else {
                ret += &*hex::encode_upper(&[slice[i].start]);
                ret += "-";
                ret += &*hex::encode_upper(&[slice[i].end]);
            }
            ret += "]";
        }
        else {
            ret += ".";
        }
    }
    ret
}


///compute levenshtein similarity for result filter and sort
#[inline]
pub fn compute_levenshtein_similarity(edit_distance: usize, min_term_length: i32) -> f64 {
    if edit_distance == 0 {
        1.0_f64
    }
    else if min_term_length == 0 {
        1.0_f64 - edit_distance as f64
    }
    else {
        1.0_f64 - edit_distance as f64 / min_term_length as f64
    }
}

/// An error that occurred while building a Levenshtein automaton.
///
/// This error is only defined when the `levenshtein` crate feature is enabled.
#[derive(Debug)]
pub enum LevenshteinError {
    /// If construction of the automaton reaches some hard-coded limit
    /// on the number of states, then this error is returned.
    ///
    /// The number given is the limit that was exceeded.
    TooManyStates(usize),

    /// other state error
    OtherStateError(),
}

impl fmt::Display for LevenshteinError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            LevenshteinError::TooManyStates(size_limit) => write!(
                f,
                "Levenshtein automaton exceeds size limit of \
                           {} states",
                size_limit
            ),
            LevenshteinError::OtherStateError() => write!(
                f,
                "other levenshtein state error",
            ),
        }
    }
}

impl std::error::Error for LevenshteinError {}


#[derive(Copy,Clone)]
pub struct State {
    pub next: [Option<usize>; 256],
    pub is_match: bool,
    pub edit_distance: Option<usize>
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "State {{")?;
        writeln!(f, "  is_match: {:?}", self.is_match)?;
        for i in 0..256 {
            if let Some(si) = self.next[i] {
                writeln!(f, "  {:?}: {:?}", i, si)?;
            }
        }
        writeln!(f, "  edit_distance: {:?}", self.edit_distance)?;
        write!(f, "}}")
    }
}



/// Dfa
#[derive(Debug)]
pub struct Dfa {
    /// states collections
    pub states: Vec<State>,
}

///Dfa Builder
pub struct DfaBuilder<DynLev: DynLevenshteinTrait> {

    ///dfa result,used to search similar query string later
    pub dfa: Dfa,

    ///dynamic Levenshtein
    pub lev: DynLev,

    /// cache used to construct dfa
    pub cache: HashMap< <DynLev as DynLevenshteinTrait>::DynLevState ,usize>,


    /// writer to draw dot
    pub dot_writer: Option<BufWriter<File>>,
    /// path colors
    pub path_colors: Vec<String>,

    ///current path color index
    pub cur_path_color_index: usize,

}


impl<DynLev: DynLevenshteinTrait> DfaBuilder<DynLev> {
    /// construct method
    pub fn new(lev: DynLev) -> Self {
        DfaBuilder {
            dfa: Dfa { states: Vec::with_capacity(256) },
            lev,
            cache: HashMap::with_capacity(128),

            dot_writer: None,
            path_colors: vec![
                "black".to_string(),
                "red".to_string(),
                "orange".to_string(),
                "green".to_string(),
                "blue".to_string(),
                "magenta".to_string(),
                "cyan".to_string(),
                "brown".to_string(),
                "purple".to_string(),
            ],
            cur_path_color_index: 0,
        }
    }

    /// construct method
    pub fn new_by_dot(lev: DynLev, output_dot_path: String) -> Self {
        let dot_file = File::create(output_dot_path).expect("create dot file error");
        DfaBuilder {
            dfa: Dfa { states: Vec::with_capacity(256) },
            lev,
            cache: HashMap::with_capacity(128),

            dot_writer: Some(BufWriter::new(dot_file)) ,
            path_colors: vec![
                "black".to_string(),
                "red".to_string(),
                "orange".to_string(),
                "green".to_string(),
                "blue".to_string(),
                "magenta".to_string(),
                "cyan".to_string(),
                "brown".to_string(),
                "purple".to_string(),
            ],
            cur_path_color_index: 0,
        }
    }


    /// build dfa method
    pub fn build(mut self) -> Result<Dfa, LevenshteinError> {

        let mut last_si :Option<usize> = None;
        let prefixstr = self.lev.get_prefix_ref().clone();
        let chars_count = prefixstr.chars().count();
        for(i, c) in prefixstr.chars().enumerate() {
            let mut fsi = if i == 0 {
                self.new_state(false)
            }
            else {
                last_si.map(|n|n).unwrap_or(0usize)
            };

            let cur_bytes = c.to_string().into_bytes();
            let cur_bytes_len = cur_bytes.len();
            for j in (0..cur_bytes_len) {
                let cur_byte = cur_bytes[j];
                let tsi = if (j == cur_bytes_len -1) && (i == chars_count-1) {
                    let si = self.cached_state(&self.lev.start());
                    if si.is_none() {
                        return Err(LevenshteinError::OtherStateError());
                    }
                    si
                }
                else {
                    Some(self.new_state(false))
                };
                self.dfa.states[fsi].next[cur_byte as usize] = tsi;
                fsi = tsi.unwrap_or(0);
                last_si = Some(fsi);
            }
        }

        let mut stack = vec![self.lev.start()];
        let mut seen = HashSet::new();

        while let Some(lev_state) = stack.pop() {
            let dfa_si_init = self.cached_state(&lev_state);
            if dfa_si_init.is_none() {
                return Err(LevenshteinError::OtherStateError());
            }
            let dfa_si = dfa_si_init.unwrap();

            //explore transitions
            for c in self.lev.transitions(&lev_state) {
                let lev_next = self.lev.step(&lev_state, Some(c));
                let next_si = self.cached_state(&lev_next);
                next_si.map(|next_si| {
                    self.add_utf8_sequences(false,false, dfa_si, next_si, c, c);
                    if !seen.contains(&next_si) {
                        seen.insert(next_si);
                        stack.push(lev_next);
                    }
                });
            }

            //explore mismatch
            let mismatch = self.add_mismatch_utf8_states(dfa_si, &lev_state);
            mismatch.map(|mismatch| {
                if !seen.contains(&(mismatch.0)) {
                    seen.insert(mismatch.0);
                    stack.push(mismatch.1);
                }
            });

            if self.dfa.states.len() > STATE_LIMIT {
                return Err(LevenshteinError::TooManyStates(STATE_LIMIT));
            }
        }
        Ok(self.dfa)
    }

    fn cached_state(&mut self, lev_state: &<DynLev as DynLevenshteinTrait>::DynLevState ) -> Option<usize> {
        self.cached(&lev_state).map(|(si, _)| si)
    }

    fn cached(&mut self, lev_state: &<DynLev as DynLevenshteinTrait>::DynLevState) -> Option<(usize, bool)> {
        if !self.lev.can_match(lev_state) {
            return None;
        }
        Some(match self.cache.entry(lev_state.clone()) {
            Entry::Occupied(v) => (*v.get(), true),
            Entry::Vacant(v) => {
                let is_match = self.lev.is_match(lev_state);
                self.dfa.states.push(State { next: [None; 256], is_match, edit_distance: lev_state.current_edit_distance(), });
                (*v.insert(self.dfa.states.len() - 1), false)
            }
        })
    }

    fn add_mismatch_utf8_states(
        &mut self,
        from_si: usize,
        lev_state: &<DynLev as DynLevenshteinTrait>::DynLevState,
    ) -> Option<(usize, <DynLev as DynLevenshteinTrait>::DynLevState )> {
        let mismatch_state = self.lev.step(&lev_state,None);
        let to_si = self.cached_state(&mismatch_state);
        to_si.map(|to_si| {
            self.add_utf8_sequences(false, false, from_si, to_si, '\u{0}', '\u{10FFFF}');
            (to_si,mismatch_state)
        })
    }

    fn add_utf8_sequences(
        &mut self,
        first_overwrite: bool,
        second_overwrite: bool,
        from_si: usize,
        to_si: usize,
        from_chr: char,
        to_chr: char,
    ) {
        for seq in Utf8Sequences::new(from_chr, to_chr) {
            let mut from_si_set : HashSet<usize> = HashSet::new();
            from_si_set.insert(from_si);

            for range in &seq.as_slice()[0..seq.len() - 1] {
                let tsi = self.new_state(false);

                let mut new_tosi_set: HashSet<usize> = HashSet::new();
                for fsi in from_si_set.into_iter() {
                    self.add_utf8_range(first_overwrite, fsi, tsi, range, &mut new_tosi_set);
                }
                from_si_set = new_tosi_set;
            }

            let mut new_tosi_set: HashSet<usize> = HashSet::new();
            for fsi in from_si_set.into_iter() {
                self.add_utf8_range(
                    second_overwrite,
                    fsi,
                    to_si,
                    &seq.as_slice()[seq.len() - 1],
                    &mut new_tosi_set,
                );
            }
        }
    }

    fn add_utf8_range(
        &mut self,
        overwrite: bool,
        from: usize,
        to: usize,
        range: &Utf8Range,
        to_si_set: &mut HashSet<usize>,
    )  {
        for b in range.start as usize..range.end as usize + 1 {
            if self.dfa.states[from].next[b].is_none() {
                self.dfa.states[from].next[b] = Some(to);
                to_si_set.insert(to);
            }
            else if overwrite {
                self.dfa.states[from].next[b] = Some(to);
                to_si_set.insert(to);
            }
            else {
                to_si_set.insert(self.dfa.states[from].next[b].unwrap());
            }
        }
    }

    fn new_state(&mut self, is_match: bool) -> usize {
        self.dfa.states.push(State { next: [None; 256], is_match, edit_distance: None });
        self.dfa.states.len() - 1
    }


    /// build dfa method
    pub fn build_by_dot(mut self) -> Result<Dfa, LevenshteinError> {
        self.dot_writer.as_mut().map(|w| w.write_fmt(format_args!("digraph {} {{\n", "dfa")));
        let mut subgraph_index = -1i32;

        let mut last_si :Option<usize> = None;
        let prefixstr = self.lev.get_prefix_ref().clone();
        let chars_count = prefixstr.chars().count();
        for(i, c) in prefixstr.chars().enumerate() {
            let mut fsi = if i == 0 {
                self.new_state(false)
            }
            else {
                last_si.map(|n|n).unwrap_or(0usize)
            };

            let cur_bytes = c.to_string().into_bytes();
            let cur_bytes_len = cur_bytes.len();
            for j in (0..cur_bytes_len) {
                let cur_byte = cur_bytes[j];
                let tsi = if (j == cur_bytes_len -1) && (i == chars_count-1) {
                    let si = self.cached_state(&self.lev.start());
                    if si.is_none() {
                        return Err(LevenshteinError::OtherStateError());
                    }
                    si
                }
                else {
                    Some(self.new_state(false))
                };
                self.dfa.states[fsi].next[cur_byte as usize] = tsi;
                fsi = tsi.unwrap_or(0);
                last_si = Some(fsi);
            }
        }

        let mut stack = vec![self.lev.start()];
        let mut seen = HashSet::new();

        let querystr = self.lev.get_query_ref().clone();
        while let Some(lev_state) = stack.pop() {
            let dfa_si_init = self.cached_state(&lev_state);
            if dfa_si_init.is_none() {
                return Err(LevenshteinError::OtherStateError());
            }
            let dfa_si = dfa_si_init.unwrap();


            subgraph_index += 1;
            let node_shape = match self.lev.is_match(&lev_state) {
                true => "box,style=filled",
                false=> "circle",
            };
            self.dot_writer.as_mut().map(|w| {
                w.write_fmt(format_args!("subgraph cluster_{} {{\n", subgraph_index)).expect("write subgraph error");
                if (subgraph_index == 0) {
                    w.write_fmt(format_args!("label =\"process #{} For:[{}]\";\n", subgraph_index, querystr)).expect("write subgraph error");
                }
                else {
                    w.write_fmt(format_args!("label =\"process #{}\";\n", subgraph_index)).expect("write subgraph error");
                }
                w.write_fmt(format_args!("S{}[shape={}];\n", dfa_si, node_shape)).expect("write node error");
            });


            //explore transitions
            for c in self.lev.transitions(&lev_state) {
                let lev_next = self.lev.step(&lev_state, Some(c));
                let next_si = self.cached_state(&lev_next);
                next_si.map(|next_si| {

                    let node_shape = match self.lev.is_match(&lev_next) {
                        true => "box",
                        false=> "circle",
                    };
                    self.dot_writer.as_mut().map(|w| {
                        w.write_fmt(format_args!("S{}[shape={}];\n", next_si, node_shape)).expect("write node error");
                    });

                    self.add_utf8_sequences_by_dot(false,false, dfa_si, next_si, c, c,true);
                    if !seen.contains(&next_si) {
                        seen.insert(next_si);
                        stack.push(lev_next);
                    }
                });
            }

            //explore mismatch
            let mismatch = self.add_mismatch_utf8_states_by_dot(dfa_si, &lev_state);
            mismatch.map(|mismatch| {
                if !seen.contains(&(mismatch.0)) {
                    seen.insert(mismatch.0);
                    stack.push(mismatch.1);
                }
            });


            self.dot_writer.as_mut().map(|w| w.write_fmt(format_args!("}}\n")));

            if self.dfa.states.len() > STATE_LIMIT {
                return Err(LevenshteinError::TooManyStates(STATE_LIMIT));
            }
        }
        self.dot_writer.as_mut().map(|w| w.write_fmt(format_args!("}}\n")));
        Ok(self.dfa)
    }

    fn add_utf8_sequences_by_dot(
        &mut self,
        is_prev_overwrite: bool,
        is_last_overwrite: bool,
        from_si: usize,
        to_si: usize,
        from_chr: char,
        to_chr: char,
        draw_dot: bool,
    ) {
        let mut dot_line_style = "solid";
        for seq in Utf8Sequences::new(from_chr, to_chr) {
            let mut from_si_set : HashSet<usize> = HashSet::new();
            from_si_set.insert(from_si);

            self.cur_path_color_index += 1;
            if self.cur_path_color_index == self.path_colors.len() {
                self.cur_path_color_index = 0;
            }
            let path_color = &self.path_colors[self.cur_path_color_index].clone();

            let mut path_name = String::new();
            if from_chr == to_chr {
                path_name = from_chr.to_string();
                dot_line_style = "solid";
            }
            else {
                path_name = String::from("*") + &*hex::encode_upper(&[seq.as_slice()[0].start]) + "," + &*seq.len().to_string();
                dot_line_style = "dashed";
            }
            let slice = &seq.as_slice()[0..seq.len()-1];
            let full_slice = &seq.as_slice();
            for index in 0..slice.len() {
                let byte_name = get_byte_name_for_dot_display(&full_slice,index);
                let range = &slice[index];
                let tsi = self.new_state(false);

                if draw_dot {
                    self.dot_writer.as_mut().map(|w| {
                        w.write_fmt(format_args!("S{}[shape=plaintext];\n", tsi)).expect("write node error");
                    });
                }

                let mut new_to_si_set: HashSet<usize> = HashSet::new();
                for fsi in from_si_set.into_iter() {
                    self.add_utf8_range(is_prev_overwrite, fsi, tsi, range, &mut new_to_si_set);
                    if draw_dot {
                        self.dot_writer.as_mut().map(|w| {
                            for newto in &new_to_si_set {
                                w.write_fmt(format_args!("S{} -> S{} [color={}, constrant=true, label=<<font color='{}'>{}/{}</font>>, style={}];\n",
                                                         fsi, newto, path_color, path_color, path_name,byte_name, dot_line_style)).expect("write virtual path error");
                            }
                        });
                    }
                }
                from_si_set = new_to_si_set;
            }

            let byte_name = get_byte_name_for_dot_display(&full_slice,seq.len()-1);
            let mut new_to_si_set: HashSet<usize> = HashSet::new();
            for fsi in from_si_set.into_iter() {
                self.add_utf8_range(
                    is_last_overwrite,
                    fsi,
                    to_si,
                    &seq.as_slice()[seq.len() - 1],
                    &mut new_to_si_set,
                );

                if draw_dot {
                    self.dot_writer.as_mut().map(|w| {
                        w.write_fmt(format_args!("S{} -> S{} [color={}, constrant=true, label=<<font color='{}'>{}/{}</font>>, style={}];\n",
                                                 fsi, to_si, path_color, path_color, path_name,byte_name, dot_line_style)).expect("write virtual path error");
                    });
                }
            }
        }
    }

    fn add_mismatch_utf8_states_by_dot(
        &mut self,
        from_si: usize,
        lev_state: &<DynLev as DynLevenshteinTrait>::DynLevState,
    ) -> Option<(usize, <DynLev as DynLevenshteinTrait>::DynLevState )> {
        let mismatch_state = self.lev.step(&lev_state,None);
        let to_si = self.cached_state(&mismatch_state);
        to_si.map(|to_si| {
            self.add_utf8_sequences_by_dot(false, false, from_si, to_si, '\u{0}', '\u{10FFFF}',true);
            (to_si,mismatch_state)
        })
    }


}
