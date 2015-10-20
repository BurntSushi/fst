#![feature(test)]

extern crate fst;
extern crate test;

use std::collections::{BTreeSet, HashSet};

use fst::raw::{Builder, Fst};
use test::Bencher;

const WORDS: &'static str = include_str!("./../data/words-10000");

fn get_words() -> Vec<String> {
    WORDS.lines().map(|s| s.to_owned()).collect()
}

#[bench]
fn search_fst_one(b: &mut Bencher) {
    let words = get_words();
    let mut bfst = Builder::memory();
    for word in &words {
        bfst.add(word).unwrap();
    }
    let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
    let mut i = 0;
    b.iter(|| {
        i = (i + 1) % words.len();
        assert!(fst.find(&words[i]).is_some());
    })
}

#[bench]
fn search_hash_one(b: &mut Bencher) {
    let words = get_words();
    let set: HashSet<String> = words.clone().into_iter().collect();
    let mut i = 0;
    b.iter(|| {
        i = (i + 1) % words.len();
        assert!(set.contains(&words[i]));
    })
}

#[bench]
fn search_btree_one(b: &mut Bencher) {
    let words = get_words();
    let set: BTreeSet<String> = words.clone().into_iter().collect();
    let mut i = 0;
    b.iter(|| {
        i = (i + 1) % words.len();
        assert!(set.contains(&words[i]));
    })
}
