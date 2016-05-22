#![feature(test)]

extern crate fnv;
extern crate fst;
#[macro_use] extern crate lazy_static;
extern crate test;

const STR_WORDS: &'static str = include_str!("./../data/words-100000");
const STR_WIKI_URLS: &'static str = include_str!("./../data/wiki-urls-100000");

fn get_keys(s: &'static str) -> Vec<String> {
    s.lines().map(str::to_owned).collect()
}

lazy_static! {
    static ref WORDS: Vec<String> = get_keys(STR_WORDS);
    static ref WIKI_URLS: Vec<String> = get_keys(STR_WIKI_URLS);
}

macro_rules! search {
    ($name:ident, $keys:expr) => {
        mod $name {
            use std::collections::{BTreeSet, HashSet};
            use std::hash::BuildHasherDefault;

            use fnv::FnvHasher;
            use fst::raw::{Builder, Fst};
            use test::Bencher;

            #[bench]
            fn fst_contains(b: &mut Bencher) {
                lazy_static! {
                    static ref FST: Fst = {
                        let mut bfst = Builder::memory();
                        for word in $keys.iter() {
                            bfst.add(word).unwrap();
                        }
                        let bytes = bfst.into_inner().unwrap();
                        Fst::from_bytes(bytes).unwrap()
                    };
                }
                let mut i = 0;
                b.iter(|| {
                    i = (i + 1) % $keys.len();
                    assert!(FST.contains_key(&$keys[i]));
                })
            }

            #[bench]
            fn hash_fnv_contains(b: &mut Bencher) {
                type Fnv = BuildHasherDefault<FnvHasher>;
                lazy_static! {
                    static ref SET: HashSet<String, Fnv> = {
                        $keys.clone().into_iter().collect()
                    };
                }
                let mut i = 0;
                b.iter(|| {
                    i = (i + 1) % $keys.len();
                    assert!(SET.contains(&$keys[i]));
                })
            }

            #[bench]
            fn hash_sip_contains(b: &mut Bencher) {
                lazy_static! {
                    static ref SET: HashSet<String> = {
                        $keys.clone().into_iter().collect()
                    };
                }
                let mut i = 0;
                b.iter(|| {
                    i = (i + 1) % $keys.len();
                    assert!(SET.contains(&$keys[i]));
                })
            }

            #[bench]
            fn btree_contains(b: &mut Bencher) {
                lazy_static! {
                    static ref SET: BTreeSet<String> = {
                        $keys.clone().into_iter().collect()
                    };
                }
                let mut i = 0;
                b.iter(|| {
                    i = (i + 1) % $keys.len();
                    assert!(SET.contains(&$keys[i]));
                })
            }
        }
    }
}

search!(words, ::WORDS);
search!(wiki_urls, ::WIKI_URLS);
