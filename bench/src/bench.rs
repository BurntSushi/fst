#![allow(warnings)]

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::iter::FromIterator;
use std::sync::Arc;
use std::time::Duration;

use fnv::FnvHashSet;
use fst::raw::{Builder, Fst};

use criterion::{
    criterion_group, criterion_main, Bencher, Benchmark, Criterion, Throughput,
};

const WORDS: &str = include_str!("../../data/words-10000");
const WORDS_BIG: &str = include_str!("../../data/words-100000");
const WIKI_URLS: &str = include_str!("../../data/wiki-urls-100000");

fn all(c: &mut Criterion) {
    build(c);
    search(c);
}

fn build(c: &mut Criterion) {
    define_corpus(c, "build", "fst/set", WORDS.as_bytes(), move |b| {
        let words = get_words();
        b.iter(|| {
            let set = fst::Set::from_iter(&words).unwrap();
            assert_eq!(words.len(), set.len());
        });
    });

    define_corpus(c, "build", "fst/map", WORDS.as_bytes(), move |b| {
        let words = get_words_outputs();
        b.iter(|| {
            let map = fst::Map::from_iter(words.iter().cloned()).unwrap();
            assert_eq!(words.len(), map.len());
        });
    });

    define_corpus(c, "build", "hash/set", WORDS.as_bytes(), move |b| {
        let words = get_words();
        b.iter(|| {
            let mut set = HashSet::new();
            for word in &words {
                set.insert(word);
            }
            assert_eq!(words.len(), set.len());
        });
    });

    define_corpus(c, "build", "hash/map", WORDS.as_bytes(), move |b| {
        let words = get_words_outputs();
        b.iter(|| {
            let mut map = HashMap::new();
            for &(ref word, len) in &words {
                map.insert(word, len);
            }
            assert_eq!(words.len(), map.len());
        });
    });

    define_corpus(c, "build", "btree/set", WORDS.as_bytes(), move |b| {
        let words = get_words();
        b.iter(|| {
            let mut set = BTreeSet::new();
            for word in &words {
                set.insert(word);
            }
            assert_eq!(words.len(), set.len());
        });
    });

    define_corpus(c, "build", "btree/map", WORDS.as_bytes(), move |b| {
        let words = get_words_outputs();
        b.iter(|| {
            let mut map = BTreeMap::new();
            for &(ref word, len) in &words {
                map.insert(word, len);
            }
            assert_eq!(words.len(), map.len());
        });
    });
}

fn search(c: &mut Criterion) {
    let lists = &[("words", WORDS_BIG), ("wikiurls", WIKI_URLS)];

    for &(name, list) in lists {
        let group = format!("search/{}", name);
        define(c, &group, "fst/contains", move |b| {
            let keys = get_keys(list);
            let set = fst::Set::from_iter(&keys).unwrap();

            let mut i = 0;
            b.iter(|| {
                assert!(set.contains(&keys[i]));
                i = (i + 1) % keys.len();
            });
        });
    }
    for &(name, list) in lists {
        let group = format!("search/{}", name);
        define(c, &group, "btree/contains", move |b| {
            let keys = get_keys(list);
            let set = BTreeSet::from_iter(&keys);

            let mut i = 0;
            b.iter(|| {
                assert!(set.contains(&keys[i]));
                i = (i + 1) % keys.len();
            });
        });
    }
    for &(name, list) in lists {
        let group = format!("search/{}", name);
        define(c, &group, "hash/contains", move |b| {
            let keys = get_keys(list);
            let set: HashSet<&String> = HashSet::from_iter(&keys);

            let mut i = 0;
            b.iter(|| {
                assert!(set.contains(&keys[i]));
                i = (i + 1) % keys.len();
            });
        });
    }
    for &(name, list) in lists {
        let group = format!("search/{}", name);
        define(c, &group, "hashfnv/contains", move |b| {
            let keys = get_keys(list);
            let set = FnvHashSet::from_iter(&keys);

            let mut i = 0;
            b.iter(|| {
                assert!(set.contains(&keys[i]));
                i = (i + 1) % keys.len();
            });
        });
    }
}

fn get_words() -> Vec<String> {
    WORDS.lines().map(|s| s.to_owned()).collect()
}

fn get_words_outputs() -> Vec<(String, u64)> {
    WORDS.lines().map(|s| (s.to_owned(), s.len() as u64)).collect()
}

fn get_keys(s: &str) -> Vec<String> {
    s.lines().map(str::to_owned).collect()
}

fn define(
    c: &mut Criterion,
    group_name: &str,
    bench_name: &str,
    bench: impl FnMut(&mut Bencher) + 'static,
) {
    let benchmark = Benchmark::new(bench_name, bench)
        .sample_size(50)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(3));
    c.bench(group_name, benchmark);
}

fn define_corpus(
    c: &mut Criterion,
    group_name: &str,
    bench_name: &str,
    corpus: &[u8],
    bench: impl FnMut(&mut Bencher) + 'static,
) {
    let tput = Throughput::Bytes(corpus.len() as u64);
    let benchmark = Benchmark::new(bench_name, bench)
        .throughput(tput)
        .sample_size(30)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(5));
    c.bench(group_name, benchmark);
}

criterion_group!(g, all);
criterion_main!(g);
