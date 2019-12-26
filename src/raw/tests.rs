use automaton::AlwaysMatch;
use error::Error;
use raw::{self, VERSION, Builder, Bound, Fst, Stream, Output};
use stream::Streamer;
use std::ops::Deref;

const TEXT: &'static str = include_str!("./../../data/words-100000");

pub fn fst_set<I, S>(ss: I) -> Fst
        where I: IntoIterator<Item=S>, S: AsRef<[u8]> {
    let mut bfst = Builder::memory();
    let mut ss: Vec<Vec<u8>> =
        ss.into_iter().map(|s| s.as_ref().to_vec()).collect();
    ss.sort();
    for s in ss.iter().into_iter() {
        bfst.add(s).unwrap();
    }
    let fst = Fst::new(bfst.into_inner().unwrap()).unwrap();
    ss.dedup();
    assert_eq!(fst.len(), ss.len());
    fst
}

pub fn fst_map<I, S>(ss: I) -> Fst
        where I: IntoIterator<Item=(S, u64)>, S: AsRef<[u8]> {
    let mut bfst = Builder::memory();
    let mut ss: Vec<(Vec<u8>, u64)> =
        ss.into_iter().map(|(s, o)| (s.as_ref().to_vec(), o)).collect();
    ss.sort();
    ss.dedup();
    for (s, o) in ss.into_iter() {
        bfst.insert(s, o).unwrap();
    }
    Fst::new(bfst.into_inner().unwrap()).unwrap()
}

pub fn fst_inputs(fst: &Fst) -> Vec<Vec<u8>> {
    let mut words = vec![];
    let mut rdr = fst.stream();
    while let Some((word, _)) = rdr.next() {
        words.push(word.to_vec());
    }
    words
}

pub fn fst_inputs_outputs(fst: &Fst) -> Vec<(Vec<u8>, u64)> {
    let mut words = vec![];
    let mut rdr = fst.stream();
    while let Some((word, out)) = rdr.next() {
        words.push((word.to_vec(), out.value()));
    }
    words
}

macro_rules! test_set {
    ($name:ident, $($s:expr),+) => {
        #[test]
        fn $name() {
            let mut items = vec![$($s),*];
            let fst = fst_set(&items);
            let mut rdr = fst.stream();
            items.sort();
            items.dedup();
            for item in &items {
                assert_eq!(rdr.next().unwrap().0, item.as_bytes());
            }
            assert_eq!(rdr.next(), None);
            for item in &items {
                assert!(fst.get(item).is_some());
            }
        }
    }
}

macro_rules! test_set_fail {
    ($name:ident, $($s:expr),+) => {
        #[test]
        #[should_panic]
        fn $name() {
            let mut bfst = Builder::memory();
            $(bfst.add($s).unwrap();)*
        }
    }
}

test_set!(fst_set_only_empty, "");
test_set!(fst_set_one, "a");
test_set!(fst_set_dupe_empty, "", "");
test_set!(fst_set_dupe1, "a", "a");
test_set!(fst_set_dupe2, "a", "b", "b");
test_set!(fst_set_two1, "a", "b");
test_set!(fst_set_two2, "a", "ab");
test_set!(fst_set_jan, "jam", "jbm", "jcm", "jdm", "jem", "jfm", "jgm");

test_set_fail!(fst_set_order1, "b", "a");
test_set_fail!(fst_set_order2, "a", "b", "c", "a");

#[test]
fn fst_set_100000() {
    let words: Vec<Vec<u8>> = TEXT.lines()
                                  .map(|s| s.as_bytes().to_vec())
                                  .collect();
    let fst = fst_set(words.clone());
    assert_eq!(words, fst_inputs(&fst));
    for word in &words {
        assert!(fst.get(word).is_some(),
                "failed to find word: {}",
                ::std::str::from_utf8(word).unwrap());
    }
}

macro_rules! test_map {
    ($name:ident, $($s:expr, $o:expr),+) => {
        #[test]
        fn $name() {
            let fst = fst_map(vec![$(($s, $o)),*]);
            let mut rdr = fst.stream();
            $({
                let (s, o) = rdr.next().unwrap();
                assert_eq!((s, o.value()), ($s.as_bytes(), $o));
            })*
            assert_eq!(rdr.next(), None);
            $({
                assert_eq!(fst.get($s.as_bytes()), Some(Output::new($o)));
            })*
        }
    }
}

macro_rules! test_map_fail {
    ($name:ident, $($s:expr, $o:expr),+) => {
        #[test]
        #[should_panic]
        fn $name() {
            let mut bfst = Builder::memory();
            $(bfst.insert($s, $o).unwrap();)*
        }
    }
}

test_map!(fst_map_only_empty1, "", 0);
test_map!(fst_map_only_empty2, "", 100);
test_map!(fst_map_only_empty3, "", 9999999999);
test_map!(fst_map_one1, "a", 0);
test_map!(fst_map_one2, "a", 100);
test_map!(fst_map_one3, "a", 999999999);
test_map!(fst_map_two, "a", 1, "b", 2);
test_map!(fst_map_many1, "a", 34786, "ab", 26);
test_map!(
    fst_map_many2,
    "a", 34786, "ab", 26, "abc", 58976, "abcd", 25,
    "z", 58, "zabc", 6798
);
test_map!(fst_map_many3, "a", 1, "ab", 0, "abc", 0);

test_map_fail!(fst_map_dupe_empty, "", 0, "", 0);
test_map_fail!(fst_map_dupe1, "a", 0, "a", 0);
test_map_fail!(fst_map_dupe2, "a", 0, "b", 0, "b", 0);
test_map_fail!(fst_map_order1, "b", 0, "a", 0);
test_map_fail!(fst_map_order2, "a", 0, "b", 0, "c", 0, "a", 0);

#[test]
fn fst_map_100000_increments() {
    let words: Vec<(Vec<u8>, u64)> =
        TEXT.lines()
            .enumerate()
            .map(|(i, s)| (s.as_bytes().to_vec(), i as u64))
            .collect();
    let fst = fst_map(words.clone());
    assert_eq!(words, fst_inputs_outputs(&fst));
    for &(ref word, out) in &words {
        assert_eq!(fst.get(word), Some(Output::new(out)));
    }
}

#[test]
fn fst_map_100000_lengths() {
    let words: Vec<(Vec<u8>, u64)> =
        TEXT.lines()
            .map(|s| (s.as_bytes().to_vec(), s.len() as u64))
            .collect();
    let fst = fst_map(words.clone());
    assert_eq!(words, fst_inputs_outputs(&fst));
    for &(ref word, out) in &words {
        assert_eq!(fst.get(word), Some(Output::new(out)));
    }
}

#[test]
fn invalid_version() {
    match Fst::new(vec![0; 32]) {
        Err(Error::Fst(raw::Error::Version { got, .. })) => assert_eq!(got, 0),
        Err(err) => panic!("expected version error, got {:?}", err),
        Ok(_) => panic!("expected version error, got FST"),
    }
}

#[test]
fn invalid_version_crate_too_old() {
    use byteorder::{ByteOrder, LittleEndian};

    let mut buf = vec![0; 32];
    LittleEndian::write_u64(&mut buf, VERSION + 1);
    match Fst::new(buf) {
        Err(Error::Fst(raw::Error::Version { got, .. })) => {
            assert_eq!(got, VERSION + 1);
        }
        Err(err) => panic!("expected version error, got {:?}", err),
        Ok(_) => panic!("expected version error, got FST"),
    }
}

#[test]
fn invalid_format() {
    match Fst::new(vec![0; 0]) {
        Err(Error::Fst(raw::Error::Format)) => {}
        Err(err) => panic!("expected format error, got {:?}", err),
        Ok(_) => panic!("expected format error, got FST"),
    }
}

#[test]
fn fst_set_zero() {
    let fst = fst_set::<_, String>(vec![]);
    let mut rdr = fst.stream();
    assert_eq!(rdr.next(), None);
}

macro_rules! test_range {
    (
        $name:ident,
        min: $min:expr,
        max: $max:expr,
        imin: $imin:expr,
        imax: $imax:expr,
        $($s:expr),*
    ) => {
        #[test]
        fn $name() {
            let items: Vec<&'static str> = vec![$($s),*];
            let items: Vec<_> =
                items.into_iter().enumerate()
                     .map(|(i, k)| (k, i as u64)).collect();
            let fst: Fst = fst_map(items.clone()).into();
            let mut rdr = Stream::new(&fst.meta, fst.data.deref(), AlwaysMatch, $min, $max);
            for i in $imin..$imax {
                assert_eq!(rdr.next().unwrap(),
                           (items[i].0.as_bytes(), Output::new(items[i].1)));
            }
            assert_eq!(rdr.next(), None);
            let mut rdr = rdr.rev();
            for i in ($imin..$imax).rev() {
                assert_eq!(rdr.next().unwrap(),
                           (items[i].0.as_bytes(), Output::new(items[i].1)));
            }
            assert_eq!(rdr.next(), None);
        }
    }
}

test_range! {
    fst_range_empty_1,
    min: Bound::Unbounded, max: Bound::Unbounded,
    imin: 0, imax: 0,
}

test_range! {
    fst_range_empty_2,
    min: Bound::Unbounded, max: Bound::Unbounded,
    imin: 0, imax: 1,
    ""
}

test_range! {
    fst_range_empty_3,
    min: Bound::Included(vec![]), max: Bound::Unbounded,
    imin: 0, imax: 1,
    ""
}

test_range! {
    fst_range_empty_4,
    min: Bound::Excluded(vec![]), max: Bound::Unbounded,
    imin: 0, imax: 0,
    ""
}

test_range! {
    fst_range_empty_5,
    min: Bound::Included(vec![]), max: Bound::Unbounded,
    imin: 0, imax: 2,
    "", "a"
}

test_range! {
    fst_range_empty_6,
    min: Bound::Excluded(vec![]), max: Bound::Unbounded,
    imin: 1, imax: 2,
    "", "a"
}

test_range! {
    fst_range_empty_7,
    min: Bound::Unbounded, max: Bound::Unbounded,
    imin: 0, imax: 2,
    "", "a"
}

test_range! {
    fst_range_empty_8,
    min: Bound::Unbounded, max: Bound::Included(vec![]),
    imin: 0, imax: 1,
    ""
}

test_range! {
    fst_range_empty_9,
    min: Bound::Unbounded, max: Bound::Excluded(vec![]),
    imin: 0, imax: 0,
    ""
}

test_range! {
    fst_range_empty_10,
    min: Bound::Unbounded, max: Bound::Included(vec![]),
    imin: 0, imax: 1,
    "", "a"
}

test_range! {
    fst_range_empty_11,
    min: Bound::Included(vec![]), max: Bound::Included(vec![]),
    imin: 0, imax: 1,
    ""
}

test_range! {
    fst_range_1,
    min: Bound::Included(vec![b'a']), max: Bound::Included(vec![b'z']),
    imin: 0, imax: 4,
    "a", "b", "y", "z"
}

test_range! {
    fst_range_2,
    min: Bound::Excluded(vec![b'a']), max: Bound::Included(vec![b'y']),
    imin: 1, imax: 3,
    "a", "b", "y", "z"
}

test_range! {
    fst_range_3,
    min: Bound::Excluded(vec![b'a']), max: Bound::Excluded(vec![b'y']),
    imin: 1, imax: 2,
    "a", "b", "y", "z"
}

test_range! {
    fst_range_4,
    min: Bound::Unbounded, max: Bound::Unbounded,
    imin: 0, imax: 4,
    "a", "b", "y", "z"
}

test_range! {
    fst_range_5,
    min: Bound::Included(b"abd".to_vec()), max: Bound::Unbounded,
    imin: 0, imax: 0,
    "a", "ab", "abc", "abcd", "abcde"
}

test_range! {
    fst_range_6,
    min: Bound::Included(b"abd".to_vec()), max: Bound::Unbounded,
    imin: 5, imax: 6,
    "a", "ab", "abc", "abcd", "abcde", "abe"
}

test_range! {
    fst_range_7,
    min: Bound::Excluded(b"abd".to_vec()), max: Bound::Unbounded,
    imin: 5, imax: 6,
    "a", "ab", "abc", "abcd", "abcde", "abe"
}

test_range! {
    fst_range_8,
    min: Bound::Included(b"abd".to_vec()), max: Bound::Unbounded,
    imin: 5, imax: 6,
    "a", "ab", "abc", "abcd", "abcde", "xyz"
}

test_range! {
    fst_range_9,
    min: Bound::Unbounded, max: Bound::Included(b"abd".to_vec()),
    imin: 0, imax: 5,
    "a", "ab", "abc", "abcd", "abcde", "abe"
}

test_range! {
    fst_range_10,
    min: Bound::Unbounded, max: Bound::Included(b"abd".to_vec()),
    imin: 0, imax: 6,
    "a", "ab", "abc", "abcd", "abcde", "abd"
}

test_range! {
    fst_range_11,
    min: Bound::Unbounded, max: Bound::Included(b"abd".to_vec()),
    imin: 0, imax: 6,
    "a", "ab", "abc", "abcd", "abcde", "abd", "abdx"
}

test_range! {
    fst_range_12,
    min: Bound::Unbounded, max: Bound::Excluded(b"abd".to_vec()),
    imin: 0, imax: 5,
    "a", "ab", "abc", "abcd", "abcde", "abe"
}

test_range! {
    fst_range_13,
    min: Bound::Unbounded, max: Bound::Excluded(b"abd".to_vec()),
    imin: 0, imax: 5,
    "a", "ab", "abc", "abcd", "abcde", "abd"
}

test_range! {
    fst_range_14,
    min: Bound::Unbounded, max: Bound::Excluded(b"abd".to_vec()),
    imin: 0, imax: 5,
    "a", "ab", "abc", "abcd", "abcde", "abd", "abdx"
}

test_range! {
    fst_range_15,
    min: Bound::Included(vec![b'd']), max: Bound::Included(vec![b'c']),
    imin: 0, imax: 0,
    "a", "b", "c", "d", "e", "f"
}

test_range! {
    fst_range_16,
    min: Bound::Included(vec![b'c']), max: Bound::Included(vec![b'c']),
    imin: 2, imax: 3,
    "a", "b", "c", "d", "e", "f"
}

test_range! {
    fst_range_17,
    min: Bound::Excluded(vec![b'c']), max: Bound::Excluded(vec![b'c']),
    imin: 0, imax: 0,
    "a", "b", "c", "d", "e", "f"
}

test_range! {
    fst_range_18,
    min: Bound::Included(vec![b'c']), max: Bound::Excluded(vec![b'c']),
    imin: 0, imax: 0,
    "a", "b", "c", "d", "e", "f"
}

test_range! {
    fst_range_19,
    min: Bound::Included(vec![b'c']), max: Bound::Excluded(vec![b'd']),
    imin: 2, imax: 3,
    "a", "b", "c", "d", "e", "f"
}

test_range! {
    fst_range_20,
    min: Bound::Included(vec![b'a', b'a', b'a']), max: Bound::Unbounded,
    imin: 1, imax: 4,
    "a", "aaa", "aba", "aca"
}

test_range! {
    fst_range_21,
    min: Bound::Included(b"aab".to_vec()), max: Bound::Unbounded,
    imin: 2, imax: 4,
    "a", "aaa", "aba", "aca"
}

test_range! {
    fst_range_22,
    min: Bound::Excluded(b"aab".to_vec()), max: Bound::Unbounded,
    imin: 2, imax: 4,
    "a", "aaa", "aba", "aca"
}

test_range! {
    fst_range_23,
    min: Bound::Included(b"a".to_vec()), max: Bound::Included(b"a".to_vec()),
    imin: 0, imax: 1,
    "a", "aaa", "aba", "aca"
}

test_range! {
    fst_range_24,
    min: Bound::Included(b"aca".to_vec()), max: Bound::Included(b"aca".to_vec()),
    imin: 3, imax: 4,
    "a", "aaa", "aba", "aca"
}

test_range! {
    fst_range_25,
    min: Bound::Included(b"aba".to_vec()), max: Bound::Included(b"aba".to_vec()),
    imin: 2, imax: 3,
    "a", "aaa", "aba", "aca"
}

test_range! {
    fst_range_26,
    min: Bound::Included(b"aaa".to_vec()), max: Bound::Unbounded,
    imin: 2, imax: 3,
    "a", "aa", "aaa"
}

test_range! {
    fst_range_27,
    min: Bound::Included(b"aa".to_vec()), max: Bound::Unbounded,
    imin: 1, imax: 3,
    "a", "aa", "aaa"
}

test_range! {
    fst_range_28,
    min: Bound::Included(b"a".to_vec()), max: Bound::Unbounded,
    imin: 0, imax: 3,
    "a", "aa", "aaa"
}


#[test]
fn reverse() {
    let items: Vec<_> =
        vec!["1"].into_iter().enumerate()
                     .map(|(i, k)| (k, i as u64)).collect();
    let fst: Fst = fst_map(items.clone()).into();
    let stream = fst.stream();
    assert!(!stream.0.reversed);
    let stream = stream.rev();
    assert!(stream.0.reversed);
    let stream = stream.rev();
    assert!(!stream.0.reversed);
}


#[test]
fn test_range_ge() {
    use crate::{Streamer, IntoStreamer};
    let items: Vec<_> =
        vec!["a", "aaa", "aba", "aca"].into_iter().enumerate()
            .map(|(i, k)| (k, i as u64)).collect();
    let fst: Fst = fst_map(items.clone()).into();
    let stream = fst.range().ge("aaa").into_stream();
    let keys = stream.into_str_keys().unwrap();
    assert_eq!(&keys[..],
    &["aaa", "aba", "aca"]);
}

#[test]
fn test_range_gt() {
    use crate::{Streamer, IntoStreamer};
    let items: Vec<_> =
        vec!["aaa", "aba", "aca"].into_iter().enumerate()
            .map(|(i, k)| (k, i as u64)).collect();
    let fst: Fst = fst_map(items.clone()).into();
    let stream = fst.range().gt("aaa").into_stream();
    let keys = stream.into_str_keys().unwrap();
    assert_eq!(&keys[..],
               &["aba", "aca"]);
}

#[test]
fn starting_transition() {
    let items: Vec<_> =
        vec!["a", "b", "c", "d"].into_iter().enumerate()
                     .map(|(i, k)| (k, i as u64)).collect();
    let fst: Fst = fst_map(items.clone()).into();
    let stream = fst.stream();
    let root = fst.root();
    assert_eq!(stream.0.starting_transition(&root).unwrap(), 0);
    let stream = stream.rev();
    assert_eq!(stream.0.starting_transition(&root).unwrap(), 3);
    let a = fst.node(root.transition(0).addr);
    assert_eq!(stream.0.starting_transition(&a), None);
}

#[test]
fn last_transition() {
    let items: Vec<_> =
        vec!["a", "b", "c", "d"].into_iter().enumerate()
                     .map(|(i, k)| (k, i as u64)).collect();
    let fst: Fst = fst_map(items.clone()).into();
    let stream = fst.stream();
    let root = fst.root();
    assert_eq!(stream.0.last_transition(&root).unwrap(), 3);
    let stream = stream.rev();
    assert_eq!(stream.0.last_transition(&root).unwrap(), 0);
    let a = fst.node(root.transition(0).addr);
    assert_eq!(stream.0.last_transition(&a), None);
}



#[test]
fn next_transition() {
    let items: Vec<_> =
        vec!["a", "ab", "ac", "ad"].into_iter().enumerate()
                     .map(|(i, k)| (k, i as u64)).collect();
    let fst: Fst = fst_map(items.clone()).into();
    let stream = fst.stream();
    let a = fst.node(fst.root().transition(0).addr);
    assert_eq!(a.len(), 3);
    assert_eq!(stream.0.next_transition(&a, 0).unwrap(), 1);
    assert_eq!(stream.0.next_transition(&a, 1).unwrap(), 2);
    assert_eq!(stream.0.next_transition(&a, 2), None);
    assert_eq!(stream.0.previous_transition(&a, 0), None);
    assert_eq!(stream.0.previous_transition(&a, 1).unwrap(), 0);
    assert_eq!(stream.0.previous_transition(&a, 2).unwrap(), 1);
    let stream = stream.rev();
    assert_eq!(stream.0.next_transition(&a, 0), None);
    assert_eq!(stream.0.next_transition(&a, 1).unwrap(), 0);
    assert_eq!(stream.0.next_transition(&a, 2).unwrap(), 1);
    assert_eq!(stream.0.previous_transition(&a, 0).unwrap(), 1);
    assert_eq!(stream.0.previous_transition(&a, 1).unwrap(), 2);
    assert_eq!(stream.0.previous_transition(&a, 2), None);
}

fn test_reverse(input: Vec<&str>) {
    let len = input.len();
    test_reverse_range(input, Bound::Unbounded, Bound::Unbounded, 0, len);
}

fn test_reverse_range(input: Vec<&str>, min: Bound, max: Bound, imin: usize, imax: usize) {
    let items: Vec<_> =
    input.into_iter().enumerate()
         .map(|(i, k)| (k, i as u64)).collect();
    let fst: Fst = fst_map(items.clone()).into();
    let mut stream = Stream::new(&fst.meta, fst.data.deref(), AlwaysMatch, min, max);
    stream = stream.rev();
    for i in (imin..imax).rev() {
        let next = stream.next();
        assert!(next.is_some());
        assert_eq!(next.unwrap(), (items[i].0.as_bytes(), Output::new(items[i].1)));
    }
    assert_eq!(stream.next(), None);
}

#[test] 
fn reverse_traversal() {
    test_reverse(vec!["a"]);
    test_reverse(vec!["a", "b"]);
    test_reverse(vec!["a", "b", "c"]);
    test_reverse(vec!["aa", "ab", "ac"]);
    test_reverse(vec!["a", "ab"]);
    test_reverse(vec!["a", "ab", "abc", "abcd", "abcde", "abd", "abdx"]);
    test_reverse(vec!["a", "ab", "abc", "abcd", "abcde", "abe"]);
}

#[test] 
fn reverse_traversal_bounds() {
    test_reverse_range(vec!["a", "ab", "abc", "abcd", "abcde", "xyz"], Bound::Included(b"abd".to_vec()), Bound::Unbounded, 5, 6);
    test_reverse_range(vec!["a", "b", "y", "z"], Bound::Included(vec![b'a']), Bound::Included(vec![b'z']), 0, 4);
    test_reverse_range(vec!["a", "ab", "abc", "abcd", "abcde", "abd"], Bound::Unbounded, Bound::Included(b"abd".to_vec()), 0, 6);
    test_reverse_range(vec!["a", "ab", "abc", "abcd", "abcde", "abd", "abdx"], Bound::Unbounded, Bound::Included(b"abd".to_vec()), 0, 6);
    test_reverse_range(vec!["a", "ab", "abc", "abcd", "abcde", "abe"],  Bound::Unbounded, Bound::Excluded(b"abd".to_vec()), 0, 5);
    test_reverse_range(vec!["", "a"], Bound::Included(vec![]), Bound::Unbounded, 0, 2);
    test_reverse_range(vec!["a", "aaa", "aba", "aca"], Bound::Included(b"aaa".to_vec()), Bound::Unbounded, 1, 4);
}

#[test]
fn bytes_written() {
    let mut bfst1 = Builder::memory();
    bfst1.add(b"bar").unwrap();
    bfst1.add(b"baz").unwrap();
    let counted_len = bfst1.bytes_written();
    let bytes = bfst1.into_inner().unwrap();
    let fst1_len = bytes.len() as u64;
    let footer_size = 24;
    assert_eq!(counted_len + footer_size, fst1_len);
}
