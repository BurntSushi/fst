use error::Error;
use fst::{Builder, Fst, Output};

const TEXT: &'static str = include_str!("./../../data/words-100000");

pub fn fst_set<I, S>(ss: I) -> Fst
        where I: IntoIterator<Item=S>, S: AsRef<[u8]> {
    let mut bfst = Builder::memory();
    let mut ss: Vec<Vec<u8>> =
        ss.into_iter().map(|s| s.as_ref().to_vec()).collect();
    ss.sort();
    for s in ss.into_iter() {
        bfst.add(s).unwrap();
    }
    Fst::from_bytes(bfst.into_inner().unwrap()).unwrap()
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
    Fst::from_bytes(bfst.into_inner().unwrap()).unwrap()
}

pub fn fst_inputs(fst: &Fst) -> Vec<Vec<u8>> {
    let mut words = vec![];
    let mut rdr = fst.reader();
    while let Some((word, _)) = rdr.next() {
        words.push(word.to_vec());
    }
    words
}

pub fn fst_input_strs(fst: &Fst) -> Vec<String> {
    let mut words = vec![];
    let mut rdr = fst.reader();
    while let Some((word, _)) = rdr.next() {
        words.push(::std::str::from_utf8(word).unwrap().to_owned());
    }
    words
}

pub fn fst_inputs_outputs(fst: &Fst) -> Vec<(Vec<u8>, u64)> {
    let mut words = vec![];
    let mut rdr = fst.reader();
    while let Some((word, out)) = rdr.next() {
        words.push((word.to_vec(), out.value()));
    }
    words
}

pub fn fst_inputstrs_outputs(fst: &Fst) -> Vec<(String, u64)> {
    let mut words = vec![];
    let mut rdr = fst.reader();
    while let Some((word, out)) = rdr.next() {
        let word = ::std::str::from_utf8(word).unwrap().to_owned();
        words.push((word, out.value()));
    }
    words
}

macro_rules! test_set {
    ($name:ident, $($s:expr),+) => {
        #[test]
        fn $name() {
            let mut items = vec![$($s),*];
            let fst = fst_set(&items);
            let mut rdr = fst.reader();
            items.sort();
            items.dedup();
            for item in &items {
                assert_eq!(rdr.next().unwrap().0, item.as_bytes());
            }
            assert_eq!(rdr.next(), None);
            for item in &items {
                assert!(fst.find(item).is_some());
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
        assert!(fst.find(word).is_some(),
                "failed to find word: {}",
                ::std::str::from_utf8(word).unwrap());
    }
}

macro_rules! test_map {
    ($name:ident, $($s:expr, $o:expr),+) => {
        #[test]
        fn $name() {
            let fst = fst_map(vec![$(($s, $o)),*]);
            let mut rdr = fst.reader();
            $({
                let (s, o) = rdr.next().unwrap();
                assert_eq!((s, o.value()), ($s.as_bytes(), $o));
            })*
            assert_eq!(rdr.next(), None);
            $({
                assert_eq!(fst.find($s.as_bytes()), Some(Output::new($o)));
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
        assert_eq!(fst.find(word), Some(Output::new(out)));
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
        assert_eq!(fst.find(word), Some(Output::new(out)));
    }
}

#[test]
fn invalid_version() {
    match Fst::from_bytes(vec![0; 16]) {
        Err(Error::Version { expected, got }) => assert_eq!(got, 0),
        Err(err) => panic!("expected version error, got {:?}", err),
        Ok(_) => panic!("expected version error, got FST"),
    }
}

#[test]
fn invalid_format() {
    match Fst::from_bytes(vec![0; 0]) {
        Err(Error::Format) => {}
        Err(err) => panic!("expected format error, got {:?}", err),
        Ok(_) => panic!("expected format error, got FST"),
    }
}

#[test]
fn fst_set_zero() {
    let fst = fst_set::<_, String>(vec![]);
    let mut rdr = fst.reader();
    assert_eq!(rdr.next(), None);
}

#[test]
fn scratch() {
    let mut bfst = Builder::memory();
    bfst.insert(b"", 10).unwrap();
    bfst.insert(b"abc", 5).unwrap();
    bfst.insert(b"abcd", 6).unwrap();
    bfst.insert(b"azzzzz", 1).unwrap();

    let fst = Fst::from_bytes(bfst.into_inner().unwrap()).unwrap();
    let mut rdr = fst.reader();
    assert_eq!(rdr.next().unwrap(), (&b""[..], Output::new(10)));
    assert_eq!(rdr.next().unwrap(), (&b"abc"[..], Output::new(5)));
    assert_eq!(rdr.next().unwrap(), (&b"abcd"[..], Output::new(6)));
    assert_eq!(rdr.next().unwrap(),
               (&b"azzzzz"[..], Output::new(1)));
    assert_eq!(rdr.next(), None);
}
