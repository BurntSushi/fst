use fst::{Builder, Error, Fst, Output};

const TEXT: &'static str = include_str!("./../../data/words-100000");

fn fst_set<I, S>(ss: I) -> Fst<Vec<u8>>
        where I: IntoIterator<Item=S>, S: AsRef<[u8]> {
    let mut bfst = Builder::memory();
    let mut ss: Vec<Vec<u8>> =
        ss.into_iter().map(|s| s.as_ref().to_vec()).collect();
    ss.sort();
    ss.dedup();
    for s in ss.into_iter() {
        bfst.add(s).unwrap();
    }
    Fst::new(bfst.into_inner().unwrap()).unwrap()
}

fn fst_map<I, S>(ss: I) -> Fst<Vec<u8>>
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

fn fst_words<B: AsRef<[u8]>>(fst: &Fst<B>) -> Vec<Vec<u8>> {
    let mut words = vec![];
    let mut rdr = fst.reader();
    while let Some((word, _)) = rdr.next() {
        words.push(word.to_vec());
    }
    words
}

fn fst_words_outputs<B: AsRef<[u8]>>(fst: &Fst<B>) -> Vec<(Vec<u8>, u64)> {
    let mut words = vec![];
    let mut rdr = fst.reader();
    while let Some((word, out)) = rdr.next() {
        words.push((word.to_vec(), out.into_option().unwrap()));
    }
    words
}

macro_rules! test_set {
    ($name:ident, $($s:expr),+) => {
        #[test]
        fn $name() {
            let fst = fst_set(vec![$($s),*]);
            let mut rdr = fst.reader();
            $(assert_eq!(rdr.next().unwrap().0, $s.as_bytes());)*
            assert_eq!(rdr.next(), None);
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
test_set!(fst_set_two1, "a", "b");
test_set!(fst_set_two2, "a", "ab");
test_set!(fst_set_jan, "jam", "jbm", "jcm", "jdm", "jem", "jfm", "jgm");

test_set_fail!(fst_set_dupe_empty, "", "");
test_set_fail!(fst_set_dupe, "a", "a");
test_set_fail!(fst_set_order, "b", "a");
test_set_fail!(fst_set_order_2, "a", "b", "c", "a");

#[test]
fn fst_set_100000() {
    let words: Vec<Vec<u8>> = TEXT.lines()
                                  .map(|s| s.as_bytes().to_vec())
                                  .collect();
    let fst = fst_set(words.clone());
    assert_eq!(words, fst_words(&fst));
}

macro_rules! test_map {
    ($name:ident, $($s:expr, $o:expr),+) => {
        #[test]
        fn $name() {
            let fst = fst_map(vec![$(($s, $o)),*]);
            let mut rdr = fst.reader();
            $({
                let (s, o) = rdr.next().unwrap();
                assert_eq!((s, o.into_option().unwrap()), ($s.as_bytes(), $o));
            })*
            assert_eq!(rdr.next(), None);
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

#[test]
fn fst_map_100000_increments() {
    let words: Vec<(Vec<u8>, u64)> =
        TEXT.lines()
            .enumerate()
            .map(|(i, s)| (s.as_bytes().to_vec(), i as u64))
            .collect();
    let fst = fst_map(words.clone());
    assert_eq!(words, fst_words_outputs(&fst));
}

#[test]
fn fst_map_100000_lengths() {
    let words: Vec<(Vec<u8>, u64)> =
        TEXT.lines()
            .map(|s| (s.as_bytes().to_vec(), s.len() as u64))
            .collect();
    let fst = fst_map(words.clone());
    assert_eq!(words, fst_words_outputs(&fst));
}

#[test]
fn invalid_version() {
    match Fst::new(vec![0; 16]) {
        Err(Error::Version { expected, got }) => assert_eq!(got, 0),
        Err(err) => panic!("expected version error, got {:?}", err),
        Ok(_) => panic!("expected version error, got FST"),
    }
}

#[test]
fn invalid_format() {
    match Fst::new(vec![0; 0]) {
        Err(Error::Format) => {}
        Err(err) => panic!("expected format error, got {:?}", err),
        Ok(_) => panic!("expected format error, got FST"),
    }
}

#[test]
fn invalid_value() {
    let mut bfst = Builder::memory();
    match bfst.insert("abc", ::std::u64::MAX) {
        Err(Error::Value { got }) => assert_eq!(got, ::std::u64::MAX),
        other => panic!("expected value error, got {:?}", other),
    }
}

#[test]
fn scratch() {
    let mut bfst = Builder::memory();
    bfst.insert(b"", 10).unwrap();
    bfst.insert(b"abc", 5).unwrap();
    bfst.insert(b"abcd", 6).unwrap();
    bfst.insert(b"azzzzz", 1).unwrap();

    let fst = Fst::new(bfst.into_inner().unwrap()).unwrap();
    let mut rdr = fst.reader();
    assert_eq!(rdr.next().unwrap(), (&b""[..], Output::some(10).unwrap()));
    assert_eq!(rdr.next().unwrap(), (&b"abc"[..], Output::some(5).unwrap()));
    assert_eq!(rdr.next().unwrap(), (&b"abcd"[..], Output::some(6).unwrap()));
    assert_eq!(rdr.next().unwrap(),
               (&b"azzzzz"[..], Output::some(1).unwrap()));
    assert_eq!(rdr.next(), None);
}
