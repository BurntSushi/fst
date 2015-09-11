use {Builder, Fst, Output};

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
    Fst::new(bfst.into_inner().unwrap())
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
    Fst::new(bfst.into_inner().unwrap())
}

fn fst_words<B: AsRef<[u8]>>(fst: &Fst<B>) -> Vec<Vec<u8>> {
    let mut words = vec![];
    let mut rdr = fst.reader();
    while let Some((word, _)) = rdr.next() {
        words.push(word.to_vec());
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
    let text = include_str!("./data/words-100000");
    let words: Vec<Vec<u8>> = text.lines()
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
                assert_eq!((s, o.0.unwrap()), ($s.as_bytes(), $o));
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
fn out_scratch() {
    let mut bfst = Builder::memory();
    // bfst.add(b"abc").unwrap();
    bfst.insert(b"", 10).unwrap();
    bfst.insert(b"abc", 5).unwrap();
    bfst.insert(b"abcd", 6).unwrap();
    bfst.insert(b"azzzzz", 1).unwrap();

    let fst = Fst::new(bfst.into_inner().unwrap());
    let mut rdr = fst.reader();
    assert_eq!(rdr.next().unwrap(), (&b""[..], Output::new(10)));
    assert_eq!(rdr.next().unwrap(), (&b"abc"[..], Output::new(5)));
    assert_eq!(rdr.next().unwrap(), (&b"abcd"[..], Output::new(6)));
    assert_eq!(rdr.next().unwrap(), (&b"azzzzz"[..], Output::new(1)));
    assert_eq!(rdr.next(), None);
}
