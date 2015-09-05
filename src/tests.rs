use quickcheck::{TestResult, quickcheck};

use {
    NONE_STATE,
    Builder, BuilderNode, BuilderTransition, CompiledAddr, Fst, Output, Node,
};

#[test]
fn prop_emits_inputs() {
    fn p(mut bs: Vec<Vec<u8>>) -> TestResult {
        bs.sort();
        bs.dedup();

        let mut bfst = Builder::memory();
        for word in &bs {
            bfst.add(word).unwrap();
        }
        let fst = Fst::new(bfst.into_inner().unwrap());
        let mut rdr = fst.reader();
        let mut words = vec![];
        while let Some(w) = rdr.next() {
            words.push(w.0.to_owned());
        }
        TestResult::from_bool(bs == words)
    }
    quickcheck(p as fn(Vec<Vec<u8>>) -> TestResult)
}

fn nodes_equal(compiled: &Node, uncompiled: &BuilderNode) -> bool {
    assert_eq!(compiled.is_final(), uncompiled.is_final);
    assert_eq!(compiled.len(), uncompiled.trans.len());
    for (ct, ut)
     in compiled.transitions().zip(uncompiled.trans.iter().cloned()) {
        assert_eq!(ct.0, ut.inp);
        assert_eq!(ct.1, ut.out);
        assert_eq!(ct.2, ut.addr);
    }
    true
}

fn compile(node: &BuilderNode) -> (CompiledAddr, Vec<u8>) {
    let mut buf = vec![0, 0, 0, 0, 0, 0, 0, 0]; // first 8 are reserved
    node.compile_to(&mut buf, NONE_STATE, 8).unwrap();
    (buf.len() as CompiledAddr - 1, buf)
}

fn roundtrip(bnode: &BuilderNode) -> bool {
    let (addr, bytes) = compile(bnode);
    let node = Node::new(addr, &bytes);
    assert_eq!(node.end_addr(), 8);
    nodes_equal(&node, &bnode)
}

fn trans(addr: CompiledAddr, inp: u8) -> BuilderTransition {
    BuilderTransition { addr: addr, inp: inp, out: Output(None) }
}

#[test]
fn bin_no_trans() {
    let node = BuilderNode {
        is_final: false,
        trans: vec![],
    };
    roundtrip(&node);
    assert_eq!(compile(&node).1.len(), 10);
}

#[test]
fn bin_one_label_packed() {
    let node = BuilderNode {
        is_final: false,
        trans: vec![trans(2, b'a')],
    };
    roundtrip(&node);
    assert_eq!(compile(&node).1.len(), 10);
}

#[test]
fn bin_one_label_not_packed() {
    let node = BuilderNode {
        is_final: false,
        trans: vec![trans(2, b'~')],
    };
    roundtrip(&node);
    assert_eq!(compile(&node).1.len(), 11);
}

#[test]
fn bin_many_trans() {
    let node = BuilderNode {
        is_final: false,
        trans: vec![
            trans(2, b'a'), trans(3, b'b'),
            trans(4, b'c'), trans(5, b'd'),
            trans(6, b'e'), trans(7, b'f'),
        ],
    };
    roundtrip(&node);
    assert_eq!(compile(&node).1.len(), 22);
}

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
test_set!(fst_set_two, "a", "b");
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
test_map!(
    fst_map_many,
    "a", 34786, "ab", 26 //, "abc", 58976, "abcd", 25
    // "z", 58, "zabc", 6798
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
