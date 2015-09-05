use quickcheck::{Arbitrary, Gen, TestResult, quickcheck};

use {
    NONE_STATE,
    Builder, BuilderNode, BuilderTransition, CompiledAddr, Fst, Output, Node,
};

#[derive(Clone, Debug)]
struct NonEmptyBytes(Vec<u8>);

impl Arbitrary for NonEmptyBytes {
    fn arbitrary<G: Gen>(g: &mut G) -> NonEmptyBytes {
        // let size = g.size();
        let size = g.gen_range(1, 10);
        let mut bytes = vec![0; size];
        for i in 0..size {
            bytes[i] = g.gen_range(0, 5);
        }
        NonEmptyBytes(bytes)
    }

    fn shrink(&self) -> Box<Iterator<Item=NonEmptyBytes>> {
        Box::new(self.0.shrink()
                       .filter(|bytes| !bytes.is_empty())
                       .map(NonEmptyBytes))
    }
}

impl Arbitrary for BuilderNode {
    fn arbitrary<G: Gen>(g: &mut G) -> BuilderNode {
        let mut trans = Vec::<BuilderTransition>::arbitrary(g);
        trans.truncate(256);
        for (i, ref mut t) in trans.iter_mut().enumerate() {
            t.inp = i as u8;
        }
        BuilderNode {
            is_final: bool::arbitrary(g),
            trans: trans,
        }
    }

    fn shrink(&self) -> Box<Iterator<Item=BuilderNode>> {
        let v = (self.is_final, self.trans.clone());
        Box::new(v.shrink().map(|(is_final, mut trans)| {
            trans.truncate(256);
            // for (i, ref mut t) in trans.iter_mut().enumerate() {
                // t.inp = i as u8;
            // }
            BuilderNode { is_final: is_final, trans: trans }
        }))
    }
}

impl Arbitrary for BuilderTransition {
    fn arbitrary<G: Gen>(g: &mut G) -> BuilderTransition {
        BuilderTransition {
            addr: CompiledAddr::arbitrary(g),
            inp: u8::arbitrary(g),
            out: Output::arbitrary(g),
        }
    }

    fn shrink(&self) -> Box<Iterator<Item=BuilderTransition>> {
        let v = (self.addr.clone(), self.inp, self.out);
        Box::new(v.shrink().map(|(addr, inp, out)| {
             BuilderTransition { addr: addr, inp: inp, out: out }
        }))
    }
}

impl Arbitrary for Output {
    fn arbitrary<G: Gen>(g: &mut G) -> Output {
        Output(Option::<u64>::arbitrary(g))
    }

    fn shrink(&self) -> Box<Iterator<Item=Output>> {
        Box::new(self.0.shrink().map(Output))
    }
}

fn nodes_equal(compiled: &Node, uncompiled: &BuilderNode) -> bool {
    assert_eq!(compiled.is_final(), uncompiled.is_final);
    assert_eq!(compiled.len(), uncompiled.trans.len());
    for (ct, ut)
     in compiled.transitions().zip(uncompiled.trans.iter().cloned()) {
        assert_eq!(ct.0, ut.inp);
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
fn prop_emits_inputs() {
    fn p(bs: Vec<NonEmptyBytes>) -> TestResult {
        let mut bs: Vec<Vec<u8>> = bs.into_iter().map(|x| x.0).collect();
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
    quickcheck(p as fn(Vec<NonEmptyBytes>) -> TestResult)
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

#[test]
fn scratch() {
    let mut bfst = Builder::memory();
    bfst.add("").unwrap();
    bfst.add("abcdefghijklmnopqrstuvwxyzabcdefghiklmnopqrstuvwxyz").unwrap();
    bfst.add("bcdefghijklmnopqrstuvwxyz").unwrap();
    bfst.add("jan").unwrap();
    bfst.add("jbm").unwrap();
    bfst.add("jcm").unwrap();
    bfst.add("jdm").unwrap();
    bfst.add("jem").unwrap();
    bfst.add("jfm").unwrap();
    bfst.add("jgm").unwrap();
    bfst.add("jhm").unwrap();
    bfst.add("jim").unwrap();
    bfst.add("jjm").unwrap();
    bfst.add("jkm").unwrap();
    bfst.add("july").unwrap();
    bfst.add("jun").unwrap();

    // let x1 = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    // let x2 = vec![0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    // let x3 = vec![1, 0];
    // bfst.add(x1).unwrap();
    // bfst.add(x2).unwrap();
    // bfst.add(x3).unwrap();

    // bfst.add(vec![0, 0, 0, 2]).unwrap();
    // bfst.add(vec![0, 1, 0, 1]).unwrap();
    // bfst.add(vec![0, 1, 1, 0, 0, 0, 0, 0, 3]).unwrap();
    // bfst.add(vec![0, 1, 2, 0, 0]).unwrap();
    // bfst.add(vec![0, 2, 0, 0, 0, 0, 1]).unwrap();
    // bfst.add(vec![0, 3, 0, 1, 0]).unwrap();
    // bfst.add(vec![0, 4, 2, 4]).unwrap();
    // bfst.add(vec![1, 4, 2, 4]).unwrap();
    // bfst.add(vec![2, 0, 1]).unwrap();

    let buf = bfst.into_inner().unwrap();
    let fst = Fst::new(buf);

    let mut rdr = fst.reader();
    while let Some(v) = rdr.next() {
        println!("{}", ::std::str::from_utf8(v.0).unwrap());
        // println!("### {:?}", v);
    }
}

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
