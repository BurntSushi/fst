#[cfg(feature = "levenshtein")]
use fst::automaton::Levenshtein;
use fst::automaton::{Str, Subsequence};
#[cfg(feature = "levenshtein")]
use fst::raw::{Builder, Fst};
use fst::set::Set;
use fst::{self, Automaton, IntoStreamer, Streamer};

static WORDS: &'static str = include_str!("../data/words-10000");

fn get_set() -> Set<Vec<u8>> {
    Set::from_iter(WORDS.lines()).unwrap()
}

#[cfg(feature = "levenshtein")]
fn fst_set<I, S>(ss: I) -> Fst<Vec<u8>>
where
    I: IntoIterator<Item = S>,
    S: AsRef<[u8]>,
{
    let mut bfst = Builder::memory();
    let mut ss: Vec<Vec<u8>> =
        ss.into_iter().map(|s| s.as_ref().to_vec()).collect();
    ss.sort();
    for s in ss.iter().into_iter() {
        bfst.add(s).unwrap();
    }
    let fst = bfst.into_fst();
    ss.dedup();
    assert_eq!(fst.len(), ss.len());
    fst
}

#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_simple() {
    let set = fst_set(vec!["woof", "wood", "banana"]);
    let q = Levenshtein::new("woog", 1).unwrap();
    let vs = set.search(&q).into_stream().into_byte_keys();
    assert_eq!(vs, vec!["wood".as_bytes(), "woof".as_bytes()]);
}

#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_unicode() {
    let set = fst_set(vec!["woof", "wood", "banana", "☃snowman☃"]);
    let q = Levenshtein::new("snoman", 3).unwrap();
    let vs = set.search(&q).into_stream().into_byte_keys();
    assert_eq!(vs, vec!["☃snowman☃".as_bytes()]);
}

#[cfg(feature = "levenshtein")]
#[test]
fn complement_small() {
    let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
    let set = Set::from_iter(keys).unwrap();
    let lev = Levenshtein::new("foo", 1).unwrap();
    let stream = set.search(lev.complement()).into_stream();

    let keys = stream.into_strs().unwrap();
    assert_eq!(keys, vec!["fa", "focus", "foul"]);
}

#[cfg(feature = "levenshtein")]
#[test]
fn startswith_small() {
    let keys = vec![
        "", "cooing", "fa", "fo", "fob", "focus", "foo", "food", "foul",
        "fritter", "frothing",
    ];
    let set = Set::from_iter(keys).unwrap();
    let lev = Levenshtein::new("foo", 1).unwrap();
    let stream = set.search(lev.starts_with()).into_stream();

    let keys = stream.into_strs().unwrap();
    assert_eq!(
        keys,
        vec![
            "cooing", "fo", "fob", "focus", "foo", "food", "foul", "frothing",
        ]
    );
}

#[cfg(feature = "levenshtein")]
#[test]
fn intersection_small() {
    let keys = vec!["fab", "fo", "fob", "focus", "foo", "food", "foul", "goo"];
    let set = Set::from_iter(keys).unwrap();
    let lev = Levenshtein::new("foo", 1).unwrap();
    let prefix = Str::new("fo").starts_with();
    let stream = set.search(lev.intersection(prefix)).into_stream();

    let keys = stream.into_strs().unwrap();
    assert_eq!(keys, vec!["fo", "fob", "foo", "food"]);
}

#[cfg(feature = "levenshtein")]
#[test]
fn union_small() {
    let keys = vec!["fab", "fob", "focus", "foo", "food", "goo"];
    let set = Set::from_iter(keys).unwrap();
    let lev = Levenshtein::new("foo", 1).unwrap();
    let prefix = Str::new("fo").starts_with();
    let stream = set.search(lev.union(prefix)).into_stream();

    let keys = stream.into_strs().unwrap();
    assert_eq!(keys, vec!["fob", "focus", "foo", "food", "goo"]);
}

#[cfg(feature = "levenshtein")]
#[test]
fn intersection_large() {
    use fst::set::OpBuilder;

    let set = get_set();
    let lev = Levenshtein::new("foo", 3).unwrap();
    let prefix = Str::new("fa").starts_with();
    let mut stream1 = set.search((&lev).intersection(&prefix)).into_stream();
    let mut stream2 = OpBuilder::new()
        .add(set.search(&lev))
        .add(set.search(&prefix))
        .intersection();
    while let Some(key1) = stream1.next() {
        assert_eq!(stream2.next(), Some(key1));
    }
    assert_eq!(stream2.next(), None);
}

#[cfg(feature = "levenshtein")]
#[test]
fn union_large() {
    use fst::set::OpBuilder;

    let set = get_set();
    let lev = Levenshtein::new("foo", 3).unwrap();
    let prefix = Str::new("fa").starts_with();
    let mut stream1 = set.search((&lev).union(&prefix)).into_stream();
    let mut stream2 = OpBuilder::new()
        .add(set.search(&lev))
        .add(set.search(&prefix))
        .union();
    while let Some(key1) = stream1.next() {
        assert_eq!(stream2.next(), Some(key1));
    }
    assert_eq!(stream2.next(), None);
}

#[test]
fn str() {
    let set = get_set();

    let exact = Str::new("vatican");
    let mut stream = set.search(&exact).into_stream();
    assert_eq!(stream.next().unwrap(), b"vatican");
    assert_eq!(stream.next(), None);

    let exact_mismatch = Str::new("abracadabra");
    let mut stream = set.search(&exact_mismatch).into_stream();
    assert_eq!(stream.next(), None);

    let starts_with = Str::new("vati").starts_with();
    let mut stream = set.search(&starts_with).into_stream();
    assert_eq!(stream.next().unwrap(), b"vatican");
    assert_eq!(stream.next().unwrap(), b"vation");
    assert_eq!(stream.next(), None);
}

#[test]
fn subsequence() {
    let set = get_set();
    let subseq = Subsequence::new("nockbunsurrundd");

    let mut stream = set.search(&subseq).into_stream();
    assert_eq!(stream.next().unwrap(), b"bannockburnsurrounded");
    assert_eq!(stream.next(), None);
}

#[test]
fn implements_default() {
    let map: fst::Map<Vec<u8>> = Default::default();
    assert!(map.is_empty());

    let set: fst::Set<Vec<u8>> = Default::default();
    assert!(set.is_empty());
}

#[test]
fn configurable_registry_preserves_map_and_set_semantics() {
    let entries = [(&b"alpha"[..], 1), (&b"alpine"[..], 2), (&b"beta"[..], 3)];
    let small_registry = fst::RegistryConfig::new(1, 1);

    let mut map =
        fst::MapBuilder::new_with_registry(Vec::new(), small_registry)
            .unwrap();
    for (key, value) in entries {
        map.insert(key, value).unwrap();
    }
    let map = fst::Map::new(map.into_inner().unwrap()).unwrap();
    for (key, value) in entries {
        assert_eq!(map.get(key), Some(value));
    }

    let mut set =
        fst::SetBuilder::new_with_registry(Vec::new(), small_registry)
            .unwrap();
    for (key, _) in entries {
        set.insert(key).unwrap();
    }
    let set = fst::Set::new(set.into_inner().unwrap()).unwrap();
    for (key, _) in entries {
        assert!(set.contains(key));
    }
}

#[test]
fn explicit_default_registry_is_byte_identical() {
    let entries = (0..4_096u64)
        .map(|value| {
            let branch = "x".repeat((value as usize) % 17);
            (format!("{value:04}-{branch}-tail-{:02}", value % 31), value)
        })
        .collect::<Vec<_>>();

    let mut default_map = fst::MapBuilder::new(Vec::new()).unwrap();
    let mut explicit_map = fst::MapBuilder::new_with_registry(
        Vec::new(),
        fst::RegistryConfig::default(),
    )
    .unwrap();
    let mut default_set = fst::SetBuilder::new(Vec::new()).unwrap();
    let mut explicit_set = fst::SetBuilder::new_with_registry(
        Vec::new(),
        fst::RegistryConfig::default(),
    )
    .unwrap();
    for (key, value) in &entries {
        default_map.insert(key, *value).unwrap();
        explicit_map.insert(key, *value).unwrap();
        default_set.insert(key).unwrap();
        explicit_set.insert(key).unwrap();
    }
    assert_eq!(
        default_map.into_inner().unwrap(),
        explicit_map.into_inner().unwrap()
    );
    assert_eq!(
        default_set.into_inner().unwrap(),
        explicit_set.into_inner().unwrap()
    );

    let mut default_raw = fst::raw::Builder::new_type(Vec::new(), 7).unwrap();
    let mut explicit_raw = fst::raw::Builder::new_type_with_registry(
        Vec::new(),
        7,
        fst::RegistryConfig::default(),
    )
    .unwrap();
    for (key, value) in entries {
        default_raw.insert(&key, value).unwrap();
        explicit_raw.insert(&key, value).unwrap();
    }
    assert_eq!(
        default_raw.into_inner().unwrap(),
        explicit_raw.into_inner().unwrap()
    );
}

#[test]
#[should_panic(expected = "registry table size must be nonzero")]
fn configurable_registry_rejects_zero_rows() {
    let _ = fst::RegistryConfig::new(0, 1);
}

#[test]
#[should_panic(expected = "registry MRU size must be nonzero")]
fn configurable_registry_rejects_zero_ways() {
    let _ = fst::RegistryConfig::new(1, 0);
}

#[test]
#[should_panic(expected = "registry dimensions overflow")]
fn configurable_registry_rejects_overflow() {
    let _ = fst::RegistryConfig::new(usize::MAX, 2);
}
