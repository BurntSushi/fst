#[cfg(feature = "levenshtein")]
use fst::automaton::Levenshtein;
use fst::automaton::{Str, Subsequence, DamerauLevenshtein};
#[cfg(feature = "levenshtein")]
use fst::raw::{Builder, Fst};
use fst::set::Set;
use fst::{self, Automaton, IntoStreamer, Streamer};
use std::collections::BTreeSet;
use std::iter::FromIterator;

#[cfg(feature = "levenshtein")]
fn fst_set1<I, S>(ss: I) -> Fst<Vec<u8>>
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
fn levenshtein_unicode_chinese() {

    let keys = vec!["_标题__北", "_标题__北七", "_标题__北京", "_标题__北京京", "_标题__北京人", "_标题__北京北", "_标题__北平" ];
    let set = Set::from_iter(&keys).unwrap();

    let dist = 1;
    let prefixlen_non_similarity = "_标题__".chars().count();
    let prefixlen_similarity = 0;
    let max_expansions = 50;
    let querystr = "_标题__北平";

    let lev = Levenshtein::new(&querystr, dist as u32,prefixlen_non_similarity as u32,
                               prefixlen_similarity as u32);
    match lev {
        Ok(lev) => {
            let lev_results = set.search(lev).into_stream()
                .into_levenshtein(max_expansions as usize);

            match lev_results {
                Ok(lev_results) => {
                    //打印模糊匹配的term结果
                    println!("levenshtein_unicode_chinese: fuzzy terms of term:[{:?}] on distance:[ {:?} ],\n\t\t\tprefixlen_non_similarity:[ {:?}],\n\t\t\tprefixlen_similarity:[ {:?}],\n\t\t\tmax_expansions:[ {:?}],\n\t\t\ttranspostions:[ {:?}]\n\t\t\t are:",
                             querystr,
                             dist as u32,
                             prefixlen_non_similarity as u32,
                             prefixlen_similarity as u32,
                             max_expansions as u32,
                             "false"
                    );

                    println!("{:?}", lev_results);
                    println!("------------------------------------------------\n\n\n");
                    assert_eq!(lev_results.len(), 4 as usize);
                    assert_eq!(lev_results.get_search_results()[0].get_input(), "_标题__北平".to_string());
                    assert_eq!(lev_results.get_search_results()[1].get_input(), "_标题__北七".to_string());
                    assert_eq!(lev_results.get_search_results()[2].get_input(), "_标题__北京".to_string());
                    assert_eq!(lev_results.get_search_results()[3].get_input(), "_标题__北".to_string());

                    assert_eq!(lev_results.get_search_results()[0].get_edit_distance(), 0 as usize);
                    assert_eq!(lev_results.get_search_results()[1].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[2].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[3].get_edit_distance(), 1 as usize);

                    return;
                },
                Err(lev_error) => {
                    assert!(false, "meet levenshtein results error:{:?}", lev_error);
                    return;
                }
            }
        },
        Err(lev_error) => {
            assert!(false, "meet levenshtein new error:{:?}", lev_error);
        },
    }
}

#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_unicode_chinese_transpositions() {

    let keys = vec!["_标题__北", "_标题__北七", "_标题__北京", "_标题__北京京", "_标题__北京人", "_标题__北京北", "_标题__平北" ];
    let set = Set::from_iter(&keys).unwrap();

    let dist = 1;
    let prefixlen_non_similarity = "_标题__".chars().count();
    let prefixlen_similarity = 0;
    let max_expansions = 50;
    let querystr = "_标题__北平";

    let lev = DamerauLevenshtein::new(&querystr, dist as u32,prefixlen_non_similarity as u32,
                               prefixlen_similarity as u32);
    match lev {
        Ok(lev) => {
            let lev_results = set.search(lev).into_stream()
                .into_levenshtein(max_expansions as usize);
            match lev_results {
                Ok(lev_results) =>{
                    //打印模糊匹配的term结果
                    println!("levenshtein_unicode_chinese_transpositions: fuzzy terms of term:[{:?}] on distance:[ {:?} ],\n\t\t\tprefixlen_non_similarity:[ {:?}],\n\t\t\tprefixlen_similarity:[ {:?}],\n\t\t\tmax_expansions:[ {:?}],\n\t\t\ttranspostions:[ {:?}]\n\t\t\t are:",
                             querystr,
                             dist as u32,
                             prefixlen_non_similarity as u32,
                             prefixlen_similarity as u32,
                             max_expansions as u32,
                             "true",
                    );
                    println!("{:?}", &lev_results);
                    println!("------------------------------------------------\n\n\n");
                    assert_eq!(lev_results.len(), 4 as usize);
                    assert_eq!(lev_results.get_search_results()[0].get_input(), "_标题__北七".to_string());
                    assert_eq!(lev_results.get_search_results()[1].get_input(), "_标题__北京".to_string());
                    assert_eq!(lev_results.get_search_results()[2].get_input(), "_标题__平北".to_string());
                    assert_eq!(lev_results.get_search_results()[3].get_input(), "_标题__北".to_string());

                    assert_eq!(lev_results.get_search_results()[0].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[1].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[2].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[3].get_edit_distance(), 1 as usize);
                    return;
                }
                ,
                Err(lev_error) => {
                    assert!(false, "meet levenshtein results error:{:?}", lev_error);
                    return;
                }
            }
        },
        Err(lev_error) => {
            assert!(false, "meet levenshtein new error:{:?}", lev_error);
        },
    }
}
#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_unicode1() {
    let set = fst_set1(vec!["woof", "wood", "banana", "☃snowman"]);
    // let q = Levenshtein::new_by_dot("snoman", 2, 0,0, String::from("snowman.dot")).unwrap();
    let q = Levenshtein::new("snoman", 2, 0,0).unwrap();
    let vs = set.search(&q).into_stream().into_byte_keys();
    assert_eq!(vs, vec!["☃snowman".as_bytes()]);
}

#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_trans1() {
    let keys = vec!["__username__boko02"];
    let set = Set::from_iter(&keys).unwrap();

    let dist = 1;
    let prefixlen_non_similarity = "__username__".chars().count();
    let prefixlen_similarity = 0;
    let max_expansions = 50;
    let querystr = "__username__book02";

    let lev = DamerauLevenshtein::new(&querystr, dist as u32,prefixlen_non_similarity as u32,
                               prefixlen_similarity as u32);
    match lev {
        Ok(lev) => {
            let lev_results = set.search(lev).into_stream()
                .into_levenshtein(max_expansions as usize);

            match lev_results {
                Ok(lev_results) => {
                    //打印模糊匹配的term结果
                    println!("levenshtein_trans1: fuzzy terms of term:[{:?}] on distance:[ {:?} ],\n\t\t\tprefixlen_non_similarity:[ {:?}],\n\t\t\tprefixlen_similarity:[ {:?}],\n\t\t\tmax_expansions:[ {:?}],\n\t\t\ttranspostions:[ {:?}]\n\t\t\t are:",
                             querystr,
                             dist as u32,
                             prefixlen_non_similarity as u32,
                             prefixlen_similarity as u32,
                             max_expansions as u32,
                             "true"
                    );

                    println!("{:?}", lev_results);
                    println!("------------------------------------------------\n\n\n");
                    assert_eq!(lev_results.len(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[0].get_input(), "__username__boko02".to_string());
                    assert_eq!(lev_results.get_search_results()[0].get_edit_distance(), 1 as usize);

                    return;
                },
                Err(lev_error) => {
                    assert!(false, "meet levenshtein results error:{:?}", lev_error);
                    return;
                }
            }
        },
        Err(lev_error) => {
            assert!(false, "meet levenshtein new error:{:?}", lev_error);
        },
    }
}

#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_trans2() {
    let keys = BTreeSet::from_iter(vec!["__username__abac9a9a8df6","__username__abac99a3adf7", "__username__baac99aa8df6"].into_iter());
    let set = Set::from_iter(&keys).unwrap();

    let dist = 2;
    let prefixlen_non_similarity = "__username__".chars().count();
    let prefixlen_similarity = 0;
    let max_expansions = 50;
    let querystr = "__username__abac99a8adf6";

    let lev = DamerauLevenshtein::new(&querystr, dist as u32,prefixlen_non_similarity as u32,
                                      prefixlen_similarity as u32);
    match lev {
        Ok(lev) => {
            let lev_results = set.search(lev).into_stream()
                .into_levenshtein(max_expansions as usize);

            match lev_results {
                Ok(lev_results) => {
                    //打印模糊匹配的term结果
                    println!("levenshtein_trans2: fuzzy terms of term:[{:?}] on distance:[ {:?} ],\n\t\t\tprefixlen_non_similarity:[ {:?}],\n\t\t\tprefixlen_similarity:[ {:?}],\n\t\t\tmax_expansions:[ {:?}],\n\t\t\ttranspostions:[ {:?}]\n\t\t\t are:",
                             querystr,
                             dist as u32,
                             prefixlen_non_similarity as u32,
                             prefixlen_similarity as u32,
                             max_expansions as u32,
                             "true"
                    );

                    println!("{:?}", lev_results);
                    println!("------------------------------------------------\n\n\n");
                    assert_eq!(lev_results.len(), 3 as usize);
                    assert_eq!(lev_results.get_search_results()[0].get_input(), "__username__abac99a3adf7".to_string());
                    assert_eq!(lev_results.get_search_results()[1].get_input(), "__username__abac9a9a8df6".to_string());
                    assert_eq!(lev_results.get_search_results()[2].get_input(), "__username__baac99aa8df6".to_string());

                    assert_eq!(lev_results.get_search_results()[0].get_edit_distance(), 2 as usize);
                    assert_eq!(lev_results.get_search_results()[1].get_edit_distance(), 2 as usize);
                    assert_eq!(lev_results.get_search_results()[2].get_edit_distance(), 2 as usize);

                    return;
                },
                Err(lev_error) => {
                    assert!(false, "meet levenshtein results error:{:?}", lev_error);
                    return;
                }
            }
        },
        Err(lev_error) => {
            assert!(false, "meet levenshtein new error:{:?}", lev_error);
        },
    }
}

#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_trans3() {
    let keys = BTreeSet::from_iter(vec!["__username_题_ab_标题__北ac99北平a8adf6",
                                        "__username_题_ab_标题__北ac99平北aa8df6",
                                        "__username_题_ab_标题__北ca9北9平a8adf6",
                                        "__username_题_ab_标题__北ac99平北a8a人df6",
                                        "__username_题_ab_题标__北ac99北平a8adf6",
                                        "__username_题_ab_标题__北ac99北a平8adf",
                                        "__username_a题_ab_标题__北ac99北a平8adf",
                                        "__username_题_ab_标题__北ac99北a平8adaf",
    ].into_iter());
    let set = Set::from_iter(&keys).unwrap();

    let dist = 2;
    let prefixlen_non_similarity = "__username_题".chars().count();
    let prefixlen_similarity = 0;
    let max_expansions = 50;
    let querystr = "__username_题_ab_标题__北ac99北平a8adf6";

    let lev = DamerauLevenshtein::new(&querystr, dist as u32,prefixlen_non_similarity as u32,
                                      prefixlen_similarity as u32);
    match lev {
        Ok(lev) => {
            let lev_results = set.search(lev).into_stream()
                .into_levenshtein(max_expansions as usize);

            match lev_results {
                Ok(lev_results) => {
                    //打印模糊匹配的term结果
                    println!("levenshtein_trans3: fuzzy terms of term:[{:?}] on distance:[ {:?} ],\n\t\t\tprefixlen_non_similarity:[ {:?}],\n\t\t\tprefixlen_similarity:[ {:?}],\n\t\t\tmax_expansions:[ {:?}],\n\t\t\ttranspostions:[ {:?}]\n\t\t\t are:",
                             querystr,
                             dist as u32,
                             prefixlen_non_similarity as u32,
                             prefixlen_similarity as u32,
                             max_expansions as u32,
                             "true"
                    );

                    println!("{:?}", lev_results);
                    println!("------------------------------------------------\n\n\n");
                    assert_eq!(lev_results.len(), 6 as usize);
                    assert_eq!(lev_results.get_search_results()[0].get_input(), "__username_题_ab_标题__北ac99北平a8adf6".to_string());
                    assert_eq!(lev_results.get_search_results()[1].get_input(), "__username_题_ab_题标__北ac99北平a8adf6".to_string());
                    assert_eq!(lev_results.get_search_results()[2].get_input(), "__username_题_ab_标题__北ac99平北a8a人df6".to_string());
                    assert_eq!(lev_results.get_search_results()[3].get_input(), "__username_题_ab_标题__北ac99平北aa8df6".to_string());
                    assert_eq!(lev_results.get_search_results()[4].get_input(), "__username_题_ab_标题__北ca9北9平a8adf6".to_string());
                    assert_eq!(lev_results.get_search_results()[5].get_input(), "__username_题_ab_标题__北ac99北a平8adf".to_string());

                    assert_eq!(lev_results.get_search_results()[0].get_edit_distance(), 0 as usize);
                    assert_eq!(lev_results.get_search_results()[1].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[2].get_edit_distance(), 2 as usize);
                    assert_eq!(lev_results.get_search_results()[3].get_edit_distance(), 2 as usize);
                    assert_eq!(lev_results.get_search_results()[4].get_edit_distance(), 2 as usize);
                    assert_eq!(lev_results.get_search_results()[5].get_edit_distance(), 2 as usize);

                    return;
                },
                Err(lev_error) => {
                    assert!(false, "meet levenshtein results error:{:?}", lev_error);
                    return;
                }
            }
        },
        Err(lev_error) => {
            assert!(false, "meet levenshtein new error:{:?}", lev_error);
        },
    }
}
#[cfg(feature = "levenshtein")]
#[test]
fn levenshtein_show_picture() {
    let keys = BTreeSet::from_iter(vec!["_title_大北平", "_title_大北平d", "_title_大平北", "_title_平大北","_title_北大平"].into_iter());
    let set = Set::from_iter(&keys).unwrap();

    let dist = 1;
    let prefixlen_non_similarity = "_title_".chars().count();
    let prefixlen_similarity = 0;
    let max_expansions = 50;
    let querystr = "_title_大北平";

    let lev = DamerauLevenshtein::new_by_dot(&querystr, dist as u32,prefixlen_non_similarity as u32,
                                      prefixlen_similarity as u32, String::from("lev.dot"));

    // dot -Tpng < lev.dot > lev.png  to see picture of automaton

    match lev {
        Ok(lev) => {
            let lev_results = set.search(lev).into_stream()
                .into_levenshtein(max_expansions as usize);

            match lev_results {
                Ok(lev_results) => {
                    //打印模糊匹配的term结果
                    println!("levenshtein_trans3: fuzzy terms of term:[{:?}] on distance:[ {:?} ],\n\t\t\tprefixlen_non_similarity:[ {:?}],\n\t\t\tprefixlen_similarity:[ {:?}],\n\t\t\tmax_expansions:[ {:?}],\n\t\t\ttranspostions:[ {:?}]\n\t\t\t are:",
                             querystr,
                             dist as u32,
                             prefixlen_non_similarity as u32,
                             prefixlen_similarity as u32,
                             max_expansions as u32,
                             "true"
                    );

                    println!("{:?}", lev_results);
                    println!("------------------------------------------------\n\n\n");
                    assert_eq!(lev_results.len(), 4 as usize);
                    assert_eq!(lev_results.get_search_results()[0].get_input(), "_title_大北平".to_string());
                    assert_eq!(lev_results.get_search_results()[1].get_input(), "_title_北大平".to_string());
                    assert_eq!(lev_results.get_search_results()[2].get_input(), "_title_大北平d".to_string());
                    assert_eq!(lev_results.get_search_results()[3].get_input(), "_title_大平北".to_string());

                    assert_eq!(lev_results.get_search_results()[0].get_edit_distance(), 0 as usize);
                    assert_eq!(lev_results.get_search_results()[1].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[2].get_edit_distance(), 1 as usize);
                    assert_eq!(lev_results.get_search_results()[3].get_edit_distance(), 1 as usize);

                    return;
                },
                Err(lev_error) => {
                    assert!(false, "meet levenshtein results error:{:?}", lev_error);
                    return;
                }
            }
        },
        Err(lev_error) => {
            assert!(false, "meet levenshtein new error:{:?}", lev_error);
        },
    }
}
