use automaton::Automaton;
///
/// Hamming automaton -- accepts strings with up to `max_mismatches` letters different than
/// `query`. The accepted strings must have the same length as `query`.
///
/// ```rust
/// use fst::{IntoStreamer, Streamer, Hamming, Set};
///
/// let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
/// let set = Set::from_iter(keys).unwrap();
///
/// let ham = Hamming::new(String::from("foo"), 1);
/// let mut stream = set.search(&ham).into_stream();
///
/// let mut keys = vec![];
/// while let Some(key) = stream.next() {
///     keys.push(key.to_vec());
/// }
/// assert_eq!(keys, vec![
///     "fob".as_bytes(),  // 1 substitution
///     "foo".as_bytes(),  // 0 insertions/deletions/substitutions
/// ]);
/// ```
#[derive(Debug)]
pub struct Hamming {
    query: String,
    max_mismatches: u32,
}

#[derive(Debug)]
struct HammingAutState {
    pos: usize,
    mismatches: u32,
}

impl Automaton for Hamming {
    type State = Option<HammingAutState>;

    fn start(&self) -> Option<HammingAutState> {
        Some(HammingAutState {
            pos: 0,
            mismatches: 0,
        })
    }

    fn is_match(&self, state: &Option<HammingAutState>) -> bool {
        state.as_ref().map(|s| s.pos == self.query.len()).unwrap_or(false)
    }

    fn can_match(&self, state: &Option<HammingAutState>) -> bool {
        state.is_some()
    }

    fn accept(&self, state: &Option<HammingAutState>, c: u8) -> Option<HammingAutState> {
        state.as_ref().and_then(|s| {
            if s.pos == self.query.len() {
                return None // Won't accept longer strings
            }

            let mismatches = s.mismatches + if c == self.query.as_bytes()[s.pos] {
                0
            } else {
                1
            };

            if mismatches > self.max_mismatches {
                None
            } else {
                Some(HammingAutState {
                    pos: s.pos + 1,
                    mismatches: mismatches
                })
            }

        })
    }
}

impl Hamming {
    ///
    /// New Hamming automaton matching strings similar to `query`.
    pub fn new(query: &String, max_mismatches: u32) -> Hamming {
        Hamming {
            query: query.clone(),
            max_mismatches: max_mismatches,
        }
    }

    #[cfg(test)]
    fn accepts(&self, s: &str) -> bool {
        let mut state = self.start();
        for c in s.bytes() {
            println!("c: {}, state {:?}", c as char, &state);
            if !self.can_match(&state) { return false }

            state = self.accept(&state, c)
        }

        println!("Final: {:?}", &state);
        self.is_match(&state)
    }
}

#[test]
fn test() {
    assert!(Hamming::new(String::from("AGATA"), 2).accepts("GAATA"));
    assert!(Hamming::new(String::from("AGATA"), 2).accepts("AAATA"));
    assert!(Hamming::new(String::from("AGATA"), 2).accepts("AGATA"));
    assert!(Hamming::new(String::from("AGATA"), 2).accepts("AGATC"));

    assert!(!Hamming::new(String::from("AGATA"), 2).accepts("CCCTA"));
    assert!(!Hamming::new(String::from("AGATA"), 2).accepts("GAAT"));
    assert!(!Hamming::new(String::from("AGATA"), 2).accepts("GAATAA"));
}
