use self::StartsWithStateInternal::*;
use std::usize;

/// Automaton describes types that behave as a finite automaton.
///
/// All implementators of this trait are represented by *byte based* automata.
/// Stated differently, all transitions in the automata correspond to a single
/// byte in the input.
///
/// This implementation choice is important for a couple reasons:
///
/// 1. The set of possible transitions in each node is small, which may make
///    efficient memory usage easier.
/// 2. The finite state transducers in this crate are all byte based, so any
///    automata used on them must also be byte based.
///
/// In practice, this does present somewhat of a problem, for example, if
/// you're storing UTF-8 encoded strings in a finite state transducer. Consider
/// using a `Levenshtein` automaton, which accepts a query string and an edit
/// distance. The edit distance should apply to some notion of *character*,
/// which could be represented by at least 1-4 bytes in a UTF-8 encoding (for
/// some definition of "character"). Therefore, the automaton must have UTF-8
/// decoding built into it. This can be tricky to implement, so you may find
/// the [`utf8-ranges`](https://crates.io/crates/utf8-ranges) crate useful.
pub trait Automaton {
    /// The type of the state used in the automaton.
    type State;

    /// Returns a single start state for this automaton.
    ///
    /// This method should always return the same value for each
    /// implementation.
    fn start(&self) -> Self::State;

    /// Returns true if and only if `state` is a match state.
    fn is_match(&self, state: &Self::State) -> bool;

    /// Returns true if and only if `state` can lead to a match in zero or more
    /// steps.
    ///
    /// If this returns `false`, then no sequence of inputs from this state
    /// should ever produce a match. If this does not follow, then those match
    /// states may never be reached. In other words, behavior may be incorrect.
    ///
    /// If this returns `true` even when no match is possible, then behavior
    /// will be correct, but callers may be forced to do additional work.
    fn can_match(&self, _state: &Self::State) -> bool {
        true
    }

    /// Returns true if and only if `state` matches and must match no matter what
    /// steps are taken.
    ///
    /// If this returns `true`, then every sequence of inputs from this state
    /// produces a match. If this does not follow, then those match states may
    /// never be reached. In other words, behavior may be incorrect.
    ///
    /// If this returns `false` even when every sequence of inputs will lead to
    /// a match, then behavior will be correct, but callers may be forced to do
    /// additional work.
    fn will_always_match(&self, _state: &Self::State) -> bool {
        false
    }

    /// Return the next state given `state` and an input.
    fn accept(&self, state: &Self::State, byte: u8) -> Self::State;

    /// Returns an automaton that matches the strings that start with something
    /// this automaton matches.
    fn starts_with(self) -> StartsWith<Self> where Self: Sized {
        StartsWith(self)
    }

    /// Returns an automaton that matches the strings matched by either this or
    /// the other automaton.
    fn union<Rhs: Automaton>(self, rhs: Rhs) -> Union<Self, Rhs> where Self: Sized {
        Union(self, rhs)
    }

    /// Returns an automaton that matches the strings matched by both this and
    /// the other automaton.
    fn intersection<Rhs: Automaton>(self, rhs: Rhs) -> Intersection<Self, Rhs> where Self: Sized {
        Intersection(self, rhs)
    }

    /// Returns an automaton that matches the strings not matched by this automaton.
    fn complement(self) -> Complement<Self> where Self: Sized {
        Complement(self)
    }
}

impl<'a, T: Automaton + ?Sized> Automaton for &'a T {
    type State = T::State;

    fn start(&self) -> Self::State {
        (*self).start()
    }

    fn is_match(&self, state: &Self::State) -> bool {
        (*self).is_match(state)
    }

    fn can_match(&self, state: &Self::State) -> bool {
        (*self).can_match(state)
    }

    fn will_always_match(&self, state: &Self::State) -> bool {
        (*self).will_always_match(state)
    }

    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        (*self).accept(state, byte)
    }
}

/// The `Automaton` state for `str`.
pub struct StringState(usize);

impl Automaton for str {
    type State = StringState;

    fn start(&self) -> StringState {
        StringState(0)
    }

    fn is_match(&self, state: &StringState) -> bool {
        state.0 == self.len()
    }

    fn can_match(&self, state: &StringState) -> bool {
        state.0 <= self.len()
    }

    fn will_always_match(&self, _state: &StringState) -> bool {
        false
    }

    fn accept(&self, state: &StringState, byte: u8) -> StringState {
        if state.0 < self.len() && self.as_bytes()[state.0] == byte {
            StringState(state.0 + 1)
        } else {
            StringState(usize::MAX)
        }
    }
}

/// The `Automaton` state for `[u8]`.
pub struct BytesState(usize);

impl Automaton for [u8] {
    type State = BytesState;

    fn start(&self) -> BytesState {
        BytesState(0)
    }

    fn is_match(&self, state: &BytesState) -> bool {
        state.0 == self.len()
    }

    fn can_match(&self, state: &BytesState) -> bool {
        state.0 <= self.len()
    }

    fn will_always_match(&self, _state: &BytesState) -> bool {
        false
    }

    fn accept(&self, state: &BytesState, byte: u8) -> BytesState {
        if state.0 < self.len() && self[state.0] == byte {
            BytesState(state.0 + 1)
        } else {
            BytesState(usize::MAX)
        }
    }
}

/// An automaton that always matches.
///
/// This is useful in a generic context as a way to express that no automaton
/// should be used.
pub struct AlwaysMatch;

impl Automaton for AlwaysMatch {
    type State = ();

    fn start(&self) -> () { () }
    fn is_match(&self, _: &()) -> bool { true }
    fn can_match(&self, _: &()) -> bool { true }
    fn will_always_match(&self, _: &()) -> bool { true }
    fn accept(&self, _: &(), _: u8) -> () { () }
}

/// An automaton that matches and string that begins with something that the
/// wrapped automaton matches.
pub struct StartsWith<A>(A);

/// The `Automaton` state for `StartsWith<A>`.
pub struct StartsWithState<A: Automaton>(StartsWithStateInternal<A>);

enum StartsWithStateInternal<A: Automaton> {
    Done,
    Running(A::State)
}

impl<A: Automaton> Automaton for StartsWith<A> {
    type State = StartsWithState<A>;
    fn start(&self) -> StartsWithState<A> {
        StartsWithState({
            let inner = self.0.start();
            if self.0.is_match(&inner) {
                Done
            } else {
                Running(inner)
            }
        })
    }

    fn is_match(&self, state: &StartsWithState<A>) -> bool {
        match state.0 {
            Done => true,
            Running(_) => false
        }
    }

    fn can_match(&self, state: &StartsWithState<A>) -> bool {
        match state.0 {
            Done => true,
            Running(ref inner) => self.0.can_match(inner)
        }
    }

    fn will_always_match(&self, state: &StartsWithState<A>) -> bool {
        match state.0 {
            Done => true,
            Running(_) => false
        }
    }

    fn accept(&self, state: &StartsWithState<A>, byte: u8) -> StartsWithState<A> {
        StartsWithState(
            match state.0 {
                Done => Done,
                Running(ref inner) => {
                    let next_inner = self.0.accept(inner, byte);
                    if self.0.is_match(&next_inner) {
                        Done
                    } else {
                        Running(next_inner)
                    }
                }
            }
        )
    } 
}

/// An automaton that matches when one of its component automata match.
pub struct Union<A, B>(A, B);

/// The `Automaton` state for `Union<A, B>`.
pub struct UnionState<A: Automaton, B: Automaton>(A::State, B::State);

impl<A: Automaton, B: Automaton> Automaton for Union<A, B> {
    type State = UnionState<A, B>;

    fn start(&self) -> UnionState<A, B> {
        UnionState(
            self.0.start(),
            self.1.start()
        )
    }

    fn is_match(&self, state: &UnionState<A, B>) -> bool {
        self.0.is_match(&state.0) || self.1.is_match(&state.1)
    }

    fn can_match(&self, state: &UnionState<A, B>) -> bool {
        self.0.can_match(&state.0) || self.1.can_match(&state.1)
    }

    fn will_always_match(&self, state: &UnionState<A, B>) -> bool {
        self.0.will_always_match(&state.0) || self.1.will_always_match(&state.1)
    }

    fn accept(&self, state: &UnionState<A, B>, byte: u8) -> UnionState<A, B> {
        UnionState(
            self.0.accept(&state.0, byte),
            self.1.accept(&state.1, byte)
        )
    }
}

/// An automaton that matches when both of its component automata match.
pub struct Intersection<A, B>(A, B);

/// The `Automaton` state for `Intersection<A, B>`.
pub struct IntersectionState<A: Automaton, B: Automaton>(A::State, B::State);

impl<A: Automaton, B: Automaton> Automaton for Intersection<A, B> {
    type State = IntersectionState<A, B>;

    fn start(&self) -> IntersectionState<A, B> {
        IntersectionState(
            self.0.start(),
            self.1.start()
        )
    }

    fn is_match(&self, state: &IntersectionState<A, B>) -> bool {
        self.0.is_match(&state.0) && self.1.is_match(&state.1)
    }

    fn can_match(&self, state: &IntersectionState<A, B>) -> bool {
        self.0.can_match(&state.0) && self.1.can_match(&state.1)
    }

    fn will_always_match(&self, state: &IntersectionState<A, B>) -> bool {
        self.0.will_always_match(&state.0) && self.1.will_always_match(&state.1)
    }

    fn accept(&self, state: &IntersectionState<A, B>, byte: u8) -> IntersectionState<A, B> {
        IntersectionState(
            self.0.accept(&state.0, byte),
            self.1.accept(&state.1, byte)
        )
    }
}

/// An automaton that matches exactly when the automaton it wraps does not.
pub struct Complement<A>(A);

/// The `Automaton` state for `Complement<A>`.
pub struct ComplementState<A: Automaton>(A::State);

impl<A: Automaton> Automaton for Complement<A> {
    type State = ComplementState<A>;

    fn start(&self) -> ComplementState<A> {
        ComplementState(self.0.start())
    }

    fn is_match(&self, state: &ComplementState<A>) -> bool {
        !self.0.is_match(&state.0)
    }

    fn can_match(&self, state: &ComplementState<A>) -> bool {
        !self.0.will_always_match(&state.0)
    }

    fn will_always_match(&self, state: &ComplementState<A>) -> bool {
        !self.0.can_match(&state.0)
    }

    fn accept(&self, state: &ComplementState<A>, byte: u8) -> ComplementState<A> {
        ComplementState(self.0.accept(&state.0, byte))
    }
}

#[cfg(test)]
mod test {
    use ::{IntoStreamer, Streamer, Levenshtein, Regex, Set, Automaton};
    use set::OpBuilder;

    static WORDS: &'static str = include_str!("../data/words-10000");

    fn get_set() -> Set {
        Set::from_iter(WORDS.lines()).unwrap()
    }

    #[test]
    fn str_small() {
        let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
        let set = Set::from_iter(keys).unwrap();
        let stream = set.search("foo").into_stream();

        let keys = stream.into_strs().unwrap();
        assert_eq!(keys, vec!["foo"]);
    }

    #[test]
    fn u8_slice_small() {
        let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
        let set = Set::from_iter(keys).unwrap();
        let stream = set.search("foo".as_bytes()).into_stream();

        let keys = stream.into_strs().unwrap();
        assert_eq!(keys, vec!["foo"]);
    }

    #[test]
    fn complement_small() {
        let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
        let set = Set::from_iter(keys).unwrap();
        let lev = Levenshtein::new("foo", 1).unwrap();
        let stream = set.search(lev.complement()).into_stream();

        let keys = stream.into_strs().unwrap();
        assert_eq!(keys, vec!["fa", "focus", "foul"]);
    }

    #[test]
    fn startswith_small() {
        let keys = vec!["", "cooing", "fa", "fo", "fob", "focus", "foo", "food", "foul", "fritter", "frothing"];
        let set = Set::from_iter(keys).unwrap();
        let lev = Levenshtein::new("foo", 1).unwrap();
        let stream = set.search(lev.starts_with()).into_stream();

        let keys = stream.into_strs().unwrap();
        assert_eq!(keys, vec!["cooing", "fo", "fob", "focus", "foo", "food", "foul", "frothing"]);
    }

    #[test]
    fn intersection_small() {
        let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
        let set = Set::from_iter(keys).unwrap();
        let lev = Levenshtein::new("foo", 1).unwrap();
        let reg = Regex::new("(..)*").unwrap();
        let stream = set.search(lev.intersection(reg)).into_stream();

        let keys = stream.into_strs().unwrap();
        assert_eq!(keys, vec!["fo", "food"]);
    }

    #[test]
    fn union_small() {
        let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
        let set = Set::from_iter(keys).unwrap();
        let lev = Levenshtein::new("foo", 1).unwrap();
        let reg = Regex::new("(..)*").unwrap();
        let stream = set.search(lev.union(reg)).into_stream();

        let keys = stream.into_strs().unwrap();
        assert_eq!(keys, vec!["fa", "fo", "fob", "foo", "food", "foul"]);
    }

    #[test]
    fn intersection_large() {
        let set = get_set();
        let lev = Levenshtein::new("foo", 3).unwrap();
        let reg = Regex::new("(..)*").unwrap();
        let mut stream1 = set.search((&lev).intersection(&reg)).into_stream();
        let mut stream2 = OpBuilder::new().add(set.search(&lev)).add(set.search(&reg)).intersection();
        while let Some(key1) = stream1.next() {
            assert_eq!(stream2.next(), Some(key1));
        }
        assert_eq!(stream2.next(), None);
    }

    #[test]
    fn union_large() {
        let set = get_set();
        let lev = Levenshtein::new("foo", 3).unwrap();
        let reg = Regex::new("(..)*").unwrap();
        let mut stream1 = set.search((&lev).union(&reg)).into_stream();
        let mut stream2 = OpBuilder::new().add(set.search(&lev)).add(set.search(&reg)).union();
        while let Some(key1) = stream1.next() {
            assert_eq!(stream2.next(), Some(key1));
        }
        assert_eq!(stream2.next(), None);
    }
}
