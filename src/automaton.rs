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
    fn can_match(&self, state: &Self::State) -> bool;

    /// Return the next state given `state` and an input.
    fn accept(&self, state: &Self::State, byte: u8) -> Self::State;
}

impl<'a, T: Automaton> Automaton for &'a T {
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

    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        (*self).accept(state, byte)
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
    fn accept(&self, _: &(), _: u8) -> () { () }
}
