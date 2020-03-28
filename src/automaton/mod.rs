#[cfg(feature = "levenshtein")]
pub use self::levenshtein::{Levenshtein, LevenshteinError};

#[cfg(feature = "levenshtein")]
mod levenshtein;

/// Automaton describes types that behave as a finite automaton.
///
/// All implementors of this trait are represented by *byte based* automata.
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

    /// Returns true if and only if `state` matches and must match no matter
    /// what steps are taken.
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

    /// If applicable, return the next state when the end of a key is seen.
    fn accept_eof(&self, _: &Self::State) -> Option<Self::State> {
        None
    }

    /// Returns an automaton that matches the strings that start with something
    /// this automaton matches.
    fn starts_with(self) -> StartsWith<Self>
    where
        Self: Sized,
    {
        StartsWith(self)
    }

    /// Returns an automaton that matches the strings matched by either this or
    /// the other automaton.
    fn union<Rhs: Automaton>(self, rhs: Rhs) -> Union<Self, Rhs>
    where
        Self: Sized,
    {
        Union(self, rhs)
    }

    /// Returns an automaton that matches the strings matched by both this and
    /// the other automaton.
    fn intersection<Rhs: Automaton>(self, rhs: Rhs) -> Intersection<Self, Rhs>
    where
        Self: Sized,
    {
        Intersection(self, rhs)
    }

    /// Returns an automaton that matches the strings not matched by this
    /// automaton.
    fn complement(self) -> Complement<Self>
    where
        Self: Sized,
    {
        Complement(self)
    }
}

impl<'a, T: Automaton> Automaton for &'a T {
    type State = T::State;

    fn start(&self) -> T::State {
        (*self).start()
    }

    fn is_match(&self, state: &T::State) -> bool {
        (*self).is_match(state)
    }

    fn can_match(&self, state: &T::State) -> bool {
        (*self).can_match(state)
    }

    fn will_always_match(&self, state: &T::State) -> bool {
        (*self).will_always_match(state)
    }

    fn accept(&self, state: &T::State, byte: u8) -> T::State {
        (*self).accept(state, byte)
    }

    fn accept_eof(&self, state: &Self::State) -> Option<Self::State> {
        (*self).accept_eof(state)
    }
}

/// An automaton that matches if the input equals to a specific string.
///
/// It can be used in combination with [`StartsWith`] to search strings
/// starting with a given prefix.
///
/// ```rust
/// extern crate fst;
///
/// use fst::{Automaton, IntoStreamer, Streamer, Set};
/// use fst::automaton::Str;
///
/// # fn main() { example().unwrap(); }
/// fn example() -> Result<(), Box<dyn std::error::Error>> {
///     let paths = vec!["/home/projects/bar", "/home/projects/foo", "/tmp/foo"];
///     let set = Set::from_iter(paths)?;
///
///     // Build our prefix query.
///     let prefix = Str::new("/home").starts_with();
///
///     // Apply our query to the set we built.
///     let mut stream = set.search(prefix).into_stream();
///
///     let matches = stream.into_strs()?;
///     assert_eq!(matches, vec!["/home/projects/bar", "/home/projects/foo"]);
///     Ok(())
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Str<'a> {
    string: &'a [u8],
}

impl<'a> Str<'a> {
    /// Constructs automaton that matches an exact string.
    #[inline]
    pub fn new(string: &'a str) -> Str<'a> {
        Str { string: string.as_bytes() }
    }
}

impl<'a> Automaton for Str<'a> {
    type State = Option<usize>;

    #[inline]
    fn start(&self) -> Option<usize> {
        Some(0)
    }

    #[inline]
    fn is_match(&self, pos: &Option<usize>) -> bool {
        *pos == Some(self.string.len())
    }

    #[inline]
    fn can_match(&self, pos: &Option<usize>) -> bool {
        pos.is_some()
    }

    #[inline]
    fn accept(&self, pos: &Option<usize>, byte: u8) -> Option<usize> {
        // if we aren't already past the end...
        if let Some(pos) = *pos {
            // and there is still a matching byte at the current position...
            if self.string.get(pos).cloned() == Some(byte) {
                // then move forward
                return Some(pos + 1);
            }
        }
        // otherwise we're either past the end or didn't match the byte
        None
    }
}

/// An automaton that matches if the input contains a specific subsequence.
///
/// It can be used to build a simple fuzzy-finder.
///
/// ```rust
/// extern crate fst;
///
/// use fst::{IntoStreamer, Streamer, Set};
/// use fst::automaton::Subsequence;
///
/// # fn main() { example().unwrap(); }
/// fn example() -> Result<(), Box<dyn std::error::Error>> {
///     let paths = vec!["/home/projects/bar", "/home/projects/foo", "/tmp/foo"];
///     let set = Set::from_iter(paths)?;
///
///     // Build our fuzzy query.
///     let subseq = Subsequence::new("hpf");
///
///     // Apply our fuzzy query to the set we built.
///     let mut stream = set.search(subseq).into_stream();
///
///     let matches = stream.into_strs()?;
///     assert_eq!(matches, vec!["/home/projects/foo"]);
///     Ok(())
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Subsequence<'a> {
    subseq: &'a [u8],
}

impl<'a> Subsequence<'a> {
    /// Constructs automaton that matches input containing the
    /// specified subsequence.
    #[inline]
    pub fn new(subsequence: &'a str) -> Subsequence<'a> {
        Subsequence { subseq: subsequence.as_bytes() }
    }
}

impl<'a> Automaton for Subsequence<'a> {
    type State = usize;

    #[inline]
    fn start(&self) -> usize {
        0
    }

    #[inline]
    fn is_match(&self, &state: &usize) -> bool {
        state == self.subseq.len()
    }

    #[inline]
    fn can_match(&self, _: &usize) -> bool {
        true
    }

    #[inline]
    fn will_always_match(&self, &state: &usize) -> bool {
        state == self.subseq.len()
    }

    #[inline]
    fn accept(&self, &state: &usize, byte: u8) -> usize {
        if state == self.subseq.len() {
            return state;
        }
        state + (byte == self.subseq[state]) as usize
    }
}

/// An automaton that always matches.
///
/// This is useful in a generic context as a way to express that no automaton
/// should be used.
#[derive(Clone, Debug)]
pub struct AlwaysMatch;

impl Automaton for AlwaysMatch {
    type State = ();

    #[inline]
    fn start(&self) -> () {
        ()
    }
    #[inline]
    fn is_match(&self, _: &()) -> bool {
        true
    }
    #[inline]
    fn can_match(&self, _: &()) -> bool {
        true
    }
    #[inline]
    fn will_always_match(&self, _: &()) -> bool {
        true
    }
    #[inline]
    fn accept(&self, _: &(), _: u8) -> () {
        ()
    }
}

/// An automaton that matches a string that begins with something that the
/// wrapped automaton matches.
#[derive(Clone, Debug)]
pub struct StartsWith<A>(A);

/// The `Automaton` state for `StartsWith<A>`.
pub struct StartsWithState<A: Automaton>(StartsWithStateKind<A>);

enum StartsWithStateKind<A: Automaton> {
    Done,
    Running(A::State),
}

impl<A: Automaton> Automaton for StartsWith<A> {
    type State = StartsWithState<A>;

    fn start(&self) -> StartsWithState<A> {
        StartsWithState({
            let inner = self.0.start();
            if self.0.is_match(&inner) {
                StartsWithStateKind::Done
            } else {
                StartsWithStateKind::Running(inner)
            }
        })
    }

    fn is_match(&self, state: &StartsWithState<A>) -> bool {
        match state.0 {
            StartsWithStateKind::Done => true,
            StartsWithStateKind::Running(_) => false,
        }
    }

    fn can_match(&self, state: &StartsWithState<A>) -> bool {
        match state.0 {
            StartsWithStateKind::Done => true,
            StartsWithStateKind::Running(ref inner) => self.0.can_match(inner),
        }
    }

    fn will_always_match(&self, state: &StartsWithState<A>) -> bool {
        match state.0 {
            StartsWithStateKind::Done => true,
            StartsWithStateKind::Running(_) => false,
        }
    }

    fn accept(
        &self,
        state: &StartsWithState<A>,
        byte: u8,
    ) -> StartsWithState<A> {
        StartsWithState(match state.0 {
            StartsWithStateKind::Done => StartsWithStateKind::Done,
            StartsWithStateKind::Running(ref inner) => {
                let next_inner = self.0.accept(inner, byte);
                if self.0.is_match(&next_inner) {
                    StartsWithStateKind::Done
                } else {
                    StartsWithStateKind::Running(next_inner)
                }
            }
        })
    }
}

/// An automaton that matches when one of its component automata match.
#[derive(Clone, Debug)]
pub struct Union<A, B>(A, B);

/// The `Automaton` state for `Union<A, B>`.
pub struct UnionState<A: Automaton, B: Automaton>(A::State, B::State);

impl<A: Automaton, B: Automaton> Automaton for Union<A, B> {
    type State = UnionState<A, B>;

    fn start(&self) -> UnionState<A, B> {
        UnionState(self.0.start(), self.1.start())
    }

    fn is_match(&self, state: &UnionState<A, B>) -> bool {
        self.0.is_match(&state.0) || self.1.is_match(&state.1)
    }

    fn can_match(&self, state: &UnionState<A, B>) -> bool {
        self.0.can_match(&state.0) || self.1.can_match(&state.1)
    }

    fn will_always_match(&self, state: &UnionState<A, B>) -> bool {
        self.0.will_always_match(&state.0)
            || self.1.will_always_match(&state.1)
    }

    fn accept(&self, state: &UnionState<A, B>, byte: u8) -> UnionState<A, B> {
        UnionState(
            self.0.accept(&state.0, byte),
            self.1.accept(&state.1, byte),
        )
    }
}

/// An automaton that matches when both of its component automata match.
#[derive(Clone, Debug)]
pub struct Intersection<A, B>(A, B);

/// The `Automaton` state for `Intersection<A, B>`.
pub struct IntersectionState<A: Automaton, B: Automaton>(A::State, B::State);

impl<A: Automaton, B: Automaton> Automaton for Intersection<A, B> {
    type State = IntersectionState<A, B>;

    fn start(&self) -> IntersectionState<A, B> {
        IntersectionState(self.0.start(), self.1.start())
    }

    fn is_match(&self, state: &IntersectionState<A, B>) -> bool {
        self.0.is_match(&state.0) && self.1.is_match(&state.1)
    }

    fn can_match(&self, state: &IntersectionState<A, B>) -> bool {
        self.0.can_match(&state.0) && self.1.can_match(&state.1)
    }

    fn will_always_match(&self, state: &IntersectionState<A, B>) -> bool {
        self.0.will_always_match(&state.0)
            && self.1.will_always_match(&state.1)
    }

    fn accept(
        &self,
        state: &IntersectionState<A, B>,
        byte: u8,
    ) -> IntersectionState<A, B> {
        IntersectionState(
            self.0.accept(&state.0, byte),
            self.1.accept(&state.1, byte),
        )
    }
}

/// An automaton that matches exactly when the automaton it wraps does not.
#[derive(Clone, Debug)]
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

    fn accept(
        &self,
        state: &ComplementState<A>,
        byte: u8,
    ) -> ComplementState<A> {
        ComplementState(self.0.accept(&state.0, byte))
    }
}
