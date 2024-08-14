use crate::automaton::{Automaton, StartsWith};

/// An automaton that matches if the input contains to a specific string.
///
/// ```rust
/// extern crate fst;
///
/// use fst::{Automaton, IntoStreamer, Streamer, Set};
/// use fst::automaton::Contains;
///
/// # fn main() { example().unwrap(); }
/// fn example() -> Result<(), Box<dyn std::error::Error>> {
///     let paths = vec!["/home/projects/bar", "/home/projects/foo", "/tmp/foo"];
///     let set = Set::from_iter(paths)?;
///
///     // Build our contains query.
///     let keyword = Contains::new("/projects");
///
///     // Apply our query to the set we built.
///     let mut stream = set.search(keyword).into_stream();
///
///     let matches = stream.into_strs()?;
///     assert_eq!(matches, vec!["/home/projects/bar", "/home/projects/foo"]);
///     Ok(())
/// }
/// ```
#[derive(Clone, Debug)]
pub struct Contains<'a> {
    string: &'a [u8],
}

impl<'a> Contains<'a> {
    /// Constructs automaton that matches an exact string.
    #[inline]
    pub fn new(string: &'a str) -> Contains<'a> {
        Self { string: string.as_bytes() }
    }
}

impl<'a> Automaton for Contains<'a> {
    type State = Option<usize>;

    #[inline]
    fn start(&self) -> Option<usize> {
        Some(0)
    }

    #[inline]
    fn is_match(&self, pos: &Option<usize>) -> bool {
        pos.is_some() && pos.unwrap() >= self.string.len()
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
            } else {
                if pos >= self.string.len() {
                    // if we're past the end, then we're done
                    return Some(i32::MAX as usize);
                } else {
                    return Some(0);
                }
            }
        }
        // otherwise we're either past the end or didn't match the byte
        None
    }
}
