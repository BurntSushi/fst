fst
===
This crate provides a fast implementation of ordered sets and maps using finite
state machines. In particular, it makes use of finite state transducers to map
keys to values as the machine is executed. Using finite state machines as data
structures enables us to store keys in a compact format that is also easily
searchable. For example, this crate leverages memory maps to make range queries
very fast.

Check out my blog post
[Index 1,600,000,000 Keys with Automata and
Rust](http://blog.burntsushi.net/transducers/)
for extensive background, examples and experiments.

[![Build status](https://github.com/BurntSushi/fst/workflows/ci/badge.svg)](https://github.com/BurntSushi/fst/actions)
[![](http://meritbadge.herokuapp.com/fst)](https://crates.io/crates/fst)

Dual-licensed under MIT or the [UNLICENSE](http://unlicense.org).


### Documentation

https://docs.rs/fst

The
[`regex-automata`](https://docs.rs/regex-automata)
crate provides implementations of the `fst::Automata` trait when its
`transducer` feature is enabled. This permits using DFAs compiled by
`regex-automata` to search finite state transducers produced by this crate.


### Installation

Simply add a corresponding entry to your `Cargo.toml` dependency list:

```toml,ignore
[dependencies]
fst = "0.4"
```


### Example

This example demonstrates building a set in memory and executing a fuzzy query
against it. You'll need `fst = "0.4"` with the `levenshtein` feature enabled in
your `Cargo.toml`.

```rust
use fst::{IntoStreamer, Set};
use fst::automaton::Levenshtein;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  // A convenient way to create sets in memory.
  let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
  let set = Set::from_iter(keys)?;

  // Build our fuzzy query.
  let lev = Levenshtein::new("foo", 1,0,0)?;

  // Apply our fuzzy query to the set we built.
  let stream = set.search(lev).into_stream();

  let keys = stream.into_strs()?;
  assert_eq!(keys, vec!["fo", "fob", "foo", "food"]);
  Ok(())
}
```

Check out the documentation for a lot more examples!


### Cargo features

* `levenshtein` - **Disabled** by default. This adds the `Levenshtein`
  automaton to the `automaton` sub-module. This includes an additional
  dependency on `utf8-ranges`. Other features are as follows: (see 
  tests/levenshtein_test.rs for detail example)
  1)  Damerau-Levenshtein features support treating one exchange 
      operation between adjacent two characters as one operation,
      while it is two operations during classic levenshtein;
  2)  Support all levenshtein parameters which are completely same as 
      features in lucene fuzzy search and spell errors check, such as 
      prefix_length, max_expansions and so on. It also supports obtain 
      sorted results of levenshtein automaton by similarity; 
  3)  Support visualization of levenshtein automaton by generating 
      picture file through dot file,one effect drawing was shown as follows:
      
      <img src="https://github.com/apollo008/fst-levenshtein-0.4.4/blob/fst-levenshtein-0.4.4/data/lev.png" alt="levenshtein" width=375 />


