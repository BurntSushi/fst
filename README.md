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
[`fst-regex`](https://docs.rs/fst-regex)
and
[`fst-levenshtein`](https://docs.rs/fst-levenshtein)
crates provide regular expression matching and fuzzy searching on FSTs,
respectively.


### Installation

Simply add a corresponding entry to your `Cargo.toml` dependency list:

```toml,ignore
[dependencies]
fst = "0.3"
```


### Example

This example demonstrates building a set in memory and executing a fuzzy query
against it. You'll need `fst = "0.3"` and `fst-levenshtein = "0.2"` in your
`Cargo.toml`.

```rust
extern crate fst;
extern crate fst_levenshtein;

use std::error::Error;
use std::process;

use fst::{IntoStreamer, Set};
use fst_levenshtein::Levenshtein;

fn try_main() -> Result<(), Box<Error>> {
  // A convenient way to create sets in memory.
  let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
  let set = Set::from_iter(keys)?;

  // Build our fuzzy query.
  let lev = Levenshtein::new("foo", 1)?;

  // Apply our fuzzy query to the set we built.
  let stream = set.search(lev).into_stream();

  let keys = stream.into_strs()?;
  assert_eq!(keys, vec!["fo", "fob", "foo", "food"]);
  Ok(())
}

fn main() {
  if let Err(err) = try_main() {
    eprintln!("{}", err);
    process::exit(1);
  }
}
```

Check out the documentation for a lot more examples!
