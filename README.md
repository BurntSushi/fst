fst
===
This crate provides a fast implementation of ordered sets and maps using finite
state machines. In particular, it makes use of finite state transducers to map
keys to values as the machine is executed. Using finite state machines as data
structures enables us to store keys in a compact format that is also easily
searchable. For example, this crate levages memory maps to make range queries,
regular expression queries and Levenshtein (edit) distance queries very fast.

Check out my blog post
[Index 1,600,000,000 Keys with Automata and
Rust](http://blog.burntsushi.net/transducers/)
for extensive background, examples and experiments.

[![Linux build status](https://api.travis-ci.org/BurntSushi/fst.png)](https://travis-ci.org/BurntSushi/fst)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/BurntSushi/fst?svg=true)](https://ci.appveyor.com/project/BurntSushi/fst)
[![](http://meritbadge.herokuapp.com/fst)](https://crates.io/crates/fst)

Dual-licensed under MIT or the [UNLICENSE](http://unlicense.org).


### Documentation

[Full API documentation and examples.](http://burntsushi.net/rustdoc/fst/)


### Installation

Simply add a corresponding entry to your `Cargo.toml` dependency list:

```ignore
[dependencies]
fst = "0.1"
```

And add this to your crate root:

```ignore
extern crate fst;
```


### Example

This example demonstrates building a set in memory and executing a fuzzy query
against it. Check out the documentation for a lot more examples!

```rust
use fst::{IntoStreamer, Streamer, Levenshtein, Set};

// A convenient way to create sets in memory.
let keys = vec!["fa", "fo", "fob", "focus", "foo", "food", "foul"];
let set = try!(Set::from_iter(keys));

// Build our fuzzy query.
let lev = try!(Levenshtein::new("foo", 1));

// Apply our fuzzy query to the set we built.
let mut stream = set.search(lev).into_stream();

let keys = try!(stream.into_strs());
assert_eq!(keys, vec!["fo", "fob", "foo", "food"]);
```
