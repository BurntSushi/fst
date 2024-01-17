fst no-std mode
===

This is a fork of [fst](https://github.com/BurntSushi/fst) adding support for `no_std` targets (see [`no_std` usage](#no-std-usage) for details).

If you're unsure whether to use this fork or the original one: Just use the original, chances are that's more up-to-date.

### Documentation

https://docs.rs/fst-no-std

### Installation

Simply add a corresponding entry to your `Cargo.toml` dependency list:

```toml,ignore
[dependencies]
fst-no-std = "0.4"
```

### Example

This example demonstrates building a set in memory and executing a fuzzy query
against it. You'll need `fst_no_std = "0.4"` with the `levenshtein` feature enabled in
your `Cargo.toml`.

```rust
use fst_no_std::{IntoStreamer, Set};
use fst_no_std::automaton::Levenshtein;

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
```

Check out the documentation for a lot more examples!

### Cargo features

* `std` - **Enabled** by default. Adds features that depend on the standard library.
* `alloc` - **Enabled** by default. Adds features that depend on `alloc`.
* `levenshtein` - **Disabled** by default. This adds the `Levenshtein`
  automaton to the `automaton` sub-module. This includes an additional
  dependency on `utf8-ranges` and `std`.

### `no_std` Usage

You can use this crate in `no_std` environments by disabling default features, like so:

```toml,ignore
[dependencies]
fst-no-std = { version = "0.4", default-features = false }
```

This way `fst-no-std` will not depend on the standard library and not even allocate (!) at the cost of being rather kneecaped: You can not construct FSTs and the evailable querying features are limited to simple lookups. You can optionally enable the `alloc` feature which adds a dependency on the `alloc` crate (i.e. you will need a global allocator) but it enables all querying features.

#### License

<sup>
Licensed under the <a href="LICENSE-MIT">MIT license</a>.
</sup>
