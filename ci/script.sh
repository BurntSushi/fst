#!/bin/sh

set -ex

cargo doc --verbose
cargo build --verbose
cargo build --verbose --manifest-path fst-bin/Cargo.toml

# If we're testing on an older version of Rust, then only check that we
# can build the crate. This is because the dev dependencies might be updated
# more frequently, and therefore might require a newer version of Rust.
#
# This isn't ideal. It's a compromise.
if [ "$TRAVIS_RUST_VERSION" = "1.20.0" ]; then
  exit
fi

cargo test --verbose
cargo test --verbose  --lib --no-default-features
if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
  cargo bench --verbose --no-run
fi
