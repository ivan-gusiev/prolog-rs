# prolog-rs

WAM Prolog implementation in Rust.

## Getting started

```bash
cargo run # runs a REPL
```

### Testing

Similar to any rust project, `cargo test` will do the trick.

Parameterized tests and snapshot tests (AKA golden or approval) are employed.
This is achieved using `parameterized` and `insta` crates correspondingly.

Therefore, the following commands are most useful for development and testing:

```bash
cargo test # run all tests
just review # review snapshots
just watch # watch all tests
just watch executetests # watch a subset of all tests
```