# fruit-salad

[![Lib.rs](https://img.shields.io/badge/Lib.rs-*-84f)](https://lib.rs/crates/fruit-salad)
[![Crates.io](https://img.shields.io/crates/v/fruit-salad)](https://crates.io/crates/fruit-salad)
[![Docs.rs](https://docs.rs/fruit-salad/badge.svg)](https://docs.rs/fruit-salad)

![Rust 1.51](https://img.shields.io/static/v1?logo=Rust&label=&message=1.51&color=grey)
[![CI](https://github.com/Tamschi/fruit-salad/workflows/CI/badge.svg?branch=develop)](https://github.com/Tamschi/fruit-salad/actions?query=workflow%3ACI+branch%3Adevelop)
![Crates.io - License](https://img.shields.io/crates/l/fruit-salad/0.0.1)

[![GitHub](https://img.shields.io/static/v1?logo=GitHub&label=&message=%20&color=grey)](https://github.com/Tamschi/fruit-salad)
[![open issues](https://img.shields.io/github/issues-raw/Tamschi/fruit-salad)](https://github.com/Tamschi/fruit-salad/issues)
[![open pull requests](https://img.shields.io/github/issues-pr-raw/Tamschi/fruit-salad)](https://github.com/Tamschi/fruit-salad/pulls)
[![good first issues](https://img.shields.io/github/issues-raw/Tamschi/fruit-salad/good%20first%20issue?label=good+first+issues)](https://github.com/Tamschi/fruit-salad/contribute)

[![crev reviews](https://web.crev.dev/rust-reviews/badge/crev_count/fruit-salad.svg)](https://web.crev.dev/rust-reviews/crate/fruit-salad/)

This is a (mostly) trait object **reference** casting and comparison crate.

There is no registry, instead targets are engraved directly into the `Dyncast` trait implementation by a derive macro.

Concrete types can be targeted too, unsafely through reinterpret casts.  
(This is subject to `#[deny(unsafe)]`. (TODO))

It also does mutability and pin projection, while being economical regarding text size…

> Basically I needed something that's a bit less fancy than the existing solutions,
> and it escalated a bit for there.

Originally developed as part of [`rhizome`](https://crates.io/crates/rhizome) but now separate,
this crate also works very well in combination with [`pinus`](https://crates.io/crates/pinus).

## Installation

Please use [cargo-edit](https://crates.io/crates/cargo-edit) to always add the latest version of this library:

```cmd
cargo add fruit-salad --features macros
```

## Example

```rust
// TODO_EXAMPLE
```

## License

Licensed under either of

- Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license
   ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

See [CONTRIBUTING](CONTRIBUTING.md) for more information.

## [Code of Conduct](CODE_OF_CONDUCT.md)

## [Changelog](CHANGELOG.md)

## Versioning

`fruit-salad` strictly follows [Semantic Versioning 2.0.0](https://semver.org/spec/v2.0.0.html) with the following exceptions:

- The minor version will not reset to 0 on major version changes (except for v1).  
Consider it the global feature level.
- The patch version will not reset to 0 on major or minor version changes (except for v0.1 and v1).  
Consider it the global patch level.

This includes the Rust version requirement specified above.  
Earlier Rust versions may be compatible, but this can change with minor or patch releases.

Which versions are affected by features and patches can be determined from the respective headings in [CHANGELOG.md](CHANGELOG.md).
