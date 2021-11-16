# fruit-salad

[![Lib.rs](https://img.shields.io/badge/Lib.rs-*-84f)](https://lib.rs/crates/fruit-salad)
[![Crates.io](https://img.shields.io/crates/v/fruit-salad)](https://crates.io/crates/fruit-salad)
[![Docs.rs](https://docs.rs/fruit-salad/badge.svg)](https://docs.rs/fruit-salad)

![Rust 1.51](https://img.shields.io/static/v1?logo=Rust&label=&message=1.51&color=grey)
[![CI](https://github.com/Tamschi/fruit-salad/workflows/CI/badge.svg?branch=unstable)](https://github.com/Tamschi/fruit-salad/actions?query=workflow%3ACI+branch%3Aunstable)
![Crates.io - License](https://img.shields.io/crates/l/fruit-salad/0.0.2)

[![GitHub](https://img.shields.io/static/v1?logo=GitHub&label=&message=%20&color=grey)](https://github.com/Tamschi/fruit-salad)
[![open issues](https://img.shields.io/github/issues-raw/Tamschi/fruit-salad)](https://github.com/Tamschi/fruit-salad/issues)
[![open pull requests](https://img.shields.io/github/issues-pr-raw/Tamschi/fruit-salad)](https://github.com/Tamschi/fruit-salad/pulls)
[![good first issues](https://img.shields.io/github/issues-raw/Tamschi/fruit-salad/good%20first%20issue?label=good+first+issues)](https://github.com/Tamschi/fruit-salad/contribute)

[![crev reviews](https://web.crev.dev/rust-reviews/badge/crev_count/fruit-salad.svg)](https://web.crev.dev/rust-reviews/crate/fruit-salad/)
[![Zulip Chat](https://img.shields.io/endpoint?label=chat&url=https%3A%2F%2Fiteration-square-automation.schichler.dev%2F.netlify%2Ffunctions%2Fstream_subscribers_shield%3Fstream%3Dproject%252Ffruit-salad)](https://iteration-square.schichler.dev/#narrow/stream/project.2Ffruit-salad)

This is a (mostly) trait object casting and comparison crate.

There is no registry, instead targets are engraved directly into the `Dyncast` trait implementation by a derive macro.

Concrete types can be targeted too, unsafely through reinterpret casts.  
(This is subject to `#[deny(unsafe_code)]`.)

It also does mutability and pin projection, while being economical regarding text size…

> Basically I needed something that's a bit less fancy than the existing solutions,
> and it escalated a bit from there.

Originally developed as part of [`rhizome`](https://crates.io/crates/rhizome) but now separate,
this crate also works very well in combination with [`pinus`](https://crates.io/crates/pinus).

## Installation

Please use [cargo-edit](https://crates.io/crates/cargo-edit) to always add the latest version of this library:

```cmd
cargo add fruit-salad --features macros
```

## Example

```rust
#[cfg(feature = "macros")]
{

#![allow(clippy::eq_op)] // Identical args are intentional.

use core::fmt::Debug;
use fruit_salad::Dyncast; // With feature `"macros"`.

#[derive(PartialEq, Dyncast, Hash)]
#[dyncast(Self, impl dyn DynHash)]
struct A;

#[derive(Debug, PartialEq, PartialOrd, Dyncast)]
#[dyncast(Self, dyn Debug)]
#[dyncast(impl dyn PartialEq<dyn Dyncast>, impl dyn PartialOrd<dyn Dyncast>)]
struct B;

let a: &dyn Dyncast = &A;
let b: &dyn Dyncast = &B;

assert_ne!(a, a); // Partial equality isn't exposed.
assert_eq!(b, b);
assert_ne!(a, b);
assert_ne!(b, a);

assert_eq!(a.partial_cmp(a), None); // Partial order isn't exposed.
assert_eq!(b.partial_cmp(b), Some(core::cmp::Ordering::Equal));
assert_eq!(a.partial_cmp(b), None);
assert_eq!(b.partial_cmp(a), None);

assert_eq!(format!("{:?}", a), "dyn Dyncast = !dyn Debug");
assert_eq!(format!("{:?}", b), "dyn Dyncast = B");

assert!(a.dyncast::<dyn Debug>().is_none());
assert!(b.dyncast::<dyn Debug>().is_some());

// Also: `…_mut`, `…_pinned`, `…_box` and combinations thereof, as well as `…ptr`.
// `…box` methods require the `"alloc"` feature.
let _a: &A = a.dyncast().unwrap();
let _b: &B = b.dyncast().unwrap();

}
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

Note that dependencies of this crate may have a more lenient MSRV policy!
Please use `cargo +nightly update -Z minimal-versions` in your automation if you don't generate Cargo.lock manually (or as necessary) and require support for a compiler older than current stable.
