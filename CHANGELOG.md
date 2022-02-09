# fruit-salad Changelog

<!-- markdownlint-disable no-trailing-punctuation -->

## next

TODO: Date

- **Breaking changes**:
  - Increased required Rust version to 1.54
    > due to rust-template upgrade (better testing).
  - Deriving a `Dyncast` implementation that targets `Self` with generics now requires `#![runtime_pointer_size_assertion] Self` instead of just `Self`.
    > This also potentially closes a memory safety issue on platforms where `usize` is larger than the vtable pointer, if Rust at some point gains the ability to have trait objects of unsized type instances.

- Features:
  - It's now possible to target arbitrary types using outer generics:

    ```rust
    #[derive(Dyncast)]
    #[dyncast(#![runtime_pointer_size_assertion] unsafe T)]
    #[repr(transparent)]
    struct DyncastWrapper<T>(pub T);
    ```

  - It's now possible to combine a reinterpret-cast and unsizing:

     ```rust
     #[derive(Dyncast)]
     #[dyncast(unsafe T as dyn Trait)]
     #[repr(transparent)]
     struct DyncastWrapper<T: Trait>(pub T);
     ```

- Revisions:
  - Fixed issue where a bunch of code would be printed into the documentation instead of just the type.
  - Dyncast targets emitted into the documentation are now links.
  - `Dyncast` is always fully qualified now when emitted from the derive macros.

## 0.0.2

2021-11-16

- Revisions:
  - Workaround for [rust#89190: Trait upcasting shadows (trait object) deref coercion](https://github.com/rust-lang/rust/issues/89190) hitting stable before its fix.
  - Added missing documentation.

## 0.0.1

2021-09-29

Initial unstable release
