#![cfg(feature = "macros")]

use fruit_salad::{implement_dyncasts, Dyncast};
use static_assertions::{assert_impl_all, assert_not_impl_any};
use std::{fmt::Debug, marker::PhantomData};

#[derive(Dyncast)]
enum Enum {}
assert_impl_all!(Enum: Dyncast);

#[derive(Dyncast)]
struct Struct;
assert_impl_all!(Struct: Dyncast);

// A `Self: 'static` bound is added automatically.
#[derive(Dyncast)]
#[dyncast(#![runtime_pointer_size_assertion] Self)]
struct GenericStruct<X>(PhantomData<X>);
assert_impl_all!(GenericStruct<()>: Dyncast);

#[derive(Dyncast)]
union Union {
	_x: usize,
}
assert_impl_all!(Union: Dyncast);

trait Trait {}
implement_dyncasts!(dyn Trait);
assert_impl_all!(dyn Trait: Dyncast);

// The `impl` coverage can be limited this way too:
struct Test<X>(PhantomData<X>);
implement_dyncasts!(Test<X: Debug>);
assert_impl_all!(Test<()>: Dyncast);
assert_not_impl_any!(Test<Test<()>>: Dyncast);

trait Test2<X> {}
implement_dyncasts!(dyn Test2<X> where X: Debug);
assert_impl_all!(dyn Test2<()>: Dyncast);
assert_not_impl_any!(dyn Test2<Test<()>>: Dyncast);
