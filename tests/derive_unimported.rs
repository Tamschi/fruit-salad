#![cfg(feature = "macros")]

use static_assertions::{assert_impl_all, assert_not_impl_any};
use std::{fmt::Debug, marker::PhantomData};

#[derive(fruit_salad::Dyncast)]
enum Enum {}
assert_impl_all!(Enum: fruit_salad::Dyncast);

#[derive(fruit_salad::Dyncast)]
struct Struct;
assert_impl_all!(Struct: fruit_salad::Dyncast);

// A `Self: 'static` bound is added automatically.
#[derive(fruit_salad::Dyncast)]
#[dyncast(#![runtime_pointer_size_assertion] Self)]
struct GenericStruct<X>(PhantomData<X>);
assert_impl_all!(GenericStruct<()>: fruit_salad::Dyncast);

#[derive(fruit_salad::Dyncast)]
union Union {
	_x: usize,
}
assert_impl_all!(Union: fruit_salad::Dyncast);

trait Trait {}
fruit_salad::implement_dyncasts!(dyn Trait);
assert_impl_all!(dyn Trait: fruit_salad::Dyncast);

// The `impl` coverage can be limited this way too:
struct Test<X>(PhantomData<X>);
fruit_salad::implement_dyncasts!(Test<X: Debug>);
assert_impl_all!(Test<()>: fruit_salad::Dyncast);
assert_not_impl_any!(Test<Test<()>>: fruit_salad::Dyncast);

trait Test2<X> {}
fruit_salad::implement_dyncasts!(dyn Test2<X> where X: Debug);
assert_impl_all!(dyn Test2<()>: fruit_salad::Dyncast);
assert_not_impl_any!(dyn Test2<Test<()>>: fruit_salad::Dyncast);
