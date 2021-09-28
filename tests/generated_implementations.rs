#![cfg(feature = "macros")]

use fruit_salad::Dyncast;

#[derive(PartialEq, Dyncast)]
#[dyncast(Self, impl dyn PartialEq<dyn Dyncast>)]
struct Struct;

#[derive(PartialEq, Dyncast)]
#[dyncast(impl dyn PartialEq<dyn Dyncast>, Self)]
struct Struct2;

#[test]
#[allow(clippy::eq_op)] // Identical args are intentional.
fn test_partial_eq() {
	let a: &dyn Dyncast = Box::leak(Box::new(Struct));
	let b: &dyn Dyncast = Box::leak(Box::new(Struct2));

	assert_eq!(a, a);
	assert_eq!(b, b);
	assert_ne!(a, b);
	assert_ne!(b, a);
}
