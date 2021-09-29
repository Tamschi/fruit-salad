#![cfg(feature = "macros")]

use fruit_salad::Dyncast;

#[derive(PartialEq, Dyncast, Hash)]
#[dyncast(Self, impl dyn DynHash)]
struct A;

#[derive(PartialEq, PartialOrd, Dyncast)]
#[dyncast(impl dyn PartialEq<dyn Dyncast>, Self, impl dyn PartialOrd<dyn Dyncast>)]
struct B;

#[test]
#[allow(clippy::eq_op)] // Identical args are intentional.
fn test_partial_eq() {
	let a: &dyn Dyncast = Box::leak(Box::new(A));
	let b: &dyn Dyncast = Box::leak(Box::new(B));

	assert_ne!(a, a); // Partial equality isn't exposed.
	assert_eq!(b, b);
	assert_ne!(a, b);
	assert_ne!(b, a);

	assert_eq!(a.partial_cmp(a), None); // Partial ordering isn't exposed.
	assert_eq!(b.partial_cmp(b), Some(core::cmp::Ordering::Equal));
	assert_eq!(a.partial_cmp(b), None);
	assert_eq!(b.partial_cmp(a), None);
}
