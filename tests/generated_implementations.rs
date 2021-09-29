#![forbid(unsafe_code)]
#![cfg(feature = "macros")]

use core::fmt::Debug;
use fruit_salad::Dyncast; // With feature `"macros"`.

#[derive(PartialEq, Dyncast, Hash)]
#[dyncast(Self, impl dyn DynHash)]
struct A;

#[derive(Debug, PartialEq, PartialOrd, Dyncast)]
#[dyncast(Self, dyn Debug)]
#[dyncast(impl dyn PartialEq<dyn Dyncast>, impl dyn PartialOrd<dyn Dyncast>)]
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

	assert_eq!(format!("{:?}", a), "dyn Dyncast = !dyn Debug");
	assert_eq!(format!("{:?}", b), "dyn Dyncast = B");

	assert!(a.dyncast::<dyn Debug>().is_none());
	assert!(b.dyncast::<dyn Debug>().is_some());

	// Also: `…_mut`, `…_pinned`, `…_box` and combinations thereof, as well as `…ptr`.
	// `…box` methods require the `"alloc"` feature.
	let _a: &A = a.dyncast().unwrap();
	let _b: &B = b.dyncast().unwrap();
}
