#![doc(hidden)]

use core::mem::MaybeUninit;

#[cfg(feature = "macros")]
pub use static_assertions::const_assert;

pub unsafe fn deallocate_owned<T>(raw: *mut T) {
	#[cfg(feature = "alloc")]
	drop(::alloc::boxed::Box::<MaybeUninit<T>>::from_raw(raw.cast()))
}
