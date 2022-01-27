#![doc(hidden)]

use core::{cell::UnsafeCell, mem::ManuallyDrop};

#[cfg(feature = "macros")]
pub use static_assertions::const_assert;

/// Deallocates a global-allocator allocation of `T`, iff the `"alloc"` feature is enabled.
///
/// # Safety
///
/// The target of `raw` currently always must be initialised.
///
/// > The requirements can be loosened a bit once the feature [`layout_for_ptr`](https://github.com/rust-lang/rust/issues/69835) is stabilised.
pub unsafe fn deallocate_owned<T: ?Sized>(raw: *mut T) {
	#[cfg(feature = "alloc")]
	drop(::alloc::boxed::Box::<ManuallyDrop<UnsafeCell<T>>>::from_raw(raw as *mut _))
}
