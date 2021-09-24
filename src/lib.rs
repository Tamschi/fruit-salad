//! This is a (mostly) trait object **reference** casting and comparison crate.
//!
//! There is no registry, instead targets are engraved directly into the `Dyncast` trait implementation by a derive macro.
//!
//! Concrete types can be targeted too, unsafely through reinterpret casts.  
//! (This is subject to `#[deny(unsafe)]`. (TODO))
//!
//! It also does mutability and pin projection, while being economical regarding text size…
//!
//! > Basically I needed something that's a bit less fancy than the existing solutions,
//! > and it escalated a bit from there.
//!
//! Originally developed as part of [`rhizome`](https://crates.io/crates/rhizome) but now separate,
//! this crate also works very well in combination with [`pinus`](https://crates.io/crates/pinus).
//!
//! ## Installation
//!
//! Please use [cargo-edit](https://crates.io/crates/cargo-edit) to always add the latest version of this library:
//!
//! ```cmd
//! cargo add fruit-salad --features macros
//! ```
//!
//! ## Example
//!
//! ```rust
//! // TODO_EXAMPLE
//! ```

#![doc(html_root_url = "https://docs.rs/fruit-salad/0.0.1")]
#![warn(clippy::pedantic)]
#![allow(clippy::semicolon_if_nothing_returned)]
#![no_std]

#[cfg(feature = "std")]
extern crate std;

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

use core::{
	any::{Any, TypeId},
	cmp::Ordering,
	fmt::{self, Debug, Display},
	hash::{Hash, Hasher},
	mem::{self, MaybeUninit},
	ops::{Deref, DerefMut},
	pin::Pin,
	ptr::NonNull,
};

#[cfg(feature = "macros")]
pub use fruit_salad_proc_macro_definitions::{implement_dyncasts, Dyncast};

pub trait Limit<'a> {
	type Limited: 'a + ?Sized;
}
impl<'a, T: 'a + ?Sized> Limit<'a> for T {
	type Limited = T;
}

pub fn test<'a, T: 'static + ?Sized>(t: &'a <T as Limit<'_>>::Limited) -> &'a T {
	t
}

impl<'a> dyn 'a + Dyncast {
	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_<T: 'static + ?Sized>(&self) -> Option<&<T as Limit<'a>>::Limited> {
		self.__dyncast(
			unsafe { NonNull::new_unchecked(self as *const Self as *mut Self) }.cast(),
			TypeId::of::<T>(),
		)
		.map(|pointer_data| unsafe {
			#[allow(clippy::cast_ptr_alignment)] // Read unaligned.
			pointer_data
				.as_ptr()
				.cast::<&<T as Limit<'a>>::Limited>()
				.read_unaligned()
		})
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_mut_<T: 'static + ?Sized>(&mut self) -> Option<&mut <T as Limit<'a>>::Limited> {
		let this = unsafe { NonNull::new_unchecked(self) };
		self.__dyncast(this.cast(), TypeId::of::<T>())
			.map(|pointer_data| unsafe {
				pointer_data
					.as_ptr()
					.cast::<&mut <T as Limit<'a>>::Limited>()
					.read_unaligned()
			})
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned_<T: 'static + ?Sized>(
		self: Pin<&Self>,
	) -> Option<Pin<&<T as Limit<'a>>::Limited>> {
		self.__dyncast(
			unsafe { NonNull::new_unchecked(&*self as *const Self as *mut Self) }.cast(),
			TypeId::of::<T>(),
		)
		.map(|pointer_data| unsafe {
			pointer_data
				.as_ptr()
				.cast::<Pin<&<T as Limit<'a>>::Limited>>()
				.read_unaligned()
		})
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned_mut_<T: 'static + ?Sized>(
		mut self: Pin<&mut Self>,
	) -> Option<Pin<&mut <T as Limit<'a>>::Limited>> {
		let this = unsafe {
			NonNull::new_unchecked(Pin::into_inner_unchecked(self.as_mut()) as *mut Self)
		};
		self.__dyncast(this.cast(), TypeId::of::<T>())
			.map(|pointer_data| unsafe {
				pointer_data
					.as_ptr()
					.cast::<Pin<&mut <T as Limit<'a>>::Limited>>()
					.read_unaligned()
			})
	}

	/// # Safety
	///
	/// See [`NonNull::as_ref`].
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_ptr_<T: 'static + ?Sized>(
		this: NonNull<Self>,
	) -> Option<NonNull<<T as Limit<'a>>::Limited>> {
		this.as_ref()
			.__dyncast(this.cast(), TypeId::of::<T>())
			.map(|pointer_data| {
				pointer_data
					.as_ptr()
					.cast::<NonNull<<T as Limit<'a>>::Limited>>()
					.read_unaligned()
			})
	}
}

impl dyn Dyncast {
	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast<T: 'static + ?Sized>(&self) -> Option<&T> {
		self.dyncast_::<T>()
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_mut<T: 'static + ?Sized>(&mut self) -> Option<&mut T> {
		self.dyncast_mut_::<T>()
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned<T: 'static + ?Sized>(self: Pin<&Self>) -> Option<Pin<&T>> {
		self.dyncast_pinned_::<T>()
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned_mut<T: 'static + ?Sized>(self: Pin<&mut Self>) -> Option<Pin<&mut T>> {
		self.dyncast_pinned_mut_::<T>()
	}

	/// # Safety
	///
	/// See [`NonNull::as_ref`].
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_ptr<T: 'static + ?Sized>(this: NonNull<Self>) -> Option<NonNull<T>> {
		Self::dyncast_ptr_::<T>(this)
	}
}

/// Reference downcasting, also to pinned trait objects.
///
/// # ☡ Potential Trip-ups
///
/// ## Dynamic formatting
///
/// Values will be formatted as `dyn Dyncast = !dyn Debug` or `dyn Dyncast = !dyn Display`
/// if they are not *dynamically* [`Debug`] or not *dynamically* [`Display`], respectively.
///
/// Add `#[dyncast(dyn Debug)]` and/or `#[dyncast(dyn Display)]` where you derive or implement this trait.
///
/// ## Partial comparisons
///
/// [`dyn Dyncast`](`Dyncast`) trait object comparisons through [`PartialEq`] and [`PartialOrd`] will always return [`false`] or [`None`] (respectively)
/// unless at least one of the underlying types is *dynamically* [`PartialEq<dyn Dyncast>`] or *dynamically* [`PartialOrd<dyn Dyncast>`], respectively.
///
/// > These implementations should mirror each other if both are available.
///
/// You can write `#[dyncast(impl dyn PartialEq<dyn Dyncast>)]` and `#[dyncast(impl dyn PartialOrd<dyn Dyncast>)]` to
/// generate implementations for these, respectively based on the plain [`PartialEq`] and [`PartialOrd`] implementations.
///
/// Comparisons between distinct types will always result in [`false`] or [`None`] with the generated implementations.
///
/// ## Complete comparisons
///
/// [`Dyncast`] alone never exposes complete comparisons without explicit dyncast.
///
/// However, the following subtraits are available for more completely comparable values:
///
/// - [`DyncastEq`], which is also [`Deref<Target = dyn Dyncast>`](`Deref`), and
/// - [`DyncastOrd`], which is also [`Deref<Target = dyn DyncastEq>`](`Deref`).
///
/// Additionally, [`DynOrd`] is an object-safe version of [`Ord`] and can be generated,
/// conditional on `Self` being [`Ord`] and [`Any`].
///
/// > That's simplified a bit, but close enough.
///
/// [`DyncastEq`] and [`DyncastOrd`] can both be implemented manually.
///
/// A [`DyncastEq`] implementation is generated implicitly iff you write `#[dyncast(impl dyn PartialEq<dyn Dyncast>)]`,
/// conditional on `Self` being [`Eq`].
///
/// A [`DyncastOrd`] implementation is generated implicitly iff you write `#[dyncast(impl dyn PartialOrd<dyn Dyncast>, impl dyn DynOrd)]`,
/// conditional on `Self` being [`DyncastEq`], [`Ord`] and [`Any`].
///
/// ## Hashing
///
/// Meaningful hashing requires that the underlying type be *dynamically* [`DynHash`].
///
/// A blanked implementation is available for types that are [`Hash`],
/// but you still need to enable dyncasts using `#[dyncast(dyn DynHash)]`.
///
/// Types that are not dynamically [`DynHash`] hash dynamically by not hashing anything.
///
/// For convenience, you can enable dyncasts without importing [`DynHash`] by writing `#[dyncast(impl dyn DynHash)]`.
///
/// # ☡ Cognitohazard Warning ☡
///
/// There is Generally Not Neat code here.
///
/// Use the [`Dyncast`](./derive.Dyncast.html) derive macro to implement this and don't worry about it too much.
///
/// I've put in some const assertions and don't rely on unstable behaviour,
/// so this shouldn't fail silently, at least.
///
/// > **Wishlist**
/// >
/// > - `self: NonNull<Self>` as receiver on object-safe `unsafe` trait methods.
/// > - [#81513](https://github.com/rust-lang/rust/issues/81513) or similar.
pub unsafe trait Dyncast {
	#[allow(clippy::type_complexity)]
	#[doc(hidden)]
	fn __dyncast(
		&self,
		this: NonNull<()>,
		target: TypeId,
	) -> Option<MaybeUninit<[u8; mem::size_of::<&dyn Dyncast>()]>>;
}

impl<'a> Debug for dyn 'a + Dyncast {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("dyn Dyncast = ")?;
		#[allow(clippy::option_if_let_else)] // Can't because `f`.
		if let Some(debug) = self.dyncast_::<dyn Debug>() {
			debug.fmt(f)
		} else {
			f.write_str("!dyn Debug")
		}
	}
}

impl Display for dyn Dyncast {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		#[allow(clippy::option_if_let_else)] // Can't because `f`.
		if let Some(display) = self.dyncast::<dyn Display>() {
			display.fmt(f)
		} else {
			f.write_str("dyn Dyncast = !dyn Display")
		}
	}
}

impl PartialEq for dyn Dyncast {
	fn eq(&self, other: &Self) -> bool {
		unsafe {
			if let Some(this) = Self::dyncast_ptr::<dyn PartialEq<Self>>(self.into()) {
				this.as_ref().eq(other)
			} else if let Some(other) = Self::dyncast_ptr::<dyn PartialEq<Self>>(other.into()) {
				other.as_ref().eq(self)
			} else {
				false
			}
		}
	}
}

impl PartialOrd for dyn Dyncast {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		unsafe {
			if let Some(this) = Self::dyncast_ptr::<dyn PartialOrd<Self>>(self.into()) {
				this.as_ref().partial_cmp(other)
			} else if let Some(other) = Self::dyncast_ptr::<dyn PartialOrd<Self>>(other.into()) {
				other.as_ref().partial_cmp(self).map(Ordering::reverse)
			} else {
				None
			}
		}
	}
}

/// Object-safe [`Hash`].
pub trait DynHash {
	fn dyn_hash(&self, state: &mut dyn Hasher);
}
impl<T: ?Sized> DynHash for T
where
	T: Hash,
{
	fn dyn_hash(&self, mut state: &mut dyn Hasher) {
		<Self as Hash>::hash(self, &mut state)
	}
}
impl Hash for dyn DynHash {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.dyn_hash(state)
	}
}

impl Hash for dyn Dyncast {
	fn hash<H: Hasher>(&self, state: &mut H) {
		if let Some(dyn_hash) = self.dyncast::<dyn DynHash>() {
			dyn_hash.dyn_hash(state)
		}
	}
}

/// Object-safe [`Ord`].
///
/// Where possible, prefer [`DyncastOrd`] over manually [`dyncast`](`<dyn Dyncast>::dyncast`)ing to [`dyn DynOrd`].
pub unsafe trait DynOrd {
	fn concrete_type_id(&self) -> TypeId;

	/// # Safety:
	///
	/// `this` must point to the same instance as `self`.
	fn dyn_cmp(&self, other: &dyn DynOrd) -> Ordering;
}

///TODO: This should be an explicitly generated implementation, instead.
unsafe impl<T> DynOrd for T
where
	T: Ord + Any,
{
	fn concrete_type_id(&self) -> TypeId {
		self.type_id()
	}

	fn dyn_cmp(&self, other: &dyn DynOrd) -> Ordering {
		match self.concrete_type_id().cmp(&other.concrete_type_id()) {
			Ordering::Equal => self.cmp(unsafe { &*(other as *const dyn DynOrd).cast::<Self>() }),
			not_equal => not_equal,
		}
	}
}
impl PartialEq<dyn DynOrd> for dyn DynOrd {
	fn eq(&self, other: &Self) -> bool {
		self.dyn_cmp(other) == Ordering::Equal
	}
}
impl PartialOrd<dyn DynOrd> for dyn DynOrd {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.dyn_cmp(other))
	}
}
impl Eq for dyn DynOrd {}
impl Ord for dyn DynOrd {
	fn cmp(&self, other: &Self) -> Ordering {
		self.dyn_cmp(other)
	}
}

mod private {
	use crate::{Dyncast, DyncastEq};

	pub trait Upcast<T: ?Sized> {
		// Based on this technique for object-safe `Self: Sized` defaults by @nbdd0121:
		// <https://rust-lang.zulipchat.com/#narrow/stream/144729-wg-traits/topic/Dyn.20upcasting.20vs.20deref.20coercion/near/254603160>

		fn upcast(&self) -> &T;
		fn upcast_mut(&mut self) -> &mut T;
	}
	impl<T> Upcast<dyn Dyncast> for T
	where
		T: 'static + Dyncast,
	{
		fn upcast(&self) -> &(dyn 'static + Dyncast) {
			self
		}

		fn upcast_mut(&mut self) -> &mut (dyn 'static + Dyncast) {
			self
		}
	}
	impl<T> Upcast<dyn DyncastEq> for T
	where
		T: 'static + DyncastEq,
	{
		fn upcast(&self) -> &(dyn 'static + DyncastEq) {
			self
		}

		fn upcast_mut(&mut self) -> &mut (dyn 'static + DyncastEq) {
			self
		}
	}
}
use private::Upcast;

/// [`Dyncast`] and *dynamically* [`Eq`]
//TODO: Other traits
pub trait DyncastEq: Dyncast + Upcast<dyn Dyncast> {
	fn as_dyncast(&self) -> &(dyn 'static + Dyncast) {
		self.upcast()
	}

	fn as_dyncast_mut(&mut self) -> &mut (dyn 'static + Dyncast) {
		self.upcast_mut()
	}
}
impl Deref for dyn DyncastEq {
	type Target = dyn Dyncast;

	fn deref(&self) -> &Self::Target {
		self.as_dyncast()
	}
}
impl DerefMut for dyn DyncastEq {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.as_dyncast_mut()
	}
}
impl Debug for dyn DyncastEq {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Debug::fmt(self.as_dyncast(), f)
	}
}
impl Display for dyn DyncastEq {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(self.as_dyncast(), f)
	}
}
impl PartialEq for dyn DyncastEq {
	fn eq(&self, other: &Self) -> bool {
		self.dyncast::<dyn PartialEq<Self>>()
			.expect("Expected `Self` to be *dynamically* `dyn PartialEq<dyn Dyncast>` via `dyn DyncastOrd: PartialOrd`")
			.eq(other)
	}
}
impl Eq for dyn DyncastEq {}
impl Hash for dyn DyncastEq {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.as_dyncast().hash(state)
	}
}

/// [`DyncastEq`] and *dynamically* [`Ord`]
//TODO: Other traits
pub trait DyncastOrd: DyncastEq + Upcast<dyn DyncastEq> {
	fn as_dyncast_eq(&self) -> &(dyn 'static + DyncastEq) {
		self.upcast()
	}

	fn as_dyncast_eq_mut(&mut self) -> &mut (dyn 'static + DyncastEq) {
		self.upcast_mut()
	}
}
impl Deref for dyn DyncastOrd {
	type Target = dyn DyncastEq;

	fn deref(&self) -> &Self::Target {
		self.as_dyncast_eq()
	}
}
impl DerefMut for dyn DyncastOrd {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.as_dyncast_eq_mut()
	}
}
impl Debug for dyn DyncastOrd {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Debug::fmt(self.as_dyncast(), f)
	}
}
impl Display for dyn DyncastOrd {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(self.as_dyncast(), f)
	}
}
impl PartialEq for dyn DyncastOrd {
	fn eq(&self, other: &Self) -> bool {
		self.as_dyncast_eq().eq(other.as_dyncast_eq())
	}
}
impl Eq for dyn DyncastOrd {}
impl PartialOrd for dyn DyncastOrd {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.dyncast::<dyn PartialOrd<dyn Dyncast>>()
		.expect("Expected `Self` to be *dynamically* `dyn PartialOrd<dyn Dyncast>` via `dyn DyncastOrd: PartialOrd`")
		.partial_cmp(other.as_dyncast())
	}
}
impl Ord for dyn DyncastOrd {
	fn cmp(&self, other: &Self) -> Ordering {
		self.dyncast::<dyn DynOrd>()
		.expect("Expected `Self` to be *dynamically* `dyn PartialOrd<dyn DynOrd>` via `dyn DyncastOrd: Ord`")
		.dyn_cmp(
			other
				.dyncast::<dyn DynOrd>()
				.expect("Expected `other` to be *dynamically* `dyn PartialOrd<dyn DynOrd>` via `dyn DyncastOrd: Ord`")
		)
	}
}
impl Hash for dyn DyncastOrd {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.as_dyncast().hash(state)
	}
}

#[doc(hidden)]
pub mod __ {
	#[cfg(feature = "macros")]
	pub use static_assertions::const_assert;
}
