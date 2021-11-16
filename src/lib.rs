//! This is a (mostly) trait object **reference** casting and comparison crate.
//!
//! [![Zulip Chat](https://img.shields.io/endpoint?label=chat&url=https%3A%2F%2Fiteration-square-automation.schichler.dev%2F.netlify%2Ffunctions%2Fstream_subscribers_shield%3Fstream%3Dproject%252Ffruit-salad)](https://iteration-square.schichler.dev/#narrow/stream/project.2Ffruit-salad)
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
//! # WIP
//!
//! Due to diminishing returns,
//! I've currently left functionality related to complete comparisons unimplemented.
//!
//! Relevant parts of the documentation are labelled `not that useful yet` and ~~stricken through~~ where unimplemented.
//!
//! # Installation
//!
//! Please use [cargo-edit](https://crates.io/crates/cargo-edit) to always add the latest version of this library:
//!
//! ```cmd
//! cargo add fruit-salad --features macros
//! ```
//!
//! # Features
//!
//! ## `"alloc"`
//!
//! Requires the [`alloc`] crate and enables casting [`Box<dyn Dyncast>`](`alloc::boxed::Box`) into other boxes.
//!
//! ## `"macros"`
//!
//! Makes the [`Dyncast`](derive.Dyncast.html) derive macro and [`implement_dyncasts!`](macro.implement_dyncasts.html) macro available.
//!
//! ## `"std"` (default)
//!
//! Requires the [`std`] crate and implies `"alloc"`.
//!
//! # Example
//!```rust
//! #[cfg(feature = "macros")]
//! {
//! #![allow(clippy::eq_op)] // Identical args are intentional.
//!
//! use core::fmt::Debug;
//! use fruit_salad::Dyncast; // With feature `"macros"`.
//!
//! #[derive(PartialEq, Dyncast, Hash)]
//! #[dyncast(Self, impl dyn DynHash)]
//! struct A;
//!
//! #[derive(Debug, PartialEq, PartialOrd, Dyncast)]
//! #[dyncast(Self, dyn Debug)]
//! #[dyncast(impl dyn PartialEq<dyn Dyncast>, impl dyn PartialOrd<dyn Dyncast>)]
//! struct B;
//!
//! let a: &dyn Dyncast = &A;
//! let b: &dyn Dyncast = &B;
//!
//! assert_ne!(a, a); // Partial equality isn't exposed.
//! assert_eq!(b, b);
//! assert_ne!(a, b);
//! assert_ne!(b, a);
//!
//! assert_eq!(a.partial_cmp(a), None); // Partial order isn't exposed.
//! assert_eq!(b.partial_cmp(b), Some(core::cmp::Ordering::Equal));
//! assert_eq!(a.partial_cmp(b), None);
//! assert_eq!(b.partial_cmp(a), None);
//!
//! assert_eq!(format!("{:?}", a), "dyn Dyncast = !dyn Debug");
//! assert_eq!(format!("{:?}", b), "dyn Dyncast = B");
//!
//! assert!(a.dyncast::<dyn Debug>().is_none());
//! assert!(b.dyncast::<dyn Debug>().is_some());
//!
//! // Also: `…_mut`, `…_pinned`, `…_box` and combinations thereof, as well as `…ptr`.
//! // `…box` methods require the `"alloc"` feature.
//! let _a: &A = a.dyncast().unwrap();
//! let _b: &B = b.dyncast().unwrap();
//! }
//! ```
//!
//! # ☡ Potential Trip-ups
//!
//! While containing a safe API that covers many use-cases, this is still a *runtime* casting crate.
//! Meaning that whether a cast will succeed can't be checked at compile-time for the most part.
//!
//! When you derive [`Dyncast`], you really need to announce each dyncast target type with the `#[dyncast(Type)]` attribute.
//!
//! > It's variadic, stacks and mostly allows repeats, so that's quite flexible.
//! > However, repeat types currently aren't explicitly deduplicated.
//! > The compiler *may* still do so, but keep it in mind.
//! >
//! > The order of dyncast targets may also affect performance,
//! > since the targets are checked against in sequence.
//!
//! You can use `Self` as shorthand for the current type, which also works with generics,
//! but limits the generated [`Dyncast`] implementation by `where Self: 'static`.
//!
//! > Announced dyncast targets must all be `'static`, but the concrete instance doesn't have to be.
//!
//! ## Dynamic formatting
//!
//! Values will be formatted as `dyn Dyncast = !dyn Debug` or `dyn Dyncast = !dyn Display`
//! if they are not *dynamically* [`Debug`] or not *dynamically* [`Display`], respectively.
//!
//! Add `#[dyncast(dyn Debug)]` and/or `#[dyncast(dyn Display)]` where you derive or implement this trait.
//!
//! ## Partial comparisons
//!
//! [`dyn Dyncast`](`Dyncast`) trait object comparisons through [`PartialEq`] and [`PartialOrd`] will always return [`false`] or [`None`] (respectively)
//! unless at least one of the underlying types is *dynamically* [`PartialEq<dyn Dyncast>`] or *dynamically* [`PartialOrd<dyn Dyncast>`], respectively.
//!
//! > These implementations should mirror each other if both are available.
//!
//! You can write `#[dyncast(impl dyn PartialEq<dyn Dyncast>)]` and `#[dyncast(impl dyn PartialOrd<dyn Dyncast>)]` to
//! generate implementations for these, respectively based on the plain [`PartialEq`] and [`PartialOrd`] implementations.
//!
//! Comparisons between distinct types will always result in [`false`] or [`None`] with the generated implementations.
//!
//! ## `not that useful yet` Complete comparisons
//!
//! [`Dyncast`] alone never exposes complete comparisons without explicit dyncast.
//!
//! However, the following subtraits are available for more completely comparable values:
//!
//! - [`DyncastEq`], which is also [`Deref<Target = dyn Dyncast>`](`Deref`), and
//! - [`DyncastOrd`], which is also [`Deref<Target = dyn DyncastEq>`](`Deref`).
//!
//! Additionally, [`DynOrd`] is an object-safe version of [`Ord`] <s>and can be generated,
//! conditional on `Self` being [`Ord`] and [`Any`](`core::any::Any`)</s>.
//!
//! > That's simplified a bit, but close enough.
//!
//! [`DyncastEq`] and [`DyncastOrd`] can both be implemented manually.
//!
//! <s>
//!
//! A [`DyncastEq`] implementation is generated implicitly iff you write `#[dyncast(impl dyn PartialEq<dyn Dyncast>)]`,
//! conditional on `Self` being [`Eq`].
//!
//! A [`DyncastOrd`] implementation is generated implicitly iff you write `#[dyncast(impl dyn PartialOrd<dyn Dyncast>, impl dyn DynOrd)]`,
//! conditional on `Self` being [`DyncastEq`], [`Ord`] and [`Any`](`core::any::Any`).
//!
//! </s>
//!
//! ## Hashing
//!
//! Meaningful hashing requires that the underlying type be *dynamically* [`DynHash`].
//!
//! A blanket implementation is available for types that are [`Hash`],
//! but you still need to enable the dyncast using `#[dyncast(dyn DynHash)]`.
//!
//! Other types (that are not dynamically [`DynHash`]) hash dynamically by not hashing anything.
//!
//! <!-- FIXME: It would be good to emit a warning for types that are hash but not dynamically DynHash, where possible. -->
//!
//! For convenience, you can enable this dyncast without importing [`DynHash`] by writing `#[dyncast(impl dyn DynHash)]`.

#![doc(html_root_url = "https://docs.rs/fruit-salad/0.0.2")]
#![warn(clippy::pedantic, missing_docs)]
#![allow(clippy::semicolon_if_nothing_returned)]
#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

use core::{
	any::TypeId,
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

/// Limited-lifetime API. This could be safe in one or two ways:
///
/// 1. Grabbing the [`TypeId`] of non-`'static` types.
///
///   > This is impossible by design and I mostly agree with that.
///   > It would probably encourage many unsound hacks.
///
/// 2. A way to statically derive a shortened object (not just reference!) lifetime from a `'static` one.
///
///   > This would solve the issue very nicely, but would definitely require at least a very magic core library function.
///   > I hope it will eventually come to the language, nonetheless.
///
/// If either of these possibilities landed, the safe API's coverage could be expanded to (most of) this unsafe one's use cases,
/// and this unsafe API's methods could be deprecated in bulk.
impl<'a> dyn 'a + Dyncast {
	/// # Safety
	///
	/// `TActual` and `TStatic` must be the same type except for lifetimes.
	///
	/// `TActual` must not be longer-lived than `Self`.
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_<TActual: ?Sized, TStatic: 'static + ?Sized>(&self) -> Option<&TActual> {
		self.__dyncast(
			NonNull::new_unchecked(self as *const Self as *mut Self).cast(),
			TypeId::of::<TStatic>(),
		)
		.map(|pointer_data| {
			#[allow(clippy::cast_ptr_alignment)] // Read unaligned.
			pointer_data.as_ptr().cast::<&TActual>().read_unaligned()
		})
	}

	/// # Safety
	///
	/// `TActual` and `TStatic` must be the same type except for lifetimes.
	///
	/// `TActual` must not be longer-lived than `Self`.
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_mut_<TActual: ?Sized, TStatic: 'static + ?Sized>(
		&mut self,
	) -> Option<&mut TActual> {
		let this = NonNull::new_unchecked(self);
		self.__dyncast(this.cast(), TypeId::of::<TStatic>())
			.map(|pointer_data| {
				pointer_data
					.as_ptr()
					.cast::<&mut TActual>()
					.read_unaligned()
			})
	}

	/// # Safety
	///
	/// `TActual` and `TStatic` must be the same type except for lifetimes.
	///
	/// `TActual` must not be longer-lived than `Self`.
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_pinned_<TActual: ?Sized, TStatic: 'static + ?Sized>(
		self: Pin<&Self>,
	) -> Option<Pin<&TActual>> {
		self.__dyncast(
			NonNull::new_unchecked(&*self as *const Self as *mut Self).cast(),
			TypeId::of::<TStatic>(),
		)
		.map(|pointer_data| {
			pointer_data
				.as_ptr()
				.cast::<Pin<&TActual>>()
				.read_unaligned()
		})
	}

	/// # Safety
	///
	/// `TActual` and `TStatic` must be the same type except for lifetimes.
	///
	/// `TActual` must not be longer-lived than `Self`.
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_pinned_mut_<TActual: ?Sized, TStatic: 'static + ?Sized>(
		mut self: Pin<&mut Self>,
	) -> Option<Pin<&mut TActual>> {
		let this = NonNull::new_unchecked(Pin::into_inner_unchecked(self.as_mut()) as *mut Self);
		self.__dyncast(this.cast(), TypeId::of::<TStatic>())
			.map(|pointer_data| {
				pointer_data
					.as_ptr()
					.cast::<Pin<&mut TActual>>()
					.read_unaligned()
			})
	}

	/// # Safety
	///
	/// See [`NonNull::as_ref`].
	///
	/// Additionally, `TActual` and `TStatic` must be the same type except for lifetimes.
	///
	/// `TActual` must not be longer-lived than `Self`.
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_ptr_<TActual: ?Sized, TStatic: 'static + ?Sized>(
		this: NonNull<Self>,
	) -> Option<NonNull<TActual>> {
		this.as_ref()
			.__dyncast(this.cast(), TypeId::of::<TStatic>())
			.map(|pointer_data| {
				pointer_data
					.as_ptr()
					.cast::<NonNull<TActual>>()
					.read_unaligned()
			})
	}

	/// Requires feature `"alloc"`.
	///
	/// # Safety
	///
	/// `TActual` and `TStatic` must be the same type except for lifetimes.
	///
	/// `TActual` must not be longer-lived than `Self`.
	///
	/// # Errors
	///
	/// Iff the cast fails, the original [`Box`](`alloc::boxed::Box`) is restored.
	#[allow(missing_docs)]
	#[cfg(feature = "alloc")]
	pub unsafe fn dyncast_box_<TActual: ?Sized, TStatic: 'static + ?Sized>(
		self: alloc::boxed::Box<Self>,
	) -> Result<alloc::boxed::Box<TActual>, alloc::boxed::Box<Self>> {
		use alloc::boxed::Box;

		let leaked = Box::into_raw(self);

		(&*leaked)
			.__dyncast(
				NonNull::new_unchecked(leaked).cast(),
				TypeId::of::<TStatic>(),
			)
			.map(|pointer_data| {
				#[allow(clippy::cast_ptr_alignment)] // Read unaligned.
				Box::from_raw(
					pointer_data
						.as_ptr()
						.cast::<*mut TActual>()
						.read_unaligned(),
				)
			})
			.ok_or_else(|| Box::from_raw(leaked))

		// Normally there should be a bit of error handling here to prevent leaks in cases where `.__dyncast` panics,
		// but since this crate fully controls that implementation, we can assume that just never happens in a meaningful way.
		//
		// There might still be panics if the caller violates a safety contract somehow, but in that case all bets are off anyway.
	}

	/// Requires feature `"alloc"`.
	///
	/// # Safety
	///
	/// `TActual` and `TStatic` must be the same type except for lifetimes.
	///
	/// `TActual` must not be longer-lived than `Self`.
	///
	/// # Errors
	///
	/// Iff the cast fails, the original [`Box`](`alloc::boxed::Box`) is restored.
	#[allow(missing_docs)]
	#[cfg(feature = "alloc")]
	pub unsafe fn dyncast_pinned_box_<TActual: ?Sized, TStatic: 'static + ?Sized>(
		self: Pin<alloc::boxed::Box<Self>>,
	) -> Result<Pin<alloc::boxed::Box<TActual>>, Pin<alloc::boxed::Box<Self>>> {
		use alloc::boxed::Box;

		let leaked = Box::into_raw(Pin::into_inner_unchecked(self));

		(&*leaked)
			.__dyncast(
				NonNull::new_unchecked(leaked).cast(),
				TypeId::of::<TStatic>(),
			)
			.map(|pointer_data| {
				#[allow(clippy::cast_ptr_alignment)] // Read unaligned.
				Pin::new_unchecked(Box::from_raw(
					pointer_data
						.as_ptr()
						.cast::<*mut TActual>()
						.read_unaligned(),
				))
			})
			.ok_or_else(|| Pin::new_unchecked(Box::from_raw(leaked)))

		// Normally there should be a bit of error handling here to prevent leaks in cases where `.__dyncast` panics,
		// but since this crate fully controls that implementation, we can assume that just never happens in a meaningful way.
		//
		// There might still be panics if the caller violates a safety contract somehow, but in that case all bets are off anyway.
	}
}

/// Safe `'static`-object dyncast API.
///
/// This can't be misused, but it will only work if the targeted instances are owned/`dyn 'static + Dyncast`.
///
/// > It does work on short-lived references to such trait objects, though.
impl dyn Dyncast {
	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast<T: 'static + ?Sized>(&self) -> Option<&T> {
		unsafe { self.dyncast_::<T, T>() }
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_mut<T: 'static + ?Sized>(&mut self) -> Option<&mut T> {
		unsafe { self.dyncast_mut_::<T, T>() }
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned<T: 'static + ?Sized>(self: Pin<&Self>) -> Option<Pin<&T>> {
		unsafe { self.dyncast_pinned_::<T, T>() }
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned_mut<T: 'static + ?Sized>(self: Pin<&mut Self>) -> Option<Pin<&mut T>> {
		unsafe { self.dyncast_pinned_mut_::<T, T>() }
	}

	/// # Safety
	///
	/// See [`NonNull::as_ref`].
	#[allow(missing_docs)]
	#[must_use]
	pub unsafe fn dyncast_ptr<T: 'static + ?Sized>(this: NonNull<Self>) -> Option<NonNull<T>> {
		Self::dyncast_ptr_::<T, T>(this)
	}

	/// Requires feature `"alloc"`.
	///
	/// # Errors
	///
	/// Iff the cast fails, the original [`Box`](`alloc::boxed::Box`) is restored.
	#[allow(missing_docs)]
	#[cfg(feature = "alloc")]
	pub fn dyncast_box<T: 'static + ?Sized>(
		self: alloc::boxed::Box<Self>,
	) -> Result<alloc::boxed::Box<T>, alloc::boxed::Box<Self>> {
		unsafe { self.dyncast_box_::<T, T>() }
	}

	/// Requires feature `"alloc"`.
	///
	/// # Errors
	///
	/// Iff the cast fails, the original [`Box`](`alloc::boxed::Box`) is restored.
	#[allow(missing_docs)]
	#[cfg(feature = "alloc")]
	pub fn dyncast_pinned_box<T: 'static + ?Sized>(
		self: Pin<alloc::boxed::Box<Self>>,
	) -> Result<Pin<alloc::boxed::Box<T>>, Pin<alloc::boxed::Box<Self>>> {
		unsafe { self.dyncast_pinned_box_::<T, T>() }
	}
}

/// Reference downcasting, also to pinned trait objects.
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
/// > - `self: NonNull<Self>` as receiver in object-safe `unsafe` trait methods.
/// > - [#81513](https://github.com/rust-lang/rust/issues/81513) or similar.
pub unsafe trait Dyncast {
	/// This likely warrants a bit of an explanation,
	/// even though it's really not part of the public API,
	/// and the main purpose of this crate is to *stop* API consumers from
	/// wasting their time with navigating this mess on their end/in their code.
	///
	/// I'll put in here for the curious, everyone else will just see the trait docs only.
	#[doc(hidden)]
	///
	/// In short, what you're looking at is a sort of focal point of some shortcomings
	/// that Rust currently has with regards to dynamic dispatch, as there are many
	/// limitations to object-safety that aren't necessarily technical in nature.
	///
	/// (I'm not saying they really NEED to be fixed with a high priority though,
	/// since for the most part each workaround is simple enough by itself and
	/// shouldn't produce all too much overhead.)
	///
	/// First off, this method uses pointers because each of the six variations
	/// of reference used (shared, mutable, pinned shared, pinned mutable,
	/// [`NonNull`] and [`Box`](`alloc::boxed::Box`)) requires the exact same operations.
	/// By only using this one method, there may be a significant reduction in text size if
	/// many concrete types are dyncast-enabled.
	///
	/// > [`Box`](`alloc::boxed::Box`)es aren't punned directly of course,
	/// but converted using their API functions in the relevant shim.
	///
	/// In any case, pointers are unfortunately not valid receivers,
	/// which is one reason there is a `this` parameters here.
	///
	/// The other reason is that even object-safety is quite picky about `Self`
	/// appearing in arguments. `this` must be the raw address `NonNull<()>`
	/// due to that limitation. Hopefully both will change in time.
	///
	/// The `target` parameter is provided as `TypeId` because a generic
	/// type argument would make the method not object-safe.
	///
	/// Similarly, it's unknown which memory layout the resulting pointer will have.
	/// The size of `&dyn Dyncast` is assumed to be enough to store it, but this is also
	/// validated statically in the generated implementation(s) of this method.
	///
	/// The return memory (interpreted as pointer) is also considered to be unaligned for its contents
	/// (while the eventual pointee instance must be aligned), so unaligned writes and reads are used for it.
	/// There may be a better method to do this. If you know one,
	/// please don't hesitate to contact me/the project about it!
	///
	/// # Implementation
	///
	/// This method's implementations are generated is as long if-else chains, each
	/// with one branch for each dyncast target plus one default branch returning [`None`].
	///
	/// The [`TypeId`] given as `target` is compared with ones baked into the function,
	/// and if there's a match, the pointer is converted to the target type (widening it if
	/// necessary) and then returned not-further-modified within the allotted memory.
	#[allow(clippy::type_complexity)]
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
		if let Some(debug) = unsafe { self.dyncast_::<dyn 'a + Debug, dyn Debug>() } {
			debug.fmt(f)
		} else {
			f.write_str("!dyn Debug")
		}
	}
}

impl<'a> Display for dyn 'a + Dyncast {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		#[allow(clippy::option_if_let_else)] // Can't because `f`.
		if let Some(display) = unsafe { self.dyncast_::<dyn 'a + Display, dyn Display>() } {
			display.fmt(f)
		} else {
			f.write_str("dyn Dyncast = !dyn Display")
		}
	}
}

impl<'a> PartialEq for dyn 'a + Dyncast {
	fn eq(&self, other: &Self) -> bool {
		unsafe {
			if let Some(this) =
				Self::dyncast_::<dyn 'a + PartialEq<Self>, dyn PartialEq<dyn Dyncast>>(self)
			{
				this.eq(other)
			} else if let Some(other) =
				Self::dyncast_::<dyn 'a + PartialEq<Self>, dyn PartialEq<dyn Dyncast>>(other)
			{
				other.eq(self)
			} else {
				false
			}
		}
	}
}

impl<'a> PartialOrd for dyn 'a + Dyncast {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		unsafe {
			if let Some(this) =
				Self::dyncast_::<dyn 'a + PartialOrd<Self>, dyn PartialOrd<dyn Dyncast>>(self)
			{
				this.partial_cmp(other)
			} else if let Some(other) =
				Self::dyncast_::<dyn 'a + PartialOrd<Self>, dyn PartialOrd<dyn Dyncast>>(other)
			{
				other.partial_cmp(self).map(Ordering::reverse)
			} else {
				None
			}
		}
	}
}

/// Object-safe [`Hash`].
pub trait DynHash {
	/// Hashes this instance.
	/// See [`Hash::hash`].
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

impl<'a> Hash for dyn 'a + Dyncast {
	fn hash<H: Hasher>(&self, state: &mut H) {
		if let Some(dyn_hash) = unsafe { self.dyncast_::<dyn 'a + DynHash, dyn DynHash>() } {
			dyn_hash.dyn_hash(state)
		}
	}
}

/// `not that useful yet` Object-safe [`Ord`].
///
/// Where possible, prefer [`DyncastOrd`] over manually [`dyncast`](trait.Dyncast.html#method.dyncast)ing to [`dyn DynOrd`].
pub unsafe trait DynOrd {
	/// Retrieves the [`TypeId`] of the underlying instance.
	fn concrete_type_id(&self) -> TypeId;

	/// Dynamically compares `self` to `other`.
	fn dyn_cmp(&self, other: &dyn DynOrd) -> Ordering;
}

///TODO: This should be an explicitly generated implementation, instead.
// unsafe impl<T> DynOrd for T
// where
// 	T: Ord + Any,
// {
// 	fn concrete_type_id(&self) -> TypeId {
// 		self.type_id()
// 	}

// 	fn dyn_cmp(&self, other: &dyn DynOrd) -> Ordering {
// 		match self.concrete_type_id().cmp(&other.concrete_type_id()) {
// 			Ordering::Equal => self.cmp(unsafe { &*(other as *const dyn DynOrd).cast::<Self>() }),
// 			not_equal => not_equal,
// 		}
// 	}
// }
impl<'a> PartialEq<dyn 'a + DynOrd> for dyn 'a + DynOrd {
	fn eq(&self, other: &Self) -> bool {
		self.dyn_cmp(other) == Ordering::Equal
	}
}
impl<'a> PartialOrd<dyn 'a + DynOrd> for dyn 'a + DynOrd {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.dyn_cmp(other))
	}
}
impl<'a> Eq for dyn 'a + DynOrd {}
impl<'a> Ord for dyn 'a + DynOrd {
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
	impl<'a, T> Upcast<dyn 'a + Dyncast> for T
	where
		T: 'a + Dyncast,
	{
		fn upcast(&self) -> &(dyn 'a + Dyncast) {
			self
		}

		fn upcast_mut(&mut self) -> &mut (dyn 'a + Dyncast) {
			self
		}
	}
	impl<'a, T> Upcast<dyn 'a + DyncastEq> for T
	where
		T: 'a + DyncastEq,
	{
		fn upcast(&self) -> &(dyn 'a + DyncastEq) {
			self
		}

		fn upcast_mut(&mut self) -> &mut (dyn 'a + DyncastEq) {
			self
		}
	}
}
use private::Upcast;

/// `not that useful yet` [`Dyncast`] and *dynamically* [`Eq`]
//TODO: Other traits
pub trait DyncastEq: Dyncast + for<'a> Upcast<dyn 'a + Dyncast> {
	/// Upcasts this instance to [`Dyncast`].
	fn as_dyncast<'a>(&self) -> &(dyn 'a + Dyncast)
	where
		Self: 'a,
	{
		self.upcast()
	}

	/// Mutably upcasts this instance to [`Dyncast`].
	fn as_dyncast_mut<'a>(&mut self) -> &mut (dyn 'a + Dyncast)
	where
		Self: 'a,
	{
		self.upcast_mut()
	}
}
impl<'a> Deref for dyn 'a + DyncastEq {
	type Target = dyn 'a + Dyncast;

	fn deref(&self) -> &Self::Target {
		self.as_dyncast()
	}
}
impl<'a> DerefMut for dyn 'a + DyncastEq {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.as_dyncast_mut()
	}
}
impl<'a> Debug for dyn 'a + DyncastEq {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Debug::fmt(self.as_dyncast(), f)
	}
}
impl<'a> Display for dyn 'a + DyncastEq {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(self.as_dyncast(), f)
	}
}
impl<'a> PartialEq for dyn 'a + DyncastEq {
	fn eq(&self, other: &Self) -> bool {
		unsafe{self.dyncast_::<dyn 'a + PartialEq<dyn 'a + Dyncast>, dyn PartialEq<dyn Dyncast>>()}
			.expect("Expected `Self` to be *dynamically* `dyn PartialEq<dyn Dyncast>` via `dyn DyncastOrd: PartialOrd`")
			.eq(other.as_dyncast())
	}
}
impl<'a> Eq for dyn 'a + DyncastEq {}
impl<'a> Hash for dyn 'a + DyncastEq {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.as_dyncast().hash(state)
	}
}

/// `not that useful yet` [`DyncastEq`] and *dynamically* [`Ord`]
//TODO: Other traits
pub trait DyncastOrd: DyncastEq + for<'a> Upcast<dyn 'a + DyncastEq> {
	/// Upcasts this instance to [`DyncastEq`].
	fn as_dyncast_eq<'a>(&self) -> &(dyn 'a + DyncastEq)
	where
		Self: 'a,
	{
		self.upcast()
	}

	/// Mutably upcasts this instance to [`DyncastEq`].
	fn as_dyncast_eq_mut<'a>(&mut self) -> &mut (dyn 'a + DyncastEq)
	where
		Self: 'a,
	{
		self.upcast_mut()
	}
}
impl<'a> Deref for dyn 'a + DyncastOrd {
	type Target = dyn 'a + DyncastEq;

	fn deref(&self) -> &Self::Target {
		self.as_dyncast_eq()
	}
}
impl<'a> DerefMut for dyn 'a + DyncastOrd {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.as_dyncast_eq_mut()
	}
}
impl<'a> Debug for dyn 'a + DyncastOrd {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Debug::fmt(self.as_dyncast(), f)
	}
}
impl<'a> Display for dyn 'a + DyncastOrd {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(self.as_dyncast(), f)
	}
}
impl<'a> PartialEq for dyn 'a + DyncastOrd {
	fn eq(&self, other: &Self) -> bool {
		self.as_dyncast_eq().eq(other.as_dyncast_eq())
	}
}
impl<'a> Eq for dyn 'a + DyncastOrd {}
impl<'a> PartialOrd for dyn 'a + DyncastOrd {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		unsafe{self.dyncast_::<dyn 'a+PartialOrd<dyn 'a+Dyncast>,dyn PartialOrd<dyn Dyncast>>()}
		.expect("Expected `Self` to be *dynamically* `dyn PartialOrd<dyn Dyncast>` via `dyn DyncastOrd: PartialOrd`")
		.partial_cmp(other.as_dyncast())
	}
}
impl<'a> Ord for dyn 'a + DyncastOrd {
	fn cmp(&self, other: &Self) -> Ordering {
		unsafe{self.dyncast_::<dyn 'a+DynOrd, dyn DynOrd>()}
		.expect("Expected `Self` to be *dynamically* `dyn PartialOrd<dyn DynOrd>` via `dyn DyncastOrd: Ord`")
		.dyn_cmp(
			unsafe{other
				.dyncast_::<dyn 'a+DynOrd,dyn DynOrd>()}
				.expect("Expected `other` to be *dynamically* `dyn PartialOrd<dyn DynOrd>` via `dyn DyncastOrd: Ord`")
		)
	}
}
impl<'a> Hash for dyn 'a + DyncastOrd {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.as_dyncast().hash(state)
	}
}

#[doc(hidden)]
pub mod __ {
	#[cfg(feature = "macros")]
	pub use static_assertions::const_assert;
}
