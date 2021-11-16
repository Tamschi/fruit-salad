#![doc(html_root_url = "https://docs.rs/fruit-salad_proc-macro-definitions/0.0.2")]
#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![allow(clippy::semicolon_if_nothing_returned, clippy::too_many_lines)]

extern crate proc_macro;

use call2_for_syn::call2_strict;
use debugless_unwrap::DebuglessUnwrap;
use lazy_static::lazy_static;
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{quote, quote_spanned, ToTokens};
use std::ops::Deref;
use syn::{
	parenthesized,
	parse::{Parse, ParseStream},
	parse_macro_input,
	punctuated::Punctuated,
	spanned::Spanned,
	visit_mut::{self, VisitMut},
	Attribute, Error, Generics, Ident, Item, Lifetime, PredicateType, Result, Token, Type,
	TypeParamBound, WhereClause, WherePredicate,
};
use tap::Pipe;
use unquote::unquote;

macro_rules! tokens_eq {
	($input:expr, $($tt:tt)*) => {
		(call2_strict($input.clone(), |input| {
			unquote!(input, $($tt)*);
			Ok(())
		})
		.ok()
		.transpose()
		.ok()
		.flatten()
		.is_some())
	};
}

/// Implements `Dyncast` for an enum, struct, trait, trait alias, type alias or union.  
/// Requires feature `"macros"`.
///
/// The implementation is limited to targets that are `'static`,
/// and targeting `Self` is explicit.
///
/// No target is available by default.
///
/// # Specifying Targets
///
/// To specify a target, add a `#[dyncast(Target)]` attribute,
/// where `Target` is an in-scope `'static` type or type path.
///
/// You can specify a target multiple times (except token literal `impl` targets),
/// specify multiple targets in one `#[dyncast(…)]` attribute by separating them with commas and
/// stack multiple `#[dyncast(…)]` attributes on the same derive target.
///
/// > There is no explicit deduplication of targets,
/// > but the compiler may still simplify them as duplicates appear as shadowed `else if` branches.
///
/// To specify non-trait types other than literally `Self`, you must prefix them with `unsafe`:
///
/// ```compile_fail
/// use fruit_salad::Dyncast;
///
/// #[derive(Dyncast)]
/// #[dyncast(A)]
/// struct A;
/// ```
///
/// ```rust
/// use fruit_salad::Dyncast;
///
/// #[derive(Dyncast)]
/// #[dyncast(unsafe A)]
/// struct A;
/// ```
///
/// ```compile_fail
/// #![deny(unsafe_code)]
///
/// use fruit_salad::Dyncast;
///
/// #[derive(Dyncast)]
/// #[dyncast(unsafe A)]
/// struct A;
/// ```
///
/// The targeted type must be layout-compatible,
/// exactly like when using [`mem::transmute`](`core::mem::transmute`).
///
/// > Conversely, misusing this feature (by for example targeting a type that's not layout-compatible)
/// > is considered out of scope and may cease to compile at any time, if it doesn't fail to do so already.
/// >
/// > (It may also begin to compile without notice. Be careful.)
///
/// ## `impl` targets
///
/// In addition to the plain type targets above, the following token literal targets are available:
///
/// - `impl dyn DynHash`
///
///   Conveniently targets [`DynHash`](trait.DynHash.html) without that being in scope.
///
///   Requires a [`DynHash`](trait.DynHash.html) implementation.
///
///   > [`DynHash`](trait.DynHash.html) is already blanket-implemented for all types that are [`Hash`].
///
/// - `impl dyn PartialEq<dyn Dyncast>`
///
///   Implements and targets dynamic partial equality,
///   so that distinct underlying types are treated as distinct.
///
///   Requires a [`PartialEq<Self>`] implementation on `Self` and that `Self` is a dyncast target.
///
/// - `impl dyn PartialOrd<dyn Dyncast>`
///
///   Implements and targets dynamic partial order,
///   so that distinct underlying types are treated as incomparable.
///
///   Requires a [`PartialOrd<Self>`] implementation on `Self` and that `Self` is a dyncast target.
///
/// **If these targets are unavailable, `dyn Dyncast` trait objects made from instances of
/// this type will not hash properly and always return [`false`] or [`None`] from comparisons,
/// all respectively!**
///
/// # Example
///
/// ```rust
/// use fruit_salad::Dyncast; // Import macro and trait.
///
/// #[derive(Debug, PartialEq, PartialOrd, Hash, Dyncast)]
/// #[dyncast(Self)] // Works like `Any`.
/// #[dyncast(dyn core::fmt::Debug)] // Must be in scope or qualified.
/// #[dyncast(impl dyn DynHash)] // As `Hash` isn't object-safe.
///
/// // Allow dynamic comparisons to succeed:
/// #[dyncast(impl dyn PartialEq<dyn Dyncast>, impl dyn PartialOrd<dyn Dyncast>)]
/// struct MyStruct;
/// ```
#[proc_macro_derive(Dyncast, attributes(dyncast, dyncast))]
pub fn dyncast_derive(input: TokenStream1) -> TokenStream1 {
	let fruit_salad = fruit_salad_ident(Span::mixed_site());
	let derive_target = parse_macro_input!(input as DeriveTarget);
	implement_dyncast(&derive_target, &fruit_salad, true).into()
}

fn implement_dyncast(
	impl_target: &ImplTarget,
	fruit_salad: &Ident,
	ignore_unrelated_attributes: bool,
) -> TokenStream2 {
	#![allow(clippy::items_after_statements)]

	let ImplTarget {
		attributes,
		dyn_,
		ident,
		generics,
	} = impl_target;

	let mut attribute_errors = if ignore_unrelated_attributes {
		vec![]
	} else {
		attributes
			.iter()
			.map(|attribute| {
				Error::new_spanned(attribute, "Attributes are not allowed here.").to_compile_error()
			})
			.collect()
	};

	let has_self_generics = !generics.params.is_empty();

	let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

	let where_clause = {
		let mut where_clause = where_clause.cloned().unwrap_or_else(|| WhereClause {
			where_token: Token![where](Span::mixed_site()),
			predicates: Punctuated::default(),
		});
		let predicates = &mut where_clause.predicates;
		predicates.push(WherePredicate::Type(PredicateType {
			lifetimes: None,
			bounded_ty: Type::Verbatim(Token![Self](Span::mixed_site()).into_token_stream()),
			colon_token: Token![:](Span::mixed_site()),
			bounds: {
				let mut bounds = Punctuated::new();
				bounds.push_value(TypeParamBound::Lifetime(Lifetime {
					apostrophe: Span::mixed_site(),
					ident: Ident::new("static", Span::mixed_site()),
				}));
				bounds
			},
		}));
		Some(where_clause)
	};

	let mut extra_impls: Vec<ExtraImplementation> = vec![];
	let mut require_self_downcast: Vec<Span> = vec![];
	let mut has_self_downcast: bool = false;

	let target_branches = attributes
		.iter()
		.filter(|attribute| {
			attribute.path.is_ident("dyncast")
				|| attribute.path.leading_colon.is_some()
					&& attribute.path.segments.len() == 2
					&& attribute.path.segments[0].ident == "fruit_salad"
					&& attribute.path.segments[1].ident == "dyncast"
		})
		.map(|attribute| {
			for segment in &attribute.path.segments {
				if !segment.arguments.is_empty() {
					attribute_errors.push(
						Error::new_spanned(&segment.arguments, "Unexpected path arguments.")
							.into_compile_error(),
					)
				}
			}

			//FIXME: Should accept comma-separated types.
			call2_strict(attribute.tokens.clone(), |input| {
				let contents;
				parenthesized!(contents in input);
				let targets = Punctuated::<DyncastTarget, Token![,]>::parse_terminated(&contents)?;
				Ok(targets)
			}).debugless_unwrap(/*FIXME: Fail better! */)
		})
		.collect::<Result<Vec<_>>>()
		.unwrap(/*FIXME: Fail better! */)
		.into_iter()
		.flatten()
		.map(|dyncast_target| {

			if tokens_eq!(dyncast_target.type_.to_token_stream(), Self) {
				has_self_downcast = true;
			}

			let unsafe_ = dyncast_target.unsafe_
				.unwrap_or_else(|| Token![unsafe](
					dyncast_target.type_.span().resolved_at(Span::mixed_site())
				));

			let diagnostics = dyncast_target.diagnostics();

			let type_ = implement_dyncast_target(dyncast_target, &mut extra_impls, &mut require_self_downcast);

			let mut assertion_type = type_.clone();
			let mut contains_self = false;
			{
				struct SelfReplacer<'a> {
					replacement: Type,
					contains_self: &'a mut bool,
				}
				impl VisitMut for SelfReplacer<'_> {
					fn visit_type_mut(&mut self, t: &mut Type) {
						if is_self_type(t) {
							*self.contains_self = true;
							*t = self.replacement.clone()
						} else {
							visit_mut::visit_type_mut(self, t)
						}
					}
				}

				SelfReplacer {
					contains_self: &mut contains_self,
					replacement: call2_strict(
						quote_spanned!(Span::mixed_site()=> #dyn_ #ident#type_generics),
						Type::parse,
					)
					.debugless_unwrap()
					.unwrap(),
				}
				.visit_type_mut(&mut assertion_type);
			}

			let pointer_size_assertion = if contains_self && has_self_generics {
				// Generic type parameters of `Self` cannot be used in the assertion.
				quote_spanned! {type_.span().resolved_at(Span::mixed_site())=>
					::#fruit_salad::__::const_assert!(::core::mem::size_of::<*mut ()>() <= ::core::mem::size_of::<&dyn Dyncast>());
				}
			} else {
				quote_spanned! {type_.span().resolved_at(Span::mixed_site())=>
					::#fruit_salad::__::const_assert!(::core::mem::size_of::<*mut #assertion_type>() <= ::core::mem::size_of::<&dyn Dyncast>());
				}
			};

			let conversion = quote_spanned! {type_.span().resolved_at(Span::mixed_site())=>
				let mut result_memory = ::core::mem::MaybeUninit::<[u8; ::core::mem::size_of::<&dyn Dyncast>()]>::uninit();
					result_memory
						.as_mut_ptr()
						.cast::<::core::ptr::NonNull<#type_>>()
						.write_unaligned(::core::ptr::NonNull::<#type_>::new_unchecked(
							this.cast::<Self>().as_ptr() as *mut #type_
						)
					);
					result_memory
			};

			let unsafe_block = quote_spanned! {unsafe_.span()=>
				#unsafe_ {
					#conversion
				}
			};


			let branch = quote_spanned! {type_.span().resolved_at(Span::mixed_site())=> if target == ::std::any::TypeId::of::<#type_>() {
				#diagnostics
				#pointer_size_assertion
				::core::option::Option::Some(#unsafe_block)
			} else};
			branch
		}).collect::<Vec<_>>();

	let extra_impls = extra_impls
		.into_iter()
		.map(|ExtraImplementation { unsafe_, what, how }| {
			quote_spanned! {what.span()=>
			#unsafe_ impl#impl_generics #what for #dyn_ #ident#type_generics
				#where_clause
				{
					#how
				}
			}
		});

	if has_self_downcast {
		require_self_downcast.clear()
	}
	let require_self_downcast = require_self_downcast
		.into_iter()
		.map(|span| quote_spanned!(span=> ::core::compile_error!("`Self`-dyncast required by this. Add `#[dyncast(Self)]`");));

	quote_spanned! {Span::mixed_site()=>
		#(#attribute_errors)*

		/// # Targets
		///
		#(#[doc = concat!("- `", stringify!(#target_branches), "`")])*
		unsafe impl#impl_generics ::#fruit_salad::Dyncast for #dyn_ #ident#type_generics
			#where_clause
		{
			fn __dyncast(
				&self,
				this: ::core::ptr::NonNull<()>,
				target: ::std::any::TypeId,
			) -> ::core::option::Option<
				::core::mem::MaybeUninit<
					[::core::primitive::u8; ::core::mem::size_of::<&dyn ::#fruit_salad::Dyncast>()]
				>
			> {
				#(#target_branches)* {
					None
				}
			}
		}

		#(#extra_impls)*
		#(#require_self_downcast)*
	}
}

struct ExtraImplementation {
	unsafe_: Option<Token![unsafe]>,
	what: Type,
	how: TokenStream2,
}

fn implement_dyncast_target(
	dyncast_target: DyncastTarget,
	extra_impls: &mut Vec<ExtraImplementation>,
	require_self_downcast: &mut Vec<Span>,
) -> Type {
	let impl_ = if let Some(impl_) = dyncast_target.impl_ {
		impl_
	} else {
		return dyncast_target.type_;
	};

	let type_ = dyncast_target.type_.to_token_stream();

	let fruit_salad = fruit_salad_ident(Span::mixed_site());

	if tokens_eq!(type_, dyn DynHash) {
		// This is just a shorthand to avoid the import (as the trait is blanket-implemented),
		// so there's nothing further done here.
		call2_strict(
			quote_spanned! {type_.span()=>
				dyn ::#fruit_salad::DynHash
			},
			Type::parse,
		)
		.unwrap()
		.unwrap()
	} else if tokens_eq!(type_, dyn PartialEq<dyn Dyncast>) {
		extra_impls.push(ExtraImplementation {
			unsafe_: None,
			what: call2_strict(
				quote_spanned! {impl_.span=>
					::core::cmp::PartialEq::<dyn ::#fruit_salad::Dyncast>
				},
				Type::parse,
			)
			.unwrap()
			.unwrap(),
			how: quote_spanned! {impl_.span=>
				fn eq(&self, other: &(dyn 'static + ::#fruit_salad::Dyncast)) -> bool {
					if let Some(other) = other.dyncast::<Self>() {
						::core::cmp::PartialEq::<Self>::eq(self, other)
					} else {
						false
					}
				}
			},
		});
		require_self_downcast.push(impl_.span);
		call2_strict(
			quote_spanned! {type_.span()=>
				dyn ::core::cmp::PartialEq::<dyn ::#fruit_salad::Dyncast>
			},
			Type::parse,
		)
		.unwrap()
		.unwrap()
	} else if tokens_eq!(type_, dyn PartialOrd<dyn Dyncast>) {
		extra_impls.push(ExtraImplementation {
			unsafe_: None,
			what: call2_strict(
				quote_spanned! {impl_.span=>
					::core::cmp::PartialOrd::<dyn ::#fruit_salad::Dyncast>
				},
				Type::parse,
			)
			.unwrap()
			.unwrap(),
			how: quote_spanned! {impl_.span=>
				fn partial_cmp(
					&self,
					other: &(dyn 'static + ::#fruit_salad::Dyncast)
				) -> ::core::option::Option<::core::cmp::Ordering> {
					if let Some(other) = other.dyncast::<Self>() {
						::core::cmp::PartialOrd::<Self>::partial_cmp(self, other)
					} else {
						None
					}
				}
			},
		});
		require_self_downcast.push(impl_.span);
		call2_strict(
			quote_spanned! {type_.span()=>
				dyn ::core::cmp::PartialOrd::<dyn ::#fruit_salad::Dyncast>
			},
			Type::parse,
		)
		.unwrap()
		.unwrap()
	} else {
		Error::new_spanned(
			&type_,
			format_args!(
				"Unknown type for generated implementation: `{}`
Expected one of:
    - `dyn PartialEq<dyn Dyncast>`",
				type_
			),
		)
		.pipe(|error| {
			extra_impls.push(ExtraImplementation {
				unsafe_: dyncast_target.unsafe_,
				what: dyncast_target.type_.clone(),
				how: error.into_compile_error(),
			})
		});
		dyncast_target.type_
	}
}

fn is_self_type(t: &Type) -> bool {
	matches!(t, Type::Path(tp) if tp.qself.is_none() && tp.path.is_ident("Self"))
}

struct DyncastTarget {
	unsafe_: Option<Token![unsafe]>,
	impl_: Option<Token![impl]>,
	type_: Type,
}
impl Parse for DyncastTarget {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(Self {
			unsafe_: input.parse().unwrap(),
			impl_: input.parse().unwrap(),
			type_: input.parse()?,
		})
	}
}
impl DyncastTarget {
	fn diagnostics(&self) -> impl 'static + ToTokens {
		if self.unsafe_.is_none()
			&& !matches!(&self.type_, Type::TraitObject(_))
			&& !is_self_type(&self.type_)
		{
			Error::new_spanned(&self.type_, "This cast requires an `unsafe` prefix.")
				.into_compile_error()
		} else if self.unsafe_.is_some()
			&& (matches!(&self.type_, Type::TraitObject(_)) || is_self_type(&self.type_))
		{
			// Causes an "unnecessary unsafe block" style warning.
			let unsafe_ = self.unsafe_.as_ref().unwrap();
			quote_spanned!(unsafe_.span=> #unsafe_ {})
		} else {
			return None;
		}
		.pipe(Some)
	}
}

/// Implements `Dyncast` for an enum, struct, trait, trail alias, type alias or union.  
/// Requires feature `"macros"`.
///
/// The implementation is limited to targets that are `'static`,
/// and targeting `Self` is explicit.
///
/// No target is available by default.
///
/// # Syntax
///
/// `dyn Ident<Generics: Constraints> where Where: Clause`, `,`-separated.
///
/// - `dyn` is optional,
/// - generics are optional,
/// - a where clause is optional.
///
/// Add dyncast targets via `#[dyncast(…)]` attributes on each parameter, like with the [`Dyncast`] derive macro.
#[proc_macro]
pub fn implement_dyncasts(input: TokenStream1) -> TokenStream1 {
	let fruit_salad = fruit_salad_ident(Span::mixed_site());
	let mut output = TokenStream2::new();
	for impl_target in parse_macro_input!(input as ImplTargets) {
		output.extend(implement_dyncast(&impl_target, &fruit_salad, false));
	}
	output.into()
}

struct ImplTarget {
	attributes: Vec<Attribute>,
	dyn_: Option<Token![dyn]>,
	ident: Ident,
	generics: Generics,
}

struct DeriveTarget(ImplTarget);
impl Deref for DeriveTarget {
	type Target = ImplTarget;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl Parse for DeriveTarget {
	fn parse(input: ParseStream) -> Result<Self> {
		let item: Item = input.parse()?;
		match item {
			Item::Enum(enum_) => ImplTarget {
				attributes: enum_.attrs,
				dyn_: None,
				ident: enum_.ident,
				generics: enum_.generics,
			},
			Item::Struct(struct_) => ImplTarget {
				attributes: struct_.attrs,
				dyn_: None,
				ident: struct_.ident,
				generics: struct_.generics,
			},
			Item::Trait(trait_) => ImplTarget {
				attributes: trait_.attrs,
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_.ident,
				generics: trait_.generics,
			},
			Item::TraitAlias(trait_alias) => ImplTarget {
				attributes: trait_alias.attrs,
				dyn_: Some(Token![dyn](Span::mixed_site())),
				ident: trait_alias.ident,
				generics: trait_alias.generics,
			},
			Item::Type(type_) => ImplTarget {
				attributes: type_.attrs,
				dyn_: None,
				ident: type_.ident,
				generics: type_.generics,
			},
			Item::Union(union) => ImplTarget {
				attributes: union.attrs,
				dyn_: None,
				ident: union.ident,
				generics: union.generics,
			},
			_ => return Err(Error::new(
				Span::mixed_site(),
				"`Dyncast` can only be derived for structs, enums, unions, traits, trait aliases and type aliases.",
			)),
		}.pipe(Self)
		.pipe(Ok)
	}
}

struct ImplTargets(Vec<ImplTarget>);
impl IntoIterator for ImplTargets {
	type Item = ImplTarget;

	type IntoIter = <Vec<ImplTarget> as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter()
	}
}

impl Parse for ImplTargets {
	fn parse(input: ParseStream) -> Result<Self> {
		Punctuated::<ImplTarget, Token![,]>::parse_terminated(input)?
			.into_iter()
			.collect::<Vec<_>>()
			.pipe(Self)
			.pipe(Ok)
	}
}

//FIXME: This one should also accept type paths.
impl Parse for ImplTarget {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(Self {
			attributes: Attribute::parse_outer(input)?,
			dyn_: input.parse()?,
			ident: input.parse()?,
			generics: {
				let mut generics: Generics = input.parse()?;
				generics.where_clause = input.parse()?;
				generics
			},
		})
	}
}

lazy_static! {
	static ref FRUIT_SALAD_NAME: String = match crate_name("fruit-salad") {
		Ok(FoundCrate::Name(name)) => name,
		Ok(FoundCrate::Itself) | Err(_) => "fruit_salad".to_owned(),
	};
}
fn fruit_salad_ident(span: Span) -> Ident {
	Ident::new(&*FRUIT_SALAD_NAME, span.resolved_at(Span::mixed_site()))
}
