#![doc(html_root_url = "https://docs.rs/fruit-salad_proc-macro-definitions/0.0.1")]
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
use quote::{quote_spanned, ToTokens};
use std::ops::Deref;
use syn::{
	parenthesized,
	parse::{Parse, ParseStream},
	parse_macro_input,
	punctuated::Punctuated,
	visit_mut::{self, VisitMut},
	Attribute, Error, Generics, Ident, Item, Lifetime, PredicateType, Result, Token, Type,
	TypeParamBound, WhereClause, WherePredicate,
};
use tap::Pipe;

/// Implements `Dyncast` for an enum, struct, trait, trait alias, type alias or union.
///
/// The implementation is limited to targets that are `'static`,
/// and targeting `Self` is explicit.
///
/// No target is available by default.
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

	let (target_errors, mut targets): (Vec<_>, Vec<Type>) = attributes
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
		.map(|dyncast_target| (dyncast_target.diagnostics(), dyncast_target.type_))
		.unzip();

	// This is required for the const assertion.
	for target in &mut targets {
		struct SelfReplacer {
			replacement: Type,
		}
		impl VisitMut for SelfReplacer {
			fn visit_type_mut(&mut self, t: &mut Type) {
				if is_self_type(t) {
					*t = self.replacement.clone()
				} else {
					visit_mut::visit_type_mut(self, t)
				}
			}
		}

		SelfReplacer {
			replacement: call2_strict(
				quote_spanned!(Span::mixed_site()=> #dyn_ #ident#type_generics),
				Type::parse,
			)
			.debugless_unwrap()
			.unwrap(),
		}
		.visit_type_mut(target);
	}

	quote_spanned! {Span::mixed_site()=>
		#(#attribute_errors)*

		/// # Targets
		///
		#(#[doc = concat!("- `", stringify!(#targets), "`")])*
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
				#(if target == ::std::any::TypeId::of::<#targets>() {
					#target_errors
					::#fruit_salad::__::const_assert!(::core::mem::size_of::<*mut #targets>() <= ::core::mem::size_of::<&dyn Dyncast>());
					::core::option::Option::Some(unsafe {
						let mut result_memory = ::core::mem::MaybeUninit::<[u8; ::core::mem::size_of::<&dyn Dyncast>()]>::uninit();
						result_memory
							.as_mut_ptr()
							.cast::<::core::ptr::NonNull<#targets>>()
							.write_unaligned(::core::ptr::NonNull::<#targets>::new_unchecked(
								this.cast::<Self>().as_ptr() as *mut #targets
							)
						);
						result_memory
					})
				} else)* {
					None
				}
			}
		}
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
			// Causes an "unneessary unsafe block" style warning.
			let unsafe_ = self.unsafe_.as_ref().unwrap();
			quote_spanned!(unsafe_.span=> #unsafe_ {})
		} else {
			return None;
		}
		.pipe(Some)
	}
}

/// Implements `Dyncast` for an enum, struct, trait, trail alias, type alias or union.
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
