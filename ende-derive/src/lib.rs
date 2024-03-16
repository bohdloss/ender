#![allow(unused)]

use proc_macro::{TokenStream as TokenStream1};
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, quote_spanned, TokenStreamExt, ToTokens};
use syn::{Attribute, Data, DataEnum, DataStruct, DeriveInput, Expr, Fields, LitInt, parse_macro_input, token, Type, Variant};
use syn::{Token, parenthesized, Error};
use syn::token::{Paren, Comma, If, Else, As, For, Semi};
use syn::parse::{Parse, ParseStream, Peek};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

static ENDE: &str = "ende";

fn convert_crate_name(name: &str) -> String {
	name.replace("-", "_")
}

/// Emulates the $crate available in regular macros
fn dollar_crate(name: &str) -> Ident {
	let crate_name = std::env::var("CARGO_PKG_NAME").expect("Can't obtain current crate name");
	Ident::new(
		&if crate_name == name { "crate".to_owned() } else { convert_crate_name(name) },
		Span::call_site()
	)
}

macro_rules! make_error {
    ($span:expr, $msg:literal $(,)? $($arg:expr),*) => {{
	    let formatted = format!($msg, $($arg),*);
	    return quote::quote_spanned!($span => compile_error!(#formatted);).into()
    }};
	($msg:literal $(,)? $($arg:expr),*) => {{
		let formatted = format!($msg, $($arg),*);
	    return quote::quote!(compile_error!(#formatted);).into()
    }};
	(??? $span:expr, $msg:literal $(,)? $($arg:expr),*) => {{
		let formatted = format!($msg, $($arg),*);
	    return Err(quote::quote_spanned!($span => compile_error!(#formatted);).into())
    }};
	(??? $msg:literal $(,)? $($arg:expr),*) => {{
		let formatted = format!($msg, $($arg),*);
	    return Err(quote::quote!(compile_error!(#formatted);).into())
    }};
}

mod kw {
	use syn::custom_keyword;
	custom_keyword!(ende);

	custom_keyword!(en);
	custom_keyword!(de);

	custom_keyword!(serde);
	custom_keyword!(skip);
	custom_keyword!(with);
	custom_keyword!(default);
	custom_keyword!(validate);
	custom_keyword!(flatten);

	custom_keyword!(fixed);
	custom_keyword!(leb128);
	custom_keyword!(big_endian);
	custom_keyword!(little_endian);
	custom_keyword!(max);
	custom_keyword!(bit_width);

	custom_keyword!(bit8);
	custom_keyword!(bit16);
	custom_keyword!(bit32);
	custom_keyword!(bit64);
	custom_keyword!(bit128);

	custom_keyword!(num);
	custom_keyword!(size);
	custom_keyword!(variant);
}

mod repr {
	use syn::custom_keyword;
	custom_keyword!(repr);
}

#[allow(unused)]
struct ReprAttr {
	repr_token: repr::repr,
	paren: Paren,
	ty: Type
}

impl Parse for ReprAttr {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let inside;
		Ok(Self {
			repr_token: input.parse()?,
			paren: parenthesized!(inside in input),
			ty: inside.parse()?
		})
	}
}

enum BitWidth {
	Bit8(LitInt),
	Bit16(LitInt),
	Bit32(LitInt),
	Bit64(LitInt),
	Bit128(LitInt),
}

impl BitWidth {
	fn span(&self) -> Span {
		match self {
			BitWidth::Bit8(x) => x.span(),
			BitWidth::Bit16(x) => x.span(),
			BitWidth::Bit32(x) => x.span(),
			BitWidth::Bit64(x) => x.span(),
			BitWidth::Bit128(x) => x.span(),
		}
	}
}

impl Parse for BitWidth {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let width: LitInt = input.parse()?;
		Ok(match width.base10_parse::<u8>()? {
			8 => BitWidth::Bit8(width),
			16 => BitWidth::Bit16(width),
			32 => BitWidth::Bit32(width),
			64 => BitWidth::Bit64(width),
			128 => BitWidth::Bit128(width),
			_ => return Err(Error::new(input.span(), "The only allowed bit widths are 8, 16, 32, 64 and 128"))
		})
	}
}

#[allow(unused)]
enum Target {
	Num(kw::num),
	Size(kw::size),
	Variant(kw::variant)
}

impl Target {
	fn do_peek(input: ParseStream) -> bool {
		input.peek(kw::num) ||
			input.peek(kw::size) ||
			input.peek(kw::variant)
	}
}

impl ToTokens for Target {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		tokens.append(match self {
			Target::Num(x) => Ident::new("num_repr", x.span),
			Target::Size(x) => Ident::new("size_repr", x.span),
			Target::Variant(x) => Ident::new("variant_repr", x.span),
		})
	}
}

impl Parse for Target {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(if input.peek(kw::num) {
			Self::Num(input.parse()?)
		} else if input.peek(kw::size) {
			Self::Size(input.parse()?)
		} else if input.peek(kw::variant) {
			Self::Variant(input.parse()?)
		} else {
			return Err(Error::new(
				input.span(),
				r#"Unknown target parameter. Only "num", "size" and "variant" are allowed in this position"#)
			)
		})
	}
}

#[allow(unused)]
enum Modifier {
	FixedInt(kw::fixed),
	Leb128Int(kw::leb128),
	BigEndian(kw::big_endian),
	LittleEndian(kw::little_endian),
	MaxSize {
		kw: kw::max,
		eq: token::Eq,
		max: Expr
	},
	BitWidth(BitWidth),
}

impl Modifier {
	fn span(&self) -> Span {
		match self {
			Self::FixedInt(x) => x.span,
			Self::Leb128Int(x) => x.span,
			Self::BigEndian(x) => x.span,
			Self::LittleEndian(x) => x.span,
			Self::MaxSize { kw, .. } => kw.span,
			Self::BitWidth(x) => x.span(),
		}
	}
}

impl Parse for Modifier {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(if input.peek(kw::fixed) {
			Self::FixedInt(input.parse()?)
		} else if input.peek(kw::leb128) {
			Self::Leb128Int(input.parse()?)
		} else if input.peek(kw::big_endian) {
			Self::BigEndian(input.parse()?)
		} else if input.peek(kw::little_endian) {
			Self::LittleEndian(input.parse()?)
		} else if input.peek(kw::max) {
			Self::MaxSize {
				kw: input.parse()?,
				eq: input.parse()?,
				max: input.parse()?
			}
		} else if input.peek(LitInt) {
			Self::BitWidth(input.parse()?)
		} else {
			return Err(Error::new(input.span(), "Unknown modifier"))
		})
	}
}

#[allow(unused)]
enum Flag {
	Serde(kw::serde),
	Skip(kw::skip),
	If {
		kw: If,
		eq: token::Eq,
		condition: Expr,
	},
	Default {
		kw: kw::default,
		eq: token::Eq,
		default: Expr
	},
	With {
		kw: kw::with,
		eq: token::Eq,
		with: Expr
	},
	Flatten {
		kw: kw::flatten,
		eq: Option<token::Eq>,
		depth: Option<Expr>
	},
	Validate {
		kw: kw::validate,
		eq: token::Eq,
		expr: Expr
	},
	ModifierList {
		target: Target,
		eq: token::Colon,
		modifiers: Punctuated<Modifier, Comma>
	}
}

impl Flag {
	fn span(&self) -> Span {
		match self {
			Flag::Serde(x) => x.span,
			Flag::Skip(x) => x.span,
			Flag::If { kw, .. } => kw.span,
			Flag::Default { kw, .. } => kw.span,
			Flag::With { kw, .. } => kw.span,
			Flag::Flatten { kw, .. } => kw.span,
			Flag::Validate { kw, ..} => kw.span,
			Flag::ModifierList { target, .. } => target.span()
		}
	}

	fn serde(&self) -> bool {
		match self {
			Flag::Serde(_) => true,
			_ => false
		}
	}

	fn skip(&self) -> bool {
		match self {
			Flag::Skip(_) => true,
			_ => false
		}
	}

	fn condition(&self) -> bool {
		match self {
			Flag::If { .. } => true,
			_ => false
		}
	}

	fn default(&self) -> bool {
		match self {
			Flag::Default { .. } => true,
			_ => false
		}
	}

	fn with(&self) -> bool {
		match self {
			Flag::With { .. } => true,
			_ => false
		}
	}

	fn as_condition(&self) -> Option<&Expr> {
		match self {
			Flag::If { condition, .. } => Some(condition),
			_ => None
		}
	}

	fn as_default(&self) -> Option<&Expr> {
		match self {
			Flag::Default { default, .. } => Some(default),
			_ => None
		}
	}

	fn as_with(&self) -> Option<&Expr> {
		match self {
			Flag::With { with, .. } => Some(with),
			_ => None
		}
	}
}

impl Parse for Flag {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(if Target::do_peek(input) {
			Flag::ModifierList {
				target: input.parse()?,
				eq: input.parse()?,
				modifiers: Punctuated::parse_separated_nonempty(input)?,
			}
		} else if input.peek(kw::serde) {
			Flag::Serde(input.parse()?)
		} else if input.peek(kw::skip) {
			Flag::Skip(input.parse()?)
		} else if input.peek(Token![if]) {
			Flag::If {
				kw: input.parse()?,
				eq: input.parse()?,
				condition: input.parse()?
			}
		} else if input.peek(kw::default) {
			Flag::Default {
				kw: input.parse()?,
				eq: input.parse()?,
				default: input.parse()?
			}
		} else if input.peek(kw::with) {
			Flag::With {
				kw: input.parse()?,
				eq: input.parse()?,
				with: input.parse()?
			}
		} else if input.peek(kw::validate) {
			return Err(Error::new(input.span(), "Still unimplemeted!"))
		} else if input.peek(kw::flatten) {
			let kw = input.parse()?;
			let (eq, depth) = if input.peek(token::Eq) {
				(Some(input.parse()?), Some(input.parse()?))
			} else {
				(None, None)
			};

			Flag::Flatten {
				kw,
				eq,
				depth,
			}
		} else {
			return Err(Error::new(input.span(), "Unknown attribute parameter"))
		})
	}
}

#[allow(unused)]
enum EndeFragment {
	En(kw::en),
	De(kw::de),
	Both
}

impl Parse for EndeFragment {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(kw::en) {
			Ok(EndeFragment::En(input.parse()?))
		} else if input.peek(kw::de) {
			Ok(EndeFragment::De(input.parse()?))
		} else {
			Ok(EndeFragment::Both)
		}
	}
}

#[allow(unused)]
struct Ende {
	ende_token: kw::ende,
	paren: Paren,
	ende: EndeFragment,
	comma: Option<Comma>,
	flag: Punctuated<Flag, Semi>
}

impl Parse for Ende {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let inside;
		let ende;
		Ok(Self {
			ende_token: input.parse()?,
			paren: parenthesized!(inside in input),
			ende: {
				let v = inside.parse()?;
				ende = if let EndeFragment::Both = v { false } else { true };
				v
			},
			comma: {
				if ende {
					Some(inside.parse()?)
				} else {
					None
				}
			},
			flag: Punctuated::parse_terminated(&inside)?
		})
	}
}

impl Ende {
	pub fn from_attributes(attrs: &[Attribute], is_encode: bool) -> Result<Vec<Flag>, TokenStream2> {
		let attrs: Vec<TokenStream2> = attrs
			.iter()
			.filter(|x| x.path().is_ident("ende"))
			.map(|x| x.meta.to_token_stream())
			.collect();

		let mut transformed: Vec<Flag> = Vec::with_capacity(attrs.len());
		for attr in attrs {
			let attr_span = attr.span();
			let x = match syn::parse2::<Ende>(attr) {
				Ok(x) => x,
				Err(error) => make_error!(??? attr_span, "Error parsing ende attribute: {}", error)
			};
			// Skip unneeded attributes
			match x.ende {
				EndeFragment::En(_) => if !is_encode { continue },
				EndeFragment::De(_) => if is_encode { continue },
				EndeFragment::Both => {}
			}

			transformed.append(&mut x.flag.into_iter().collect());
		}

		Ok(transformed)
	}
}

fn apply_modifiers(ident: &Ident, source: &Ident, attrs: &[Flag]) -> Result<(TokenStream2, TokenStream2), TokenStream2> {
	let dollar_crate = dollar_crate(ENDE);
    let mut aggregate = TokenStream2::new();

	let mut found = false;
	for flag in attrs.iter() {
		let middle = match flag {
			Flag::ModifierList { target, modifiers, .. } => {
				let mut aggregate = TokenStream2::new();
				for modifier in modifiers.iter() {
					aggregate.append_all(match &modifier {
						Modifier::FixedInt(_) => {quote!(
							#source.options.#target.num_encoding = #dollar_crate::NumEncoding::FixedInt;
						)}
						Modifier::Leb128Int(_) => {quote!(
							#source.options.#target.num_encoding = #dollar_crate::NumEncoding::Leb128Int;
						)}
						Modifier::BigEndian(_) => {quote!(
							#source.options.#target.endianness = #dollar_crate::Endianness::BigEndian;
						)}
						Modifier::LittleEndian(_) => {quote!(
							#source.options.#target.endianness = #dollar_crate::Endianness::BigEndian;
						)}
						Modifier::MaxSize { max, .. } => {
							match target {
								Target::Size(_) => {}
								_ => make_error!(??? target.span(), r#"Only "size" allowed in conjunction with "max""#)
							}

							quote!(
							#source.options.size_repr.max_size = { #max };
						)}
						Modifier::BitWidth(width) => {
							match target {
								Target::Num(_) => make_error!(??? target.span(), r#"Bit width is only allowed in conjuction with "size" or "variant""#),
								_ => {}
							}

							let width = match width {
								BitWidth::Bit8(x) => quote_spanned!(x.span()=>Bit8),
								BitWidth::Bit16(x) => quote_spanned!(x.span()=>Bit16),
								BitWidth::Bit32(x) => quote_spanned!(x.span()=>Bit32),
								BitWidth::Bit64(x) => quote_spanned!(x.span()=>Bit64),
								BitWidth::Bit128(x) => quote_spanned!(x.span()=>Bit128),
							};
							quote!(
								#source.options.#target.width = #dollar_crate::BitWidth::#width;
							)
						}
					});
				}
				aggregate
			}
			Flag::Flatten { depth, .. } => {
				let depth = depth.as_ref().map(ToTokens::to_token_stream).unwrap_or(quote!(1u64));

				quote!(
					#source.options.flatten = { #depth };
				)
			}
			_ => continue
		};

		found = true;

		aggregate.append_all(middle);
	}

	if !found {
		return Ok((TokenStream2::new(), TokenStream2::new()));
	}

	Ok((quote!(
		let #ident: #dollar_crate::BinOptions = #source.options;
		#aggregate
	),
	quote!(
		#source.options = #ident;
	)))
}

fn gen_encode_fn_call<T: ToTokens>(field: &T, attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
	let dollar_crate = dollar_crate(ENDE);

	if attrs.iter().any(Flag::skip) {
		return Ok(quote!());
	}

	let encode = if let Some(with) = attrs.iter().find(|x| x.with()).and_then(Flag::as_with) {
		quote!(
			{ #with };
		)
	} else {
		if attrs.iter().any(Flag::serde) {
			quote!(
				serde::Serialize::serialize(#field, &mut *__encoder)?;
			)
		} else {
			quote!(
				#dollar_crate::Encode::encode(#field, __encoder)?;
			)
		}
	};

	let (before, after) = apply_modifiers(
		&format_ident!("__state"),
		&format_ident!("__encoder"),
		attrs
	)?;

	if let Some(cond) = attrs.iter().find(|x| x.condition()).and_then(Flag::as_condition) {
		return Ok(quote!(
			#before
			if #cond {
				#encode
			}
			#after
		))
	}

	Ok(quote!(
		#before
		#encode
		#after
	))
}

fn gen_decode_fn_call(attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
	let dollar_crate = dollar_crate(ENDE);

	let default = if let Some(default) = attrs.iter().find(|x| x.default()).and_then(Flag::as_default) {
		default.to_token_stream()
	} else {
		quote!(Default::default())
	};

	if attrs.iter().any(Flag::skip) {
		return Ok(default);
	}

	let decode = if let Some(with) = attrs.iter().find(|x| x.with()).and_then(Flag::as_with) {
		quote!(
			{ #with }
		)
	} else {
		if attrs.iter().any(Flag::serde) {
			quote!(
				serde::Deserialize::deserialize(&mut *__decoder)?
			)
		} else {
			quote!(
				#dollar_crate::Decode::decode(__decoder)?
			)
		}
	};

	let (before, after) = apply_modifiers(
		&format_ident!("__state"),
		&format_ident!("__decoder"),
		attrs
	)?;

	if let Some(cond) = attrs.iter().find(|x| x.condition()).and_then(Flag::as_condition) {
		return Ok(quote!(
			#before
			let __val = if #cond {
				#decode
			} else {
				#default
			};
			#after
			__val
		))
	}

	Ok(quote!(
		#before
		let __val = #decode;
		#after
		__val
	))
}

fn derive_struct_encode(struct_data: &DataStruct) -> Result<TokenStream2, TokenStream2> {
	Ok(match struct_data.fields {
		Fields::Named(ref fields) => {
			let mut ref_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();

			for field in fields.named.iter() {
				let attrs = Ende::from_attributes(&field.attrs, true)?;
				let field_name = field.ident.as_ref().unwrap();

				ref_code.append_all(quote!(
					let ref #field_name = self.#field_name;
				));
				let encode = gen_encode_fn_call(field_name, &attrs)?;
				fields_code.append_all(quote!( { #encode } ));
			}

			quote!(
				#ref_code
				#fields_code
			)
		}
		Fields::Unnamed(ref fields) => {
			let mut ref_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();

			for (field_index, field) in fields.unnamed.iter().enumerate() {
				let attrs = Ende::from_attributes(&field.attrs, true)?;
				let field_accessor: TokenStream2 = field_index.to_string().parse().unwrap();
				let field_name = format_ident!("m{}", field_index);

				ref_code.append_all(quote!(
					let ref #field_name = self.#field_accessor;
				));
				let encode = gen_encode_fn_call(&field_name, &attrs)?;
				fields_code.append_all(quote!( { #encode } ));
			}

			quote!(
				#ref_code
				#fields_code
			)
		}
		Fields::Unit => {
			TokenStream2::new()
		}
	})
}

fn derive_struct_decode(struct_data: &DataStruct, reset_state: &TokenStream2) -> Result<TokenStream2, TokenStream2> {
	Ok(match struct_data.fields {
		Fields::Named(ref fields) => {
			let mut fields_code = TokenStream2::new();
			let mut aggregate_code = TokenStream2::new();
			let mut ref_code = TokenStream2::new();

			for field in fields.named.iter() {
				let field_name = field.ident.as_ref().unwrap();
				let ref field_type = field.ty;

				let attrs = Ende::from_attributes(&field.attrs, false)?;

				let decode = gen_decode_fn_call(&attrs)?;
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
			}

			quote!(
				#fields_code

				#reset_state
				Ok(Self {
					#aggregate_code
				})
			)
		}
		Fields::Unnamed(ref fields) => {
			let mut fields_code = TokenStream2::new();
			let mut aggregate_code = TokenStream2::new();
			let mut ref_code = TokenStream2::new();

			for (idx, field) in fields.unnamed.iter().enumerate() {
				let ref field_name = format_ident!("m{}", idx);
				let ref field_type = field.ty;
				let attrs = Ende::from_attributes(&field.attrs, false)?;

				let decode = gen_decode_fn_call(&attrs)?;
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
			}

			quote!(
				#fields_code

				#reset_state
				Ok(Self(
					#aggregate_code
				))
			)
		}
		Fields::Unit => {
			quote!(
				#reset_state
				Ok(Self)
			)
		}
	})
}

fn derive_variant_encode(variant: &Variant, idx: &Ident, uvariant: bool) -> Result<TokenStream2, TokenStream2> {
	let ref variant_name = variant.ident;
	let write_variant = if uvariant {
		quote!(
			__encoder.write_uvariant(#idx as _)?;
		)
	} else {
		quote!(
			__encoder.write_ivariant(#idx as _)?;
		)
	};

	Ok(match variant.fields {
		Fields::Named(ref fields) => {
			let mut match_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();

			for field in fields.named.iter() {
				let field_name = field.ident.as_ref().unwrap();
				let attrs = Ende::from_attributes(&field.attrs, true)?;

				match_code.append_all(quote!(
					#field_name,
				));
				let encode = gen_encode_fn_call(field_name, &attrs)?;
				fields_code.append_all(quote!( { #encode } ));
			}

			quote!(
				Self::#variant_name { #match_code } => {
					#write_variant
					#fields_code
				}
			)
		}
		Fields::Unnamed(ref fields) => {
			let mut match_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();

			for (field_index, field) in fields.unnamed.iter().enumerate() {
				let field_accessor = format_ident!("m{}", field_index);
				let attrs = Ende::from_attributes(&field.attrs, true)?;

				match_code.append_all(quote!(
					#field_accessor,
				));
				let encode = gen_encode_fn_call(&field_accessor, &attrs)?;
				fields_code.append_all(quote!( { #encode } ));
			}

			quote!(
				Self::#variant_name ( #match_code ) => {
					#write_variant
					#fields_code
				}
			)
		}
		Fields::Unit => {
			quote!(
				Self::#variant_name => {
					#write_variant
				}
			)
		}
	})
}

fn derive_variant_decode(variant: &Variant, idx: &Ident, reset_state: &TokenStream2) -> Result<TokenStream2, TokenStream2> {
	let ref variant_name = variant.ident;

	Ok(match variant.fields {
		Fields::Named(ref fields) => {
			let mut aggregate_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();
			let mut ref_code = TokenStream2::new();

			for field in fields.named.iter() {
				let field_name = field.ident.as_ref().unwrap();
				let ref field_type = field.ty;
				let attrs = Ende::from_attributes(&field.attrs, false)?;

				let decode = gen_decode_fn_call(&attrs)?;
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
			}

			quote!(
				#idx => {
					#fields_code

					#reset_state
					Self::#variant_name {
						#aggregate_code
					}
				}
			)
		}
		Fields::Unnamed(ref fields) => {
			let mut aggregate_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();
			let mut ref_code = TokenStream2::new();

			for (field_index, field) in fields.unnamed.iter().enumerate() {
				let field_name = format_ident!("m{}", field_index);
				let ref field_type = field.ty;
				let attrs = Ende::from_attributes(&field.attrs, false)?;

				let decode = gen_decode_fn_call(&attrs)?;
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
			}

			quote!(
				#idx => {
					#fields_code

					#reset_state
					Self::#variant_name (
						#aggregate_code
					)
				}
			)
		}
		Fields::Unit => {
			quote!(
				#idx => {
					#reset_state
					Self::#variant_name
				}
			)
		}
	})
}

fn derive_enum_encode(enum_data: &DataEnum, repr: Option<ReprAttr>) -> Result<TokenStream2, TokenStream2> {
	if enum_data.variants.len() == 0 {
		return Ok(quote!());
	}

	let mut variants_code = TokenStream2::new();
	let mut const_code = TokenStream2::new();
	let const_ty = repr.map(|x| x.ty).unwrap_or(syn::parse2(quote!(usize)).unwrap());
	let uvariant = const_ty.to_token_stream().to_string().starts_with("u");

	// Iterate over variants
	let mut idx = quote!(0);
	for (actual_index, variant) in enum_data.variants.iter().enumerate() {
		if let Some((_, expr)) = variant.discriminant.as_ref() {
			idx = quote!( ( #expr ) );
		}

		// Aggregate all the match cases
		let const_ident = format_ident!("__index_{}", actual_index);
		variants_code.append_all(derive_variant_encode(variant, &const_ident, uvariant)?);
		const_code.append_all(quote!(const #const_ident: #const_ty = #idx;));

		idx = quote!( ( #const_ident + 1) );
	}
	Ok(quote!(
		#const_code
		match self {
			#variants_code
		}
	))
}

fn derive_enum_decode(enum_data: &DataEnum, repr: Option<ReprAttr>, reset_state: &TokenStream2) -> Result<TokenStream2, TokenStream2> {
	let dollar_crate = dollar_crate(ENDE);

	if enum_data.variants.len() == 0 {
		return Ok(quote!(
			Err(#dollar_crate::EncodingError::InvalidVariant)
		));
	}

	let mut variants_code = TokenStream2::new();
	let mut const_code = TokenStream2::new();
	let const_ty = repr.map(|x| x.ty).unwrap_or(syn::parse2(quote!(usize)).unwrap());
	let uvariant = const_ty.to_token_stream().to_string().starts_with("u");
	let read_variant = if uvariant {
		quote!(
			__decoder.read_uvariant()? as _
		)
	} else {
		quote!(
			__decoder.read_ivariant()? as _
		)
	};

	// Iterate over variants
	let mut idx = quote!(0);
	for (actual_index, variant) in enum_data.variants.iter().enumerate() {
		if let Some((_, expr)) = variant.discriminant.as_ref() {
			idx = quote!( ( #expr ) );
		}

		// Aggregate all the match cases
		let const_ident = format_ident!("__index_{}", actual_index);
		variants_code.append_all(derive_variant_decode(variant, &const_ident, reset_state)?);
		const_code.append_all(quote!(const #const_ident: #const_ty = #idx;));

		idx = quote!( ( #const_ident + 1) );
	}
	Ok(quote!(
		#const_code
		Ok(match #read_variant {
			#variants_code
			_ => return Err(#dollar_crate::EncodingError::InvalidVariant)
		})
	))
}

fn do_derive(input: &DeriveInput, is_encode: bool) -> Result<TokenStream2, TokenStream2> {
	let attrs = Ende::from_attributes(&input.attrs, is_encode)?;
	let (before, after) = apply_modifiers(
		&format_ident!("__global_state"),
		&if is_encode {
			format_ident!("__encoder")
		} else {
			format_ident!("__decoder")
		},
		&attrs
	)?;

	Ok(match input.data {
		Data::Struct(ref struct_data) => {
			if is_encode {
				let encode = derive_struct_encode(struct_data)?;
				quote!(
					#before
					#encode
					#after
				)
			} else {
				let decode = derive_struct_decode(struct_data, &after)?;
				quote!(
					#before
					#decode
				)
			}
		}
		Data::Enum(ref enum_data) => {
			let repr = input.attrs
				.iter()
				.find(|x| x.path().is_ident("repr"))
				.map(|x| x.meta.to_token_stream())
				.map(|x| syn::parse2::<ReprAttr>(x))
				.map(|x| x.unwrap());

			if is_encode {
				let encode = derive_enum_encode(enum_data, repr)?;
				quote!(
					#before
					#encode
					#after
				)
			} else {
				let decode = derive_enum_decode(enum_data, repr, &after)?;
				quote!(
					#before
					#decode
				)
			}
		},
		Data::Union(ref union_data) => {
			if is_encode {
				make_error!(??? union_data.union_token.span(), "Encode cannot be derived on unions")
			} else {
				make_error!(??? union_data.union_token.span(), "Decode cannot be derived on unions")
			}
		}
	})
}

#[proc_macro_derive(Encode, attributes(ende))]
pub fn encode(input: TokenStream1) -> TokenStream1 {
	let dollar_crate = dollar_crate(ENDE);
	let input = parse_macro_input!(input as DeriveInput);
	let ref name = input.ident;

	let body = match do_derive(&input, true) {
		Ok(body) => body,
		Err(error) => return error.into()
	};

	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

	quote!(
		#[automatically_derived]
		impl #impl_generics #dollar_crate::Encode for #name #ty_generics #where_clause {
			fn encode<__T: std::io::Write>(&self, __encoder: &mut #dollar_crate::BinStream<__T>) -> #dollar_crate::EncodingResult<()> {
				#body
				Ok(())
			}
		}
	).into()
}

#[proc_macro_derive(Decode, attributes())]
pub fn decode(input: TokenStream1) -> TokenStream1 {
	let dollar_crate = dollar_crate(ENDE);
	let input = parse_macro_input!(input as DeriveInput);
	let ref name = input.ident;

	let body = match do_derive(&input, false) {
		Ok(body) => body,
		Err(error) => return error.into()
	};

	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

	quote!(
		#[automatically_derived]
		impl #impl_generics #dollar_crate::Decode for #name #ty_generics #where_clause {
			fn decode<__T: std::io::Read>(__decoder: &mut #dollar_crate::BinStream<__T>) -> #dollar_crate::EncodingResult<Self> {
				#body
			}
		}
	).into()
}