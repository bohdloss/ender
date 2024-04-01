use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, TokenStreamExt};
use crate::ctxt::{Ctxt, Field, Flavor, ItemType, Struct, Variant};
use crate::generator::{ConstCode, RefCode};

impl Ctxt {
	pub(super) fn derive_decode(&self) -> syn::Result<TokenStream2> {
		match self.item_type {
			ItemType::Struct => {
				let (pre, post) = self.flags.mods.derive(self)?;
				let body = self.struct_data.derive_decode(self)?;
				let modified = self.flags.derive_stream_modifiers(self, body)?;

				Ok(quote!(
					#pre
					let __val: Self = { #modified };
					#post
					Ok(__val)
				))
			},
			ItemType::Enum => {
				let ref crate_name = self.flags.crate_name;
				let ref encoder = self.encoder;

				// Edge case for 0-variant enums
				if self.variants.len() == 0 {
					return Ok(quote!(
						Err(#crate_name::EncodingError::InvalidVariant)
					))
				}

				let (pre, post) = self.flags.mods.derive(self)?;
				let mut const_code = ConstCode::new(self);
				let mut variant_code = TokenStream2::new();

				let read_variant = if self.enum_repr.signed() {
					quote!(
						#crate_name::Encoder::read_ivariant(#encoder)? as _
					)
				} else {
					quote!(
						#crate_name::Encoder::read_uvariant(#encoder)? as _
					)
				};

				for variant in self.variants.iter() {
					const_code.append(variant);
					variant_code.append_all(variant.derive_decode(self)?);
				}

				let body = quote!(
					match #read_variant {
						#variant_code
						_ => Err(#crate_name::EncodingError::InvalidVariant),
					}?
				);
				let modified = self.flags.derive_stream_modifiers(self, body)?;

				Ok(quote!(
					#const_code

					#pre
					let __val: Self = { #modified };
					#post
					Ok(__val)
				))
			}
		}
	}
}

impl Variant {
	/// Generates the match arm for this variant
	fn decode_match(&self, _ctxt: &Ctxt, body: TokenStream2) -> syn::Result<TokenStream2> {
		let ref index = self.index.ident;
		Ok(quote!(
			#index => { #body },
		))
	}

	/// Generates code for aggregating together the decoded fields of an enum
	fn decode_aggregate(&self, ctxt: &Ctxt) -> syn::Result<TokenStream2> {
		let ref name = self.name;
		let fields = self.fields.iter().map(|x| &x.name);
		let ref crate_name = ctxt.flags.crate_name;

		Ok(match self.flavor {
			Flavor::Unit => {
				quote!(
					#crate_name::EncodingResult::Ok(Self::#name)
				)
			}
			Flavor::Tuple => {
				quote!(
					#crate_name::EncodingResult::Ok(Self::#name ( #(#fields),* ))
				)
			}
			Flavor::Struct => {
				quote!(
					#crate_name::EncodingResult::Ok(Self::#name { #(#fields),* })
				)
			}
		})
	}

	/// Generates the decode code for this variant, including the match arm
	pub fn derive_decode(&self, ctxt: &Ctxt) -> syn::Result<TokenStream2> {
		let mut ref_code = RefCode::new(ctxt);
		let mut field_code = TokenStream2::new();

		for field in self.fields.iter() {
			field_code.append_all(field.derive_decode(ctxt, &mut ref_code)?);
		}

		let aggregate = self.decode_aggregate(ctxt)?;

		self.decode_match(ctxt, quote!(
			#field_code

			#aggregate
		))
	}
}

impl Struct {
	/// Generates code for aggregating together the decoded fields of a struct
	fn decode_aggregate(&self, ctxt: &Ctxt) -> syn::Result<TokenStream2> {
		let fields = self.fields.iter().map(|x| &x.name);
		let ref crate_name = ctxt.flags.crate_name;
		Ok(match self.flavor {
			Flavor::Unit => {
				quote!(
					#crate_name::EncodingResult::Ok(Self)
				)
			}
			Flavor::Tuple => {
				quote!(
					#crate_name::EncodingResult::Ok(Self ( #(#fields),* ))
				)
			}
			Flavor::Struct => {
				quote!(
					#crate_name::EncodingResult::Ok(Self { #(#fields),* })
				)
			}
		})
	}

	/// Generates the decode code for this struct
	pub fn derive_decode(&self, ctxt: &Ctxt) -> syn::Result<TokenStream2> {
		let mut ref_code = RefCode::new(ctxt);
		let mut field_code = TokenStream2::new();

		for field in self.fields.iter() {
			field_code.append_all(field.derive_decode(ctxt, &mut ref_code)?);
		}

		let aggregate = self.decode_aggregate(ctxt)?;

		Ok(quote!(
			#field_code

			#aggregate ?
		))
	}
}

impl Field {
	pub fn derive_decode(&self, ctxt: &Ctxt, ref_code: &mut RefCode) -> syn::Result<TokenStream2> {
		let ref name = self.name;
		let ref ty = self.ty;
		let ref default = self.flags.default;

		let (pre, post) = self.flags.mods.derive(ctxt)?;
		let decode = self.flags.function.derive(ctxt, self)?;
		let modified = self.flags.derive_stream_modifiers(ctxt, decode)?;
		let decode = if self.flags.skip {
			quote!(
				{
					#ref_code
					#default
				}
			)
		} else if let Some(condition) = &self.flags.condition {
			quote!(
				{
					#ref_code
					if #condition {
						#pre
						let __val: #ty = #modified;
						#post
						__val
					} else {
						#default
					}
				}
			)
		} else {
			quote!(
				{
					#ref_code
					#pre
					let __val: #ty = #modified;
					#post
					__val
				}
			)
		};

		ref_code.append(self);
		let validate = self.flags.derive_validation(ctxt, &ref_code)?;

		let decode = quote!(
			let #name: #ty = #decode;
			#validate
		);

		Ok(decode)
	}
}