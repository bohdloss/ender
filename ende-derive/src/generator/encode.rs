use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, TokenStreamExt};

use crate::ctxt::{Ctxt, Field, Flavor, ItemType, Struct, Variant};
use crate::generator::{ConstCode, RefCode};

impl Ctxt {
	pub(super) fn derive_encode(&self) -> syn::Result<TokenStream2> {
		match self.item_type {
			ItemType::Struct => {
				let (pre, post) = self.flags.mods.derive(self)?;
				let body = self.struct_data.derive_encode(self)?;
				let modified = self.flags.derive_stream_modifiers(self, body)?;

				Ok(quote!(
					#pre
					{ #modified }
					#post
					Ok(())
				))
			},
			ItemType::Enum => {
				// Edge case for 0-variant enums
				if self.variants.len() == 0 {
					return Ok(quote!(
						Ok(())
					))
				}

				let (pre, post) = self.flags.mods.derive(self)?;
				let mut const_code = ConstCode::new(self);
				let mut variant_code = TokenStream2::new();

				for variant in self.variants.iter() {
					const_code.append(variant);
					variant_code.append_all(variant.derive_encode(self)?);
				}

				let body = quote!(
					match self {
						#variant_code
					}
				);
				let modified = self.flags.derive_stream_modifiers(self, body)?;

				Ok(quote!(
					#const_code

					#pre
					{ #modified }
					#post
					Ok(())
				))
			}
		}
	}
}
impl Variant {
	/// Generates the match arm for this variant
	fn encode_match(&self, _ctxt: &Ctxt, body: TokenStream2) -> syn::Result<TokenStream2> {
		let ref name = self.name;
		let fields = self.fields.iter().map(|x| &x.name);
		Ok(match self.flavor {
			Flavor::Unit => {
				quote!(
					Self::#name => { #body },
				)
			}
			Flavor::Tuple => {
				quote!(
					Self::#name ( #(#fields),* ) => { #body },
				)
			}
			Flavor::Struct => {
				quote!(
					Self::#name { #(#fields),* } => { #body },
				)
			}
		})
	}

	/// Generates the encode code for this variant, including the match arm
	pub fn derive_encode(&self, ctxt: &Ctxt) -> syn::Result<TokenStream2> {
		let mut ref_code = RefCode::new(ctxt);
		let mut field_code = TokenStream2::new();

		for field in self.fields.iter() {
			field_code.append_all(field.derive_encode(ctxt, &mut ref_code)?);
		}

		let ref crate_name = ctxt.flags.crate_name;
		let ref encoder = ctxt.encoder;
		let ref index = self.index.ident;

		let write_variant = if ctxt.enum_repr.signed() {
			quote!(
				#crate_name::Encoder::write_ivariant(#encoder, #index as _)?;
			)
		} else {
			quote!(
				#crate_name::Encoder::write_uvariant(#encoder, #index as _)?;
			)
		};

		self.encode_match(ctxt, quote!(
			#write_variant
			#ref_code
			#field_code
		))
	}
}

impl Struct {
	/// Generates the encode code for this struct
	pub fn derive_encode(&self, ctxt: &Ctxt) -> syn::Result<TokenStream2> {
		let mut ref_code = RefCode::new(ctxt);
		let mut field_code = TokenStream2::new();

		for field in self.fields.iter() {
			field_code.append_all(field.derive_encode(ctxt, &mut ref_code)?);
		}

		Ok(quote!(
			#ref_code
			#field_code
		))
	}
}

impl Field {
	pub fn derive_encode(&self, ctxt: &Ctxt, ref_code: &mut RefCode) -> syn::Result<TokenStream2> {
		ref_code.append(self);

		let (pre, post) = self.flags.mods.derive(ctxt)?;
		let validate = self.flags.derive_validation(ctxt, &ref_code)?;
		let encode = self.flags.function.derive(ctxt, self)?;
		let encode = if self.flags.skip {
			quote!(
				#validate
			)
		} else if let Some(condition) = &self.flags.condition {
			quote!(
				#validate
				if #condition {
					#pre
					#encode;
					#post
				}
			)
		} else {
			quote!(
				#validate
				#pre
				#encode;
				#post
			)
		};

		let modified = self.flags.derive_stream_modifiers(ctxt, encode)?;

		Ok(modified)
	}
}