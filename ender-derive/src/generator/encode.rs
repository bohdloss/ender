use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens, TokenStreamExt};

use crate::ctxt::{Ctxt, Field, Flavor, ItemType, Struct, Variant};
use crate::flags::FlagTarget;
use crate::generator::{ConstCode, RefCode};

impl Ctxt {
    pub(super) fn derive_encode(&self) -> syn::Result<TokenStream2> {
        let ref crate_name = self.flags.crate_name;
        let ref item_name = self.item_name;

        match self.item_type {
            ItemType::Struct => {
                let (pre, post) = self.flags.mods.derive(self)?;
                let body = self.struct_data.derive_encode(self)?;
                let modified = self.flags.derive_stream_modifiers(
                    self,
                    body,
                    FlagTarget::Item,
                    item_name.to_string(),
                )?;
                let seek = self.flags.derive_seek(self)?;
                let pos_tracker = self.flags.derive_pos_tracker(self)?;
                let puller = self.flags.derive_puller(self)?;
                let pusher = self.flags.derive_pusher(self)?;

                Ok(quote!(
                    #pos_tracker
                    #puller
                    #pusher
                    #pre
                    #seek
                    { #modified }
                    #post
                    #crate_name::EncodingResult::Ok(())
                ))
            }
            ItemType::Enum => {
                // Edge case for 0-variant enums
                if self.variants.len() == 0 {
                    return Ok(quote!(
                        #crate_name::EncodingResult::Ok(())
                    ));
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
                let modified = self.flags.derive_stream_modifiers(
                    self,
                    body,
                    FlagTarget::Item,
                    item_name.to_string(),
                )?;
                let seek = self.flags.derive_seek(self)?;
                let pos_tracker = self.flags.derive_pos_tracker(self)?;
                let puller = self.flags.derive_puller(self)?;
                let pusher = self.flags.derive_pusher(self)?;

                Ok(quote!(
                    #const_code

                    #pos_tracker
                    #puller
                    #pusher
                    #pre
                    #seek
                    { #modified }
                    #post
                    #crate_name::EncodingResult::Ok(())
                ))
            }
        }
    }
}
impl Variant {
    /// Generates the match arm for this variant
    fn encode_match(&self, ctxt: &Ctxt, body: TokenStream2) -> syn::Result<TokenStream2> {
        let ref name = self.name;
        let fields = self.fields.iter().map(|x| &x.name);
        let body = self.flags.derive_stream_modifiers(
            ctxt,
            body,
            FlagTarget::Variant,
            name.to_string(),
        )?;

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
                #crate_name::Encoder::write_ivariant(#encoder, #index)?;
            )
        } else {
            quote!(
                #crate_name::Encoder::write_uvariant(#encoder, #index)?;
            )
        };

        self.encode_match(
            ctxt,
            quote!(
                #write_variant
                #ref_code
                #field_code
            ),
        )
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
        let ref field_name = self.name;
        let ref field_accessor = self.accessor;
        let ref field_ty = self.ty;

        let (pre, post) = self.flags.mods.derive(ctxt)?;
        let validate = self.flags.derive_validation(ctxt, Some(&ref_code))?;
        let seek = self.flags.derive_seek(ctxt)?;
        let pos_tracker = self.flags.derive_pos_tracker(ctxt)?;
        let puller = self.flags.derive_puller(ctxt)?;
        let pusher = self.flags.derive_pusher(ctxt)?;

        let encode = if let Some(converter) = &self.flags.ty_mods {
            self.flags.function.derive_encode(
                ctxt,
                converter.convert_into(self)?,
                converter.ty(),
            )?
        } else {
            self.flags
                .function
                .derive_encode(ctxt, field_name.to_token_stream(), field_ty)?
        };

        let modified = self.flags.derive_stream_modifiers(
            ctxt,
            encode,
            FlagTarget::Field,
            field_accessor.to_string(),
        )?;

        let encode = if self.flags.skip {
            quote!(
                #validate
            )
        } else if let Some(condition) = &self.flags.condition {
            quote!(
                #validate
                #pos_tracker
                #puller
                #pusher
                if #condition {
                    #pre
                    #seek
                    #modified;
                    #post
                }
            )
        } else {
            quote!(
                #validate
                #pos_tracker
                #puller
                #pusher
                #pre
                #seek
                #modified;
                #post
            )
        };

        Ok(quote!(#encode ; ))
    }
}
