use crate::ctxt::{Ctxt, Field, Flavor, ItemType, Struct, Variant};
use crate::flags::FlagTarget;
use crate::generator::{ConstCode, RefCode};
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, TokenStreamExt};

impl Ctxt {
    pub(super) fn derive_decode(&self, in_place: bool) -> syn::Result<TokenStream2> {
        let ref crate_name = self.flags.crate_name;
        let ref item_name = self.item_name;
        match self.item_type {
            ItemType::Struct => {
                let (pre, post) = self.flags.mods.derive(self)?;
                let body = self.struct_data.derive_decode(self, in_place)?;
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

                Ok(if in_place {
                    quote!(
                        #pos_tracker
                        #puller
                        #pusher
                        #pre
                        #seek
                        { #modified };
                        #post
                        #crate_name::EncodingResult::Ok(())
                    )
                } else {
                    quote!(
                        #pos_tracker
                        #puller
                        #pusher
                        #pre
                        #seek
                        let __val: Self = { #modified };
                        #post
                        #crate_name::EncodingResult::Ok(__val)
                    )
                })
            }
            ItemType::Enum => {
                let ref crate_name = self.flags.crate_name;
                let ref encoder = self.encoder;

                // Edge case for 0-variant enums
                if self.variants.len() == 0 {
                    return Ok(quote!(
                        #crate_name::EncodingResult::Err(#crate_name::EncodingError::InvalidVariant(#crate_name::Opaque::from(0usize)))
                    ));
                }

                let (pre, post) = self.flags.mods.derive(self)?;
                let mut const_code = ConstCode::new(self);
                let mut variant_code = TokenStream2::new();
                let mut variant_code_in_place = TokenStream2::new();

                let read_variant = if self.enum_repr.signed() {
                    quote!(
                        #crate_name::Encoder::read_ivariant(#encoder)?
                    )
                } else {
                    quote!(
                        #crate_name::Encoder::read_uvariant(#encoder)?
                    )
                };

                for variant in self.variants.iter() {
                    const_code.append(variant);
                    variant_code.append_all(variant.derive_decode(self, false)?);
                    variant_code_in_place.append_all(variant.derive_decode(self, true)?);
                }

                let body = if in_place {
                    quote!(
                        let __decoded_variant = #read_variant;
                        match self {
                            #variant_code_in_place
                            _ => *self = match __decoded_variant {
                                #variant_code
                                __unknown_variant => #crate_name::EncodingResult::Err(#crate_name::EncodingError::InvalidVariant(#crate_name::Opaque::from(__unknown_variant))),
                            }?
                        }
                    )
                } else {
                    quote!(
                        match #read_variant {
                            #variant_code
                            __unknown_variant => #crate_name::EncodingResult::Err(#crate_name::EncodingError::InvalidVariant(#crate_name::Opaque::from(__unknown_variant))),
                        }?
                    )
                };
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

                Ok(if in_place {
                    quote!(
                        #const_code

                        #pos_tracker
                        #puller
                        #pusher
                        #pre
                        #seek
                        { #modified };
                        #post
                        #crate_name::EncodingResult::Ok(())
                    )
                } else {
                    quote!(
                        #const_code

                        #pos_tracker
                        #puller
                        #pusher
                        #pre
                        #seek
                        let __val: Self = { #modified };
                        #post
                        #crate_name::EncodingResult::Ok(__val)
                    )
                })
            }
        }
    }
}

impl Variant {
    /// Generates the match arm for this variant
    fn decode_match(&self, ctxt: &Ctxt, body: TokenStream2, in_place: bool) -> syn::Result<TokenStream2> {
        let ref index = self.index.ident;
        let ref name = self.name;
        let fields = self.fields.iter().map(|x| &x.name);
        let body = self.flags.derive_stream_modifiers(
            ctxt,
            body,
            FlagTarget::Variant,
            name.to_string(),
        )?;

        Ok(if in_place {
            match self.flavor {
                Flavor::Unit => {
                    quote!(
                    Self::#name if __decoded_variant == #index => { #body },
                )
                }
                Flavor::Tuple => {
                    quote!(
                    Self::#name ( #(#fields),* ) if __decoded_variant == #index => { #body },
                )
                }
                Flavor::Struct => {
                    quote!(
                    Self::#name { #(#fields),* } if __decoded_variant == #index => { #body },
                )
                }
            }
        } else {
            quote!(
            #index => { #body },
        )
        })
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
    pub fn derive_decode(&self, ctxt: &Ctxt, in_place: bool) -> syn::Result<TokenStream2> {
        let mut ref_code = RefCode::new(ctxt);
        let mut field_code = TokenStream2::new();

        for field in self.fields.iter() {
            field_code.append_all(field.derive_decode(ctxt, &mut ref_code, in_place)?);
        }

        let aggregate = if in_place { TokenStream2::new() } else { self.decode_aggregate(ctxt)? };

        self.decode_match(
            ctxt,
            quote!(
                #field_code

                #aggregate
            ),
            in_place
        )
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
    pub fn derive_decode(&self, ctxt: &Ctxt, in_place: bool) -> syn::Result<TokenStream2> {
        let mut ref_code = RefCode::new(ctxt);
        let mut field_code = TokenStream2::new();

        for field in self.fields.iter() {
            field_code.append_all(field.derive_decode(ctxt, &mut ref_code, in_place)?);
        }

        let aggregate = self.decode_aggregate(ctxt)?;

        Ok(if in_place {
            quote!(
                #ref_code
                #field_code
            )
        } else {
            quote!(
                #field_code

                #aggregate ?
            )
        })
    }
}

impl Field {
    pub fn derive_decode(&self, ctxt: &Ctxt, ref_code: &mut RefCode, in_place: bool) -> syn::Result<TokenStream2> {
        let ref field_name = self.name;
        let ref field_accessor = self.accessor;
        let ref field_ty = self.ty;
        let ref default = self.flags.default;

        let mut optimized_in_place = false;

        let (pre, post) = self.flags.mods.derive(ctxt)?;
        let decode = if let Some(converter) = &self.flags.ty_mods {
            converter.convert_from(
                self,
                self.flags
                    .function
                    .derive_decode(ctxt, converter.ty(), &self, false, &mut optimized_in_place)?,
            )?
        } else {
            self.flags.function.derive_decode(ctxt, field_ty, &self, in_place, &mut optimized_in_place)?
        };
        let modified = self.flags.derive_stream_modifiers(
            ctxt,
            decode,
            FlagTarget::Field,
            field_accessor.to_string(),
        )?;
        let seek = self.flags.derive_seek(ctxt)?;
        let pos_tracker = self.flags.derive_pos_tracker(ctxt)?;
        let puller = self.flags.derive_puller(ctxt)?;
        let pusher = self.flags.derive_pusher(ctxt)?;

        let decode = if self.flags.skip {
            if in_place {
                quote!({})
            } else {
                quote!(
                    {
                        #ref_code
                        #default
                    }
                )
            }
        } else if let Some(condition) = &self.flags.condition {
            if in_place {
                if optimized_in_place {
                    quote!(
                        {
                            if #condition {
                                #pre
                                #seek
                                #modified;
                                #post
                            }
                        }
                    )
                } else {
                    quote!(
                        {
                            if #condition {
                                #pre
                                #seek
                                *#field_name = #modified;
                                #post
                            }
                        }
                    )
                }
            } else {
                quote!(
                    {
                        #ref_code
                        if #condition {
                            #pre
                            #seek
                            let __val: #field_ty = #modified;
                            #post
                            __val
                        } else {
                            #default
                        }
                    }
                )
            }
        } else {
            if in_place {
                if optimized_in_place {
                    quote!(
                        {
                            #pre
                            #seek
                            #modified;
                            #post
                        }
                    )
                } else {
                    quote!(
                        {
                            #pre
                            #seek
                            *#field_name = #modified;
                            #post
                        }
                    )
                }
            } else {
                quote!(
                    {
                        #ref_code
                        #pre
                        #seek
                        let __val: #field_ty = #modified;
                        #post
                        __val
                    }
                )
            }
        };

        ref_code.append(self, in_place);
        let validate = self.flags.derive_validation(ctxt, if in_place { None } else { Some(ref_code) })?;

        Ok(if in_place {
            quote!(
                #pos_tracker
                #puller
                #pusher
                #decode
                #validate
            )
        } else {
            quote!(
                #pos_tracker
                #puller
                #pusher
                let #field_name: #field_ty = #decode;
                #validate
            )
        })
    }
}
