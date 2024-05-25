use proc_macro::TokenStream as TokenStream1;

use proc_macro2::{Ident, Span};
use quote::{quote};
use syn::{DeriveInput, GenericParam, LifetimeParam, parse_macro_input, parse_quote};

use crate::ctxt::{Ctxt, Target};

mod ctxt;
mod enums;
mod flags;
mod generator;
mod lifetime;
mod parse;

static ENDE: &str = "ende";

/// Emulates the $crate available in regular macros
fn dollar_crate(name: &str) -> Ident {
    let crate_name = std::env::var("CARGO_PKG_NAME").expect("Can't obtain current crate name");
    Ident::new(
        &if crate_name == name {
            "crate".to_owned()
        } else {
            name.replace("-", "_")
        },
        Span::call_site(),
    )
}

#[proc_macro_derive(Encode, attributes(ende))]
pub fn encode(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as DeriveInput);
    let ctxt = match Ctxt::parse_from(&input, Target::Encode) {
        Ok(ctxt) => ctxt,
        Err(err) => return TokenStream1::from(err.to_compile_error()),
    };

    let (impl_generics, ty_generics, where_clause) = ctxt.generics.split_for_impl();
    let ref crate_name = ctxt.flags.crate_name;
    let ref item_name = ctxt.item_name;
    let ref encoder = ctxt.encoder;
    let ref encoder_generic = ctxt.encoder_generic;

    let body = match ctxt.derive() {
        Ok(ctxt) => ctxt,
        Err(err) => return TokenStream1::from(err.to_compile_error()),
    };
        
    if ctxt.requires_seeking_impl() {
        quote!(
            #[automatically_derived]
            impl #impl_generics #crate_name::Encode for #item_name #ty_generics #where_clause {
                #crate_name::requires_seek!(encode);
                
                fn seek_encode<#encoder_generic: #crate_name::io::Write + #crate_name::io::Seek>(&self, #encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<()> {
                    #body
                }
            }
        )
    } else {
        quote!(
            #[automatically_derived]
            impl #impl_generics #crate_name::Encode for #item_name #ty_generics #where_clause {
                fn encode<#encoder_generic: #crate_name::io::Write>(&self, #encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<()> {
                    #body
                }
                
                #crate_name::empty_seek!(encode);
            }
        )
    }.into()
}

#[proc_macro_derive(Decode, attributes(ende))]
pub fn decode(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as DeriveInput);
    let ctxt = match Ctxt::parse_from(&input, Target::Decode) {
        Ok(ctxt) => ctxt,
        Err(err) => return TokenStream1::from(err.to_compile_error()),
    };

    let (impl_generics, ty_generics, where_clause) = ctxt.generics.split_for_impl();
    let ref crate_name = ctxt.flags.crate_name;
    let ref item_name = ctxt.item_name;
    let ref encoder = ctxt.encoder;
    let ref encoder_generic = ctxt.encoder_generic;

    let body = match ctxt.derive() {
        Ok(ctxt) => ctxt,
        Err(err) => return TokenStream1::from(err.to_compile_error()),
    };

    if ctxt.requires_seeking_impl() {
        quote!(
            #[automatically_derived]
            impl #impl_generics #crate_name::Decode for #item_name #ty_generics #where_clause {
                #crate_name::requires_seek!(decode);
                
                fn seek_decode<#encoder_generic: #crate_name::io::Read + #crate_name::io::Seek>(#encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<Self> {
                    #body
                }
            }
        )
    } else {
        quote!(
            #[automatically_derived]
            impl #impl_generics #crate_name::Decode for #item_name #ty_generics #where_clause {
                fn decode<#encoder_generic: #crate_name::io::Read>(#encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<Self> {
                    #body
                }
                
                #crate_name::empty_seek!(decode);
            }
        )
    }.into()
}

#[proc_macro_derive(BorrowDecode, attributes(ende))]
pub fn borrow_decode(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as DeriveInput);
    let ctxt = match Ctxt::parse_from(&input, Target::BorrowDecode) {
        Ok(ctxt) => ctxt,
        Err(err) => return TokenStream1::from(err.to_compile_error()),
    };

    let body = match ctxt.derive() {
        Ok(ctxt) => ctxt,
        Err(err) => return TokenStream1::from(err.to_compile_error()),
    };

    let ref decoder_lif = ctxt.borrow_data.decoder;
    let lif: LifetimeParam = if ctxt.borrow_data.sub_lifetimes.is_empty() {
        parse_quote!(
            #decoder_lif
        )
    } else {
        let sub_lifs = ctxt.borrow_data.sub_lifetimes.iter();
        parse_quote!(
            #decoder_lif: #(#sub_lifs)+*
        )
    };

    // Inject the decoder's lifetime parameter in the `impl` generics
    let mut generics = ctxt.generics.clone();
    generics.params.insert(0, GenericParam::Lifetime(lif));

    // Impl generics use injected lifetime
    let (impl_generics, _, _) = generics.split_for_impl();
    // Ty and where clause use the original generics
    let (_, ty_generics, where_clause) = ctxt.generics.split_for_impl();
    let ref crate_name = ctxt.flags.crate_name;
    let ref item_name = ctxt.item_name;
    let ref encoder = ctxt.encoder;
    let ref encoder_generic = ctxt.encoder_generic;

    if ctxt.requires_seeking_impl() {
        quote!(
            #[automatically_derived]
            impl #impl_generics #crate_name::BorrowDecode<#decoder_lif> for #item_name #ty_generics #where_clause {
                #crate_name::requires_seek!(borrow_decode(#decoder_lif));
                
                fn seek_borrow_decode<#encoder_generic: #crate_name::io::BorrowRead<#decoder_lif> + #crate_name::io::Seek>(#encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<Self> {
                    #body
                }
            }
        )
    } else {
        quote!(
            #[automatically_derived]
            impl #impl_generics #crate_name::BorrowDecode<#decoder_lif> for #item_name #ty_generics #where_clause {
                fn borrow_decode<#encoder_generic: #crate_name::io::BorrowRead<#decoder_lif>>(#encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<Self> {
                    #body
                }
                
                #crate_name::empty_seek!(borrow_decode(#decoder_lif));
            }
        )
    }.into()
}
