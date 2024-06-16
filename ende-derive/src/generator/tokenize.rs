use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens, TokenStreamExt};

use crate::ctxt::Ctxt;
use crate::enums::{BitWidth, Endianness, NumEncoding, StrEncoding, StrLen};
use crate::flags::SeekParam;
use crate::parse::{FlattenParam, ModTarget, SeekTarget};

/// A trait similar to ToTokens, except it requires a reference to a Ctxt
pub trait CtxtToTokens {
    fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2;
}

impl CtxtToTokens for Endianness {
    fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
        let ref crate_name = ctxt.flags.crate_name;
        let val: TokenStream2 = self.to_string().parse().unwrap();
        quote!(#crate_name::Endianness::#val)
    }
}

impl CtxtToTokens for NumEncoding {
    fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
        let ref crate_name = ctxt.flags.crate_name;
        let val: TokenStream2 = self.to_string().parse().unwrap();
        quote!(#crate_name::NumEncoding::#val)
    }
}

impl CtxtToTokens for StrEncoding {
    fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
        let ref crate_name = ctxt.flags.crate_name;
        let val: TokenStream2 = self.to_string().parse().unwrap();
        quote!(#crate_name::StrEncoding::#val)
    }
}

impl CtxtToTokens for StrLen {
    fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
        let ref crate_name = ctxt.flags.crate_name;

        let variant = match self {
            StrLen::LengthPrefixed => quote!(LengthPrefixed),
            StrLen::NullTerminated => quote!(NullTerminated),
            StrLen::NullTerminatedFixed(max) => quote!(NullTerminatedFixed(#max)),
        };

        quote!(#crate_name::StrLen::#variant)
    }
}

impl CtxtToTokens for BitWidth {
    fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
        let ref crate_name = ctxt.flags.crate_name;
        let val: TokenStream2 = self.to_string().parse().unwrap();
        quote!(#crate_name::BitWidth::#val)
    }
}

impl ToTokens for FlattenParam {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            FlattenParam::Expr(expr) => expr.to_tokens(tokens),
        }
    }
}

impl ToTokens for ModTarget {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(match self {
            ModTarget::Num { .. } => quote!(num_repr),
            ModTarget::Size { .. } => quote!(size_repr),
            ModTarget::Variant { .. } => quote!(variant_repr),
            ModTarget::String { .. } => quote!(string_repr),
        })
    }
}

impl CtxtToTokens for SeekParam {
    fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
        let ref crate_name = ctxt.flags.crate_name;

        let variant = match self.target {
            SeekTarget::Start { .. } => quote!(Start),
            SeekTarget::End { .. } => quote!(End),
            SeekTarget::Cur { .. } => quote!(Current),
        };

        let ref seek = self.seek;
        quote!(#crate_name::io::SeekFrom::#variant(#seek))
    }
}
