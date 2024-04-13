use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, TokenStreamExt, ToTokens};
use crate::ctxt::Ctxt;

use crate::enums::{AesBits, AesMode, AsymmEncryption, BitWidth, CfbFeedback, Compression, DeflateLevel, Endianness, GZipLevel, NumEncoding, RsaBits, RsaMode, RsaPadding, StrEncoding, StrLenEncoding, SymmEncryption, ZLibLevel, ZStdLevel};
use crate::parse::{CompressionConstructor, EncryptionConstructor, FlattenParam, ModTarget, SecretConstructor};

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

impl CtxtToTokens for StrLenEncoding {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val: TokenStream2 = self.to_string().parse().unwrap();
		quote!(#crate_name::StrLenEncoding::#val)
	}
}

impl CtxtToTokens for BitWidth {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val: TokenStream2 = self.to_string().parse().unwrap();
		quote!(#crate_name::BitWidth::#val)
	}
}

impl CtxtToTokens for SymmEncryption {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val = match self {
			SymmEncryption::None => {
				quote!(None)
			}
			SymmEncryption::Aes(bits, mode) => {
				let bits = bits.ctxt_tokens(ctxt);
				let mode = mode.ctxt_tokens(ctxt);
				quote!(Aes(#bits, #mode))
			}
		};
		quote!(#crate_name::encryption::SymmEncryption::#val)
	}
}

impl CtxtToTokens for AesBits {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val: TokenStream2 = self.to_string().parse().unwrap();
		quote!(#crate_name::encryption::AesBits::#val)
	}
}

impl CtxtToTokens for AesMode {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val = match self {
			AesMode::Ecb => quote!(Ecb),
			AesMode::Cbc => quote!(Cbc),
			AesMode::Cfb(feedback) => {
				let feedback = feedback.ctxt_tokens(ctxt);
				quote!(Cfb(#feedback))
			},
			AesMode::Ofb => quote!(Ofb),
			AesMode::Ctr => quote!(Ctr),
		};
		quote!(#crate_name::encryption::AesMode::#val)
	}
}

impl CtxtToTokens for CfbFeedback {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val: TokenStream2 = self.to_string().parse().unwrap();
		quote!(#crate_name::encryption::CfbFeedback::#val)
	}
}

impl CtxtToTokens for EncryptionConstructor {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		match self {
			EncryptionConstructor::Literal(encryption) => encryption.ctxt_tokens(ctxt),
			EncryptionConstructor::Expr(expr) => expr.to_token_stream(),
		}
	}
}

impl CtxtToTokens for AsymmEncryption {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val = match self {
			AsymmEncryption::Rsa(bits, padding, mode) => {
				let bits = bits.ctxt_tokens(ctxt);
				let padding = padding.ctxt_tokens(ctxt);
				let mode = mode.ctxt_tokens(ctxt);

				quote!(Rsa(#bits, #padding, #mode))
			}
		};
		quote!(#crate_name::encryption::AsymmEncryption::#val)
	}
}

impl CtxtToTokens for RsaBits {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val: TokenStream2 = self.to_string().parse().unwrap();
		quote!(#crate_name::encryption::RsaBits::#val)
	}
}

impl CtxtToTokens for RsaPadding {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val: TokenStream2 = self.to_string().parse().unwrap();
		quote!(#crate_name::encryption::RsaPadding::#val)
	}
}

impl CtxtToTokens for RsaMode {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val: TokenStream2 = self.to_string().parse().unwrap();
		quote!(#crate_name::encryption::RsaMode::#val)
	}
}

impl CtxtToTokens for SecretConstructor {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		match self {
			SecretConstructor::Literal(encryption) => encryption.ctxt_tokens(ctxt),
			SecretConstructor::Expr(expr) => expr.to_token_stream(),
		}
	}
}

impl CtxtToTokens for Compression {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let val = match self {
			Compression::None => quote!(None),
			Compression::ZStd(level) => {
				let level = level.ctxt_tokens(ctxt);
				quote!(ZStd(#level))
			},
			Compression::ZLib(level) => {
				let level = level.ctxt_tokens(ctxt);
				quote!(ZLib(#level))
			},
			Compression::Deflate(level) => {
				let level = level.ctxt_tokens(ctxt);
				quote!(Deflate(#level))
			},
			Compression::GZip(level) => {
				let level = level.ctxt_tokens(ctxt);
				quote!(GZip(#level))
			},
		};
		quote!(#crate_name::compression::Compression::#val)
	}
}

impl CtxtToTokens for ZStdLevel {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let mut val = self.to_string();
		val.insert(0, 'L');
		let val: TokenStream2 = val.parse().unwrap();
		quote!(#crate_name::compression::ZStdLevel::#val)
	}
}

impl CtxtToTokens for ZLibLevel {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let mut val = self.to_string();
		val.insert(0, 'L');
		let val: TokenStream2 = val.parse().unwrap();
		quote!(#crate_name::compression::ZLibLevel::#val)
	}
}

impl CtxtToTokens for DeflateLevel {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let mut val = self.to_string();
		val.insert(0, 'L');
		let val: TokenStream2 = val.parse().unwrap();
		quote!(#crate_name::compression::DeflateLevel::#val)
	}
}

impl CtxtToTokens for GZipLevel {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		let ref crate_name = ctxt.flags.crate_name;
		let mut val = self.to_string();
		val.insert(0, 'L');
		let val: TokenStream2 = val.parse().unwrap();
		quote!(#crate_name::compression::GZipLevel::#val)
	}
}

impl CtxtToTokens for CompressionConstructor {
	fn ctxt_tokens(&self, ctxt: &Ctxt) -> TokenStream2 {
		match self {
			CompressionConstructor::Literal(compression) => compression.ctxt_tokens(ctxt),
			CompressionConstructor::Expr(expr) => expr.to_token_stream(),
		}
	}
}

impl ToTokens for FlattenParam {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		match self {
			FlattenParam::Some(..) => tokens.append_all(quote!(1)),
			FlattenParam::None(..) => tokens.append_all(quote!(0)),
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