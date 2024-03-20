use proc_macro::{TokenStream as TokenStream1};
use std::str::FromStr;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, quote_spanned, TokenStreamExt, ToTokens};
use syn::{Attribute, Data, DataEnum, DataStruct, DeriveInput, Expr, Fields, LitInt, LitStr, parse_macro_input, token, Type, Variant};
use syn::{Token, parenthesized, Error};
use syn::token::{Paren, Comma, If, Semi};
use syn::parse::{Parse, ParseStream};
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
	custom_keyword!(encrypted);
	custom_keyword!(compressed);

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

#[allow(unused)]
enum BitWidth {
	Bit8(LitInt),
	Bit16(LitInt),
	Bit32(LitInt),
	Bit64(LitInt),
	Bit128(LitInt),
}

#[allow(unused)]
enum AesBits {
	N128,
	N192,
	N256
}

impl ToTokens for AesBits {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		let ident = match self {
			AesBits::N128 => quote!(N128),
			AesBits::N192 => quote!(N192),
			AesBits::N256 => quote!(N256),
		};
		tokens.append_all(quote!(#dollar_crate::encryption::AesBits::#ident))
	}
}

#[allow(unused)]
enum AesMode {
	Ecb,
	Cbc,
	Cfb(CfbFeedback),
	Ofb,
	Ctr
}

impl ToTokens for AesMode {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		tokens.append_all(match self {
			AesMode::Ecb => quote!(#dollar_crate::encryption::AesMode::Ecb),
			AesMode::Cbc => quote!(#dollar_crate::encryption::AesMode::Cbc),
			AesMode::Cfb(x) => quote!(#dollar_crate::encryption::AesMode::Cfb(#x)),
			AesMode::Ofb => quote!(#dollar_crate::encryption::AesMode::Ofb),
			AesMode::Ctr => quote!(#dollar_crate::encryption::AesMode::Ctr),
		})
	}
}

#[allow(unused)]
enum CfbFeedback {
	N1,
	N8,
	N128
}

impl ToTokens for CfbFeedback {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		let ident = match self {
			CfbFeedback::N1 => quote!(N1),
			CfbFeedback::N8 => quote!(N8),
			CfbFeedback::N128 => quote!(N128),
		};
		tokens.append_all(quote!(#dollar_crate::encryption::CfbFeedback::#ident))
	}
}

#[allow(unused)]
enum RsaPadding {
	None,
	Pkcs1,
	Pkcs1Oaep,
	Pkcs1Pss
}

impl ToTokens for RsaPadding {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		tokens.append_all(match self {
			RsaPadding::None => quote!(#dollar_crate::encryption::RsaPadding::None),
			RsaPadding::Pkcs1 => quote!(#dollar_crate::encryption::RsaPadding::Pkcs1),
			RsaPadding::Pkcs1Oaep => quote!(#dollar_crate::encryption::RsaPadding::Pkcs1Oaep),
			RsaPadding::Pkcs1Pss => quote!(#dollar_crate::encryption::RsaPadding::Pkcs1Pss),
		});
	}
}

#[allow(unused)]
enum RsaBits {
	N1024,
	N2048,
	N4096,
}

impl ToTokens for RsaBits {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		tokens.append_all(match self {
			RsaBits::N1024 => quote!(#dollar_crate::encryption::RsaBits::N1024),
			RsaBits::N2048 => quote!(#dollar_crate::encryption::RsaBits::N2048),
			RsaBits::N4096 => quote!(#dollar_crate::encryption::RsaBits::N4096),
		})
	}
}

#[allow(unused)]
enum RsaMode {
	Normal,
	Reverse
}

impl ToTokens for RsaMode {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		tokens.append_all(match self {
			RsaMode::Normal => quote!(#dollar_crate::encryption::RsaMode::Normal),
			RsaMode::Reverse => quote!(#dollar_crate::encryption::RsaMode::Reverse),
		});
	}
}

#[allow(unused)]
enum Encryption {
	Aes(AesBits, AesMode),
	Rsa(RsaBits, RsaPadding, RsaMode)
}

impl ToTokens for Encryption {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		tokens.append_all(match self {
			Encryption::Aes(bits, mode) => quote!(#dollar_crate::encryption::Encryption::Aes(#bits, #mode)),
			Encryption::Rsa(..) => unimplemented!(),
		})
	}
}

impl FromStr for Encryption {
	type Err = &'static str;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		const USAGE: &str = r#"Invalid encryption format. Usage: "{key_size}-bit {cipher}/{mode}/{padding}""#;

		let (key_size, rest) = s.split_once("-").ok_or(USAGE)?;
		let (bit_token, rest) = rest.split_once(" ").ok_or(USAGE)?;
		let (cipher, rest) = rest.split_once("/").ok_or(USAGE)?;
		let (mode, padding) = rest.split_once("/").unwrap_or((rest, ""));

		if bit_token != "bit" {
			return Err(USAGE);
		}

		Ok(match cipher {
			"AES" => {
				let bits = match key_size {
					"128" => AesBits::N128,
					"192" => AesBits::N192,
					"256" => AesBits::N256,
					_ => return Err(r#"Allowed key sizes for AES are: 128, 192, 256"#)
				};

				let mode = if mode.starts_with("CFB") {
				    let cfb_feedback = match &mode[3..] {
						"1" => CfbFeedback::N1,
					    "8" => CfbFeedback::N8,
					    "128" => CfbFeedback::N128,
					    _ => return Err(r#"Allowed CFB feedback sizes are: 1, 8, 128"#)
				    };
					AesMode::Cfb(cfb_feedback)
				} else {
					match mode {
						"ECB" => AesMode::Ecb,
						"CBC" => AesMode::Cbc,
						"OFB" => AesMode::Ofb,
						"CTR" => AesMode::Ctr,
						_ => return Err(r#"Allowed modes for AES are: ECB, CBC, CFB, OFB, CTR"#)
					}
				};

				let _padding = match padding {
					"" => {},
					_ => return Err(r#"Allowed padding modes for AES are: <empty>"#)
				};

				Encryption::Aes(bits, mode)
			},
			"RSA" | "revRSA" => {
				let bits = match key_size {
					"1024" => RsaBits::N1024,
					"2048" => RsaBits::N2048,
					"4096" => RsaBits::N4096,
					_ => return Err(r#"Allowed key sizes for RSA are: 1024, 2048 (recommended), 4096"#)
				};

				match mode {
					"ECB" => {},
					_ => return Err(r#"Allowed modes for RSA are: ECB"#)
				}

				let padding = match padding {
					"None" => RsaPadding::None,
					"PKCS1" => RsaPadding::Pkcs1,
					"PKCS1_OAEP" => RsaPadding::Pkcs1Oaep,
					"PKCS1_PSS" => RsaPadding::Pkcs1Pss,
					_ => return Err(r#"Allowed padding modes for RSA are: PKCS1, PKCS1_OAEP, PKCS1_PSS"#)
				};

				let rsa_mode = if cipher.starts_with("rev") { RsaMode::Reverse } else { RsaMode::Normal };

				Encryption::Rsa(bits, padding, rsa_mode)
			}
			_ => return Err(r#"Allowed ciphers are: AES, RSA, revRSA"#)
		})
	}
}

#[allow(unused)]
enum Compression {
	ZStd(u8),
	ZLib(u8),
	Deflate(u8),
	GZip(u8)
}

impl ToTokens for Compression {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		let dollar_crate = dollar_crate(ENDE);
		tokens.append_all(match self {
			Compression::ZStd(x) => {
				let ident = format_ident!("L{}", x);
				quote!(#dollar_crate::compression::Compression::ZStd(#dollar_crate::compression::ZStdLevel::#ident))
			}
			Compression::ZLib(x) => {
				let ident = format_ident!("L{}", x);
				quote!(#dollar_crate::compression::Compression::ZLib(#dollar_crate::compression::ZLibLevel::#ident))
			}
			Compression::Deflate(x) => {
				let ident = format_ident!("L{}", x);
				quote!(#dollar_crate::compression::Compression::Deflate(#dollar_crate::compression::DeflateLevel::#ident))
			}
			Compression::GZip(x) => {
				let ident = format_ident!("L{}", x);
				quote!(#dollar_crate::compression::Compression::GZip(#dollar_crate::compression::GZipLevel::#ident))
			}
		});
	}
}

impl FromStr for Compression {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		const USAGE: &str = r#"Invalid compression format. Usage: "{format}/{level}""#;

		let (format, level) = s.split_once("/").ok_or(USAGE.to_owned())?;

		let level = u8::from_str(level).map_err(|x| x.to_string())?;

		fn in_range(level: u8, min: u8, max: u8) -> Result<u8, String> {
			if level >= min && level <= max {
				Ok(level)
			} else {
				Err(format!("Compression level must be within range {}-{}", min, max))
			}
		}

		Ok(match format {
			"ZStd" => Compression::ZStd(in_range(level, 1, 22)?),
			"ZLib" => Compression::ZLib(in_range(level, 0, 9)?),
			"Deflate" => Compression::Deflate(in_range(level, 0, 9)?),
			"GZip" => Compression::GZip(in_range(level, 1, 9)?),
			_ => return Err(r#"Allowed compression formats are: ZStd, ZLib, Deflate, GZip"#.to_owned())
		})
	}
}

impl BitWidth {
	#[allow(unused)]
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
	#[allow(unused)]
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
enum EncryptionParam {
	Static(Encryption),
	Expr(Expr)
}

impl EncryptionParam {
	fn to_expr(&self) -> Expr {
		match self {
			EncryptionParam::Static(x) => syn::parse2(x.to_token_stream()).unwrap(),
			EncryptionParam::Expr(x) => x.clone(),
		}
	}

	// Rsa needs to be treated differently
	fn is_rsa(&self) -> bool {
		match self {
			EncryptionParam::Static(x) => match x {
				Encryption::Rsa(..) => true,
				_ => false
			}
			_ => false
		}
	}
}

#[allow(unused)]
enum CompressionParam {
	Static(Compression),
	Expr(Expr)
}

impl CompressionParam {
	fn to_expr(&self) -> Expr {
		match self {
			CompressionParam::Static(x) => syn::parse2(x.to_token_stream()).unwrap(),
			CompressionParam::Expr(x) => x.clone(),
		}
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
		expr: Expr,
		comma: Option<Comma>,
		format: Option<LitStr>,
		fmt_args: Punctuated<Expr, Comma>
	},
	Encrypted {
		kw: kw::encrypted,
		eq: token::Eq,
		encryption: EncryptionParam,
		comma1: Option<Comma>,
		key: Option<Expr>,
		comma2: Option<Comma>,
		iv: Option<Expr>
	},
	Compressed {
		kw: kw::compressed,
		eq: token::Eq,
		compression: CompressionParam
	},
	ModifierList {
		target: Target,
		eq: token::Colon,
		modifiers: Punctuated<Modifier, Comma>
	}
}

impl Flag {
	#[allow(unused)]
	fn span(&self) -> Span {
		match self {
			Flag::Serde(x) => x.span,
			Flag::Skip(x) => x.span,
			Flag::If { kw, .. } => kw.span,
			Flag::Default { kw, .. } => kw.span,
			Flag::With { kw, .. } => kw.span,
			Flag::Flatten { kw, .. } => kw.span,
			Flag::Validate { kw, ..} => kw.span,
			Flag::Encrypted { kw, .. } => kw.span,
			Flag::Compressed { kw, .. } => kw.span,
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

	fn validate(&self) -> bool {
		match self {
			Flag::Validate { .. } => true,
			_ => false
		}
	}

	fn flatten(&self) -> bool {
		match self {
			Flag::Flatten { .. } => true,
			_ => false
		}
	}

	fn stream_modifier(&self) -> bool {
		match self {
			Flag::Encrypted { .. } | Flag::Compressed { .. } => true,
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

	fn as_validate(&self) -> Option<(&Expr, Option<&LitStr>, &Punctuated<Expr, Comma>)> {
		match self {
			Flag::Validate { expr, format, fmt_args, .. } => Some((expr, format.as_ref(), fmt_args)),
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
			let kw = input.parse()?;
			let eq = input.parse()?;
			let expr = input.parse()?;

			let (comma, format, fmt_args) = if input.peek(Comma) {
				let comma = input.parse()?;
				let format = input.parse()?;

				let fmt_args = if input.peek(Comma) {
					input.parse::<Comma>()?;
					Punctuated::parse_separated_nonempty(input)?
				} else {
					Punctuated::new()
				};

				(Some(comma), Some(format), fmt_args)
			} else {
				(None, None, Punctuated::new())
			};

			Flag::Validate {
				kw,
				eq,
				expr,
				comma,
				format,
				fmt_args,
			}
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
		} else if input.peek(kw::encrypted) {
			let kw = input.parse()?;
			let eq = input.parse()?;

			let encryption = if input.peek(LitStr) {
				let encryption: LitStr = input.parse()?;
				let encryption = Encryption::from_str(&encryption.value())
					.map_err(|x| Error::new(encryption.span(), x))?;

				EncryptionParam::Static(encryption)
			} else {
				EncryptionParam::Expr(input.parse()?)
			};

			let (comma1, key) = if input.peek(Comma) {
				(Some(input.parse()?), Some(input.parse()?))
			} else {
				(None, None)
			};

			let (comma2, iv) = if input.peek(Comma) {
				(Some(input.parse()?), Some(input.parse()?))
			} else {
				(None, None)
			};

			Flag::Encrypted {
				kw,
				eq,
				encryption,
				comma1,
				key,
				comma2,
				iv,
			}
		} else if input.peek(kw::compressed) {
			let kw = input.parse()?;
			let eq = input.parse()?;

			let compression = if input.peek(LitStr) {
				let compression: LitStr = input.parse()?;
				let compression = Compression::from_str(&compression.value())
					.map_err(|x| Error::new(compression.span(), x))?;

				CompressionParam::Static(compression)
			} else {
				CompressionParam::Expr(input.parse()?)
			};

			Flag::Compressed {
				kw,
				eq,
				compression,
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
			let _attr_span = attr.span();
			let x = match syn::parse2::<Ende>(attr) {
				Ok(x) => x,
				Err(error) => make_error!(??? error.span(), "Error parsing ende attribute: {}", error)
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

	fn ref_code_needed(attrs: &[Flag]) -> bool {
		attrs.iter().any(|x|
			x.condition() ||
				x.default() ||
				x.with() ||
				x.flatten() ||
				x.stream_modifier()
		)
	}
}

fn gen_validation_code(ref_code: Option<&TokenStream2>, attrs: &[Flag]) -> Result<Option<TokenStream2>, TokenStream2> {
	let dollar_crate = dollar_crate(ENDE);
	Ok(if let Some((cond, format, fmt_args)) = attrs.iter().find(|x| x.validate()).and_then(Flag::as_validate) {
		let format_code = if let Some(format) = format {
			let fmt_args = fmt_args.iter();
			quote!(format!(#format, #(#fmt_args),*))
		} else {
			quote!(format!("Assertion failed"))
		};

		Some(quote!(
			{
				#ref_code
				if !{ #cond } {
					return Err(#dollar_crate::EncodingError::ValidationError(#format_code));
				}
			}
		))
	} else {
		None
	})
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
				let depth = depth.as_ref().map(ToTokens::to_token_stream).unwrap_or(quote!(1usize));

				quote!(
					#source.options.flatten = { #depth };
				)
			}
			Flag::Encrypted { encryption, key, iv, .. } => {
				if let EncryptionParam::Static(x) = encryption {
					if let Encryption::Rsa(bits, padding, mode) = x {

						if iv.is_some() {
							make_error!(??? iv.span(), "IVs are not needed for RSA encryption");
						}

						let key = key.as_ref().map(|key| {
							quote!(#source.crypto.rsa.store_key(#key);)
						});

						quote!(
							#source.crypto.rsa.bits = #bits;
							#source.crypto.rsa.padding = #padding;
							#source.crypto.rsa.mode = #mode;
							#key
						)
					} else { continue }
				} else { continue }
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

fn apply_stream_modifiers(is_encode: bool, code: TokenStream2, attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
	let dollar_crate = dollar_crate(ENDE);
	let mut aggregate = code;
	for x in attrs.iter() {
		match x {
			Flag::Compressed { compression, .. } => {
				let compression = compression.to_expr();
				aggregate = if is_encode {
					quote!(
						#dollar_crate::compression::encode_with_compression(
							__encoder,
							#compression,
							|__encoder| {
								#aggregate
								Ok(())
							}
						)?;
					)
				} else {
					quote!(
						#dollar_crate::compression::decode_with_compression(
							__decoder,
							#compression,
							|__decoder| { Ok({ #aggregate }) }
						)?
					)
				};
			},
			Flag::Encrypted { encryption, key, iv, .. } => {
				if encryption.is_rsa() { continue }

				let key: TokenStream2 = key.as_ref().map(|x| quote!(Some(#x))).unwrap_or(quote!(None));
				let iv: TokenStream2 = iv.as_ref().map(|x| quote!(Some(#x))).unwrap_or(quote!(None));

				let encryption = encryption.to_expr();
				aggregate = if is_encode {
					quote!(
						#dollar_crate::encryption::encode_with_encryption(
							__encoder,
							#encryption,
							#key,
							#iv,
							|__encoder| {
								#aggregate
								Ok(())
							}
						)?;
					)
				} else {
					quote!(
						#dollar_crate::encryption::decode_with_encryption(
							__decoder,
							#encryption,
							#key,
							#iv,
							|__decoder| { Ok({ #aggregate }) }
						)?
					)
				};
			}
			_ => continue
		}
	}

	Ok(aggregate)
}

fn gen_encode_fn_call(field: TokenStream2, attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
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

	let with_condition = if let Some(cond) = attrs.iter().find(|x| x.condition()).and_then(Flag::as_condition) {
		quote!(
			#before
			if #cond {
				#encode
			}
			#after
		)
	} else {
		quote!(
			#before
			#encode
			#after
		)
	};

	Ok(apply_stream_modifiers(true, with_condition, attrs)?)
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

	let with_condition = if let Some(cond) = attrs.iter().find(|x| x.condition()).and_then(Flag::as_condition) {
		quote!(
			#before
			let __val = if #cond {
				#decode
			} else {
				#default
			};
			#after
			__val
		)
	} else {
		quote!(
			#before
			let __val = #decode;
			#after
			__val
		)
	};

	Ok(apply_stream_modifiers(false, with_condition, attrs)?)
}

fn derive_struct_encode(struct_data: &DataStruct, pre_return: &TokenStream2, attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
	let code = match struct_data.fields {
		Fields::Named(ref fields) => {
			let mut ref_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();
			let mut ref_needed = false;

			for field in fields.named.iter() {
				let attrs = Ende::from_attributes(&field.attrs, true)?;
				let field_name = field.ident.as_ref().unwrap();

				ref_code.append_all(quote!(
					let ref #field_name = self.#field_name;
				));
				let encode = gen_encode_fn_call(quote!(&self.#field_name), &attrs)?;
				let validate = gen_validation_code(Some(&ref_code), &attrs)?;
				ref_needed |= Ende::ref_code_needed(&attrs);
				fields_code.append_all(quote!(
					#validate
					{ #encode }
				));
			}

			let ref_code = ref_needed.then_some(ref_code);
			quote!(
				#ref_code
				#fields_code
				#pre_return
			)
		}
		Fields::Unnamed(ref fields) => {
			let mut ref_code = TokenStream2::new();
			let mut fields_code = TokenStream2::new();
			let mut ref_needed = false;

			for (field_index, field) in fields.unnamed.iter().enumerate() {
				let attrs = Ende::from_attributes(&field.attrs, true)?;
				let field_accessor: TokenStream2 = field_index.to_string().parse().unwrap();
				let field_name = format_ident!("m{}", field_index);

				ref_code.append_all(quote!(
					let ref #field_name = self.#field_accessor;
				));
				let encode = gen_encode_fn_call(quote!(&self.#field_accessor), &attrs)?;
				let validate = gen_validation_code(Some(&ref_code), &attrs)?;
				ref_needed |= Ende::ref_code_needed(&attrs);
				fields_code.append_all(quote!(
					#validate
					{ #encode }
				));
			}

			let ref_code = ref_needed.then_some(ref_code);
			quote!(
				#ref_code
				#fields_code
				#pre_return
			)
		}
		Fields::Unit => {
			quote!(
				#pre_return
			)
		}
	};

	let transformed = apply_stream_modifiers(true, code, attrs)?;

	Ok(quote!(
		#transformed
		Ok(())
	))
}

fn derive_struct_decode(struct_data: &DataStruct, pre_return: &TokenStream2, attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
	let (fields_code, aggregate_code) = match struct_data.fields {
		Fields::Named(ref fields) => {
			let mut fields_code = TokenStream2::new();
			let mut aggregate_code = TokenStream2::new();
			let mut ref_code = TokenStream2::new();

			for field in fields.named.iter() {
				let field_name = field.ident.as_ref().unwrap();
				let ref field_type = field.ty;

				let attrs = Ende::from_attributes(&field.attrs, false)?;

				let decode = gen_decode_fn_call(&attrs)?;
				let maybe_ref_code = Ende::ref_code_needed(&attrs).then_some(&ref_code);
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#maybe_ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
				let validate = gen_validation_code(Some(&ref_code), &attrs)?;
				fields_code.append_all(validate);
			}

			(fields_code, quote!(Self { #aggregate_code }))
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
				let maybe_ref_code = Ende::ref_code_needed(&attrs).then_some(&ref_code);
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#maybe_ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
				let validate = gen_validation_code(Some(&ref_code), &attrs)?;
				fields_code.append_all(validate);
			}

			(fields_code, quote!(Self( #aggregate_code )))
		}
		Fields::Unit => {
			(quote!(), quote!(Self))
		}
	};

	let transformed = apply_stream_modifiers(false, aggregate_code, attrs)?;

	Ok(quote!(
		#fields_code
		#pre_return
		Ok(#transformed)
	))
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
				let encode = gen_encode_fn_call(quote!(#field_name), &attrs)?;
				let validate = gen_validation_code(None, &attrs)?;
				fields_code.append_all(quote!(
					#validate
					{ #encode }
				));
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
				let field_name = format_ident!("m{}", field_index);
				let attrs = Ende::from_attributes(&field.attrs, true)?;

				match_code.append_all(quote!(
					#field_name,
				));
				let encode = gen_encode_fn_call(quote!(#field_name), &attrs)?;
				let validate = gen_validation_code(None, &attrs)?;
				fields_code.append_all(quote!(
					#validate
					{ #encode }
				));
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

fn derive_variant_decode(variant: &Variant, idx: &Ident, pre_return: &TokenStream2) -> Result<TokenStream2, TokenStream2> {
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
				let maybe_ref_code = Ende::ref_code_needed(&attrs).then_some(&ref_code);
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#maybe_ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
				let validate = gen_validation_code(Some(&ref_code), &attrs)?;
				fields_code.append_all(validate);
			}

			quote!(
				#idx => {
					#fields_code

					#pre_return
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
				let maybe_ref_code = Ende::ref_code_needed(&attrs).then_some(&ref_code);
				fields_code.append_all(quote!(
					let #field_name: #field_type = {
						#maybe_ref_code
						#decode
					};
				));
				aggregate_code.append_all(quote!(
					#field_name,
				));
				ref_code.append_all(quote!(
					let ref #field_name = #field_name;
				));
				let validate = gen_validation_code(Some(&ref_code), &attrs)?;
				fields_code.append_all(validate);
			}

			quote!(
				#idx => {
					#fields_code

					#pre_return
					Self::#variant_name (
						#aggregate_code
					)
				}
			)
		}
		Fields::Unit => {
			quote!(
				#idx => {
					#pre_return
					Self::#variant_name
				}
			)
		}
	})
}

fn derive_enum_encode(enum_data: &DataEnum, repr: Option<ReprAttr>, pre_return: &TokenStream2, attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
	if enum_data.variants.len() == 0 {
		return Ok(quote!(Ok(())));
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

	let transformed = apply_stream_modifiers(true, quote!(
		#const_code
		match self {
			#variants_code
		}
		#pre_return
	), attrs)?;

	Ok(quote!(
		#transformed
		Ok(())
	))
}

fn derive_enum_decode(enum_data: &DataEnum, repr: Option<ReprAttr>, pre_return: &TokenStream2, attrs: &[Flag]) -> Result<TokenStream2, TokenStream2> {
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
		variants_code.append_all(derive_variant_decode(variant, &const_ident, pre_return)?);
		const_code.append_all(quote!(const #const_ident: #const_ty = #idx;));

		idx = quote!( ( #const_ident + 1) );
	}

	let transformed = apply_stream_modifiers(false, quote!(
		match #read_variant {
			#variants_code
			_ => return Err(#dollar_crate::EncodingError::InvalidVariant)
		}
	), attrs)?;

	Ok(quote!(
		#const_code
		Ok(#transformed)
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

	let code = match input.data {
		Data::Struct(ref struct_data) => {
			if is_encode {
				let encode = derive_struct_encode(struct_data, &after, &attrs)?;
				quote!(
					#before
					#encode
				)
			} else {
				let decode = derive_struct_decode(struct_data, &after, &attrs)?;
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
				let encode = derive_enum_encode(enum_data, repr, &after, &attrs)?;
				quote!(
					#before
					#encode
				)
			} else {
				let decode = derive_enum_decode(enum_data, repr, &after, &attrs)?;
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
	};

	Ok(code)
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
			}
		}
	).into()
}

#[proc_macro_derive(Decode, attributes(ende))]
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