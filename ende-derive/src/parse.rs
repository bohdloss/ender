use std::str::FromStr;
use parse_display::Display;
use proc_macro2::{Ident, Span};
use syn::{Error, Expr, LitInt, LitStr, parenthesized, Token, Type};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Paren;
use crate::ctxt::Scope;
use crate::enums::{AsymmEncryption, BitWidth, Compression, SymmEncryption};

const FLAGS_USAGE: &str = r#"Unknown Flag. The following are valid flags:
	en - Scope flag. All the following flags only apply to the encoding step. Can only be used as the first flag.
	de - Scope flag. All the following flags only apply to the decoding step. Can only be used as the first flag.
	crate: $ident - Changes the name of the ende crate, in case it was re-exported with another name.
	serde: $ident - The field or item should be encoded/decoded using serde. Optionally, the name of the serde crate can be provided in case it was re-exported with another name.
	skip - The field or item shouldn't be encoded/decoded. During decoding, it will fall back to the default for that field or item (can be overridden by the `default` flag).
	if: $expr - The field should only be encoded if $expr evaluates to true. If the condition is false while decoding, it will fall back to the default for that field (can be overridden by the `default` flag).
	default: $expr - The default value for a field or item that cannot be decoded for whatever reason is "Default::default()". This flag allows changing that to $expr.
	with: $expr - The field should be encoded/decoded using the given $expr. Only valid when the Scope flag is specified.
	as: $ty - The field should be encoded/decoded as if it was of the given type. It should then be converted back to the appropriate type using From or Into traits
	flatten: $expr - Changes the flatten state variable. See the documentation for "Encode" and "Decode" for more info.
	validate: $expr, $literal, $expr, $expr, ... - Performs a check before encoding and after decoding a field. If the check fails, an error is returned. Allows specifying a custom error message with fmt arguments (optional).
	encrypted: $expr, key: $expr, iv: $expr - The field should be encoded/decoded using encryption. The first argument can be a literal (example: "128-bit AES/CBC") or an expression. Allows specifying a key and iv (optional).
	secret: $expr, public: $expr, private: $expr - The field is a block of data encrypted using asymmetric encryption. The first argument can be a literal (example: "2048-bit RSA/ECB/PKCS1") or an expression. Allows specifying a public and private key (optional).
	compressed: $expr - The field should be encoded/decoded using compression. The argument can be a literal (example: "ZLib/6") or an expression.
	$target: $modifier, $modifier, ... - Temporarily changes the settings while encoding/decoding a field or item. Target can be "num"/"size"/"variant". Modifier can be bit-width (8, 16, 32, 64, 128), endianness (big_endian, little_endian), num-encoding (fixed, leb128), max-size (max = $expr).
"#;

const ENCRYPTION_USAGE: &str = r#"Unknown encryption parameter. Usage example: encrypted: $expr, key: $expr, iv: $expr"#;
const SECRET_USAGE: &str = r#"Unknown secret parameter. Usage example: secret: $expr, public: $expr, private: $expr"#;

const MODIFIER_USAGE: &str = r#"Unknown modifier. Modifier can be bit-width (8, 16, 32, 64, 128), endianness (big_endian, little_endian), num-encoding (fixed, leb128), max-size (max = $expr)"#;

const BITWIDTH_USAGE: &str = r#"Invalid bit-width modifier. Allowed bit-widths are 8, 16, 32, 64, 128"#;

pub mod kw {
	use syn::custom_keyword;

	/* Used to parse the #[repr($ty)] attribute on enums */
	custom_keyword!(repr);

	/* Conversions */
	custom_keyword!(simple);
	custom_keyword!(convert);

	/* Keywords used for flags */
	custom_keyword!(en);
	custom_keyword!(de);
	custom_keyword!(serde);
	custom_keyword!(skip);
	custom_keyword!(with);
	custom_keyword!(flatten);
	custom_keyword!(validate);
	custom_keyword!(encrypted);
	custom_keyword!(secret);
	custom_keyword!(compressed);

	/* Keywords used for parsing encryption parameters */
	// Symmetric encryption
	custom_keyword!(key);
	custom_keyword!(iv);
	// Asymmetric encryption
	custom_keyword!(public);
	custom_keyword!(private);

	/* Keywords used for modifiers TARGETS */
	custom_keyword!(num);
	custom_keyword!(size);
	custom_keyword!(variant);

	/* Keywords used for the modifiers themselves */
	// Numerical encodings
	custom_keyword!(fixed);
	custom_keyword!(leb128);
	// Endianness
	custom_keyword!(big_endian);
	custom_keyword!(little_endian);
	// Max size
	custom_keyword!(max);
}

/// The `#[repr($ty)]` attribute
#[derive(Clone)]
pub struct ReprAttribute {
	pub kw: kw::repr,
	pub paren: Paren,
	pub ty: Type,
}

/// Represents a formatting, like `format!("A{}G", "BCDEF");`
#[derive(Clone)]
pub struct Formatting {
	pub format: LitStr,
	pub args: Option<(Token![,], Punctuated<Expr, Token![,]>)>,
}

/// An encryption argument represents a key, or an initialization vector.
/// Formatted as `key: $expr` or `iv: $expr`, etc...
#[derive(Clone)]
pub enum EncryptionArgument {
	Key {
		kw: kw::key,
		colon: Token![:],
		key: Expr,
	},
	Iv {
		kw: kw::iv,
		colon: Token![:],
		iv: Expr,
	},
}

impl EncryptionArgument {
	pub fn is_key(&self) -> bool {
		match self {
			Self::Key { .. } => true,
			_ => false,
		}
	}

	pub fn span(&self) -> Span {
		match self {
			Self::Key { kw, .. } => kw.span,
			Self::Iv { kw, .. } => kw.span,
		}
	}

	pub fn into_expr(self) -> Expr {
		match self {
			Self::Key { key, .. } => key,
			Self::Iv { iv, .. } => iv,
		}
	}
}

/// The constructor for an encryption parameter. Can either be given by an expression,
/// or defined in a string such as `"128-bit AES/CBC"`
#[derive(Clone)]
pub enum EncryptionConstructor {
	Literal(SymmEncryption),
	Expr(Expr),
}

#[derive(Clone)]
pub struct EncryptionData {
	pub ctor: EncryptionConstructor,
	pub args: Option<(Token![,], Punctuated<EncryptionArgument, Token![,]>)>,
}

impl EncryptionData {
	pub fn validate(self) -> syn::Result<(EncryptionConstructor, Option<Expr>, Option<Expr>)> {
		let ctor = self.ctor;

		let mut key = None;
		let mut iv = None;
		if let Some(args) = self.args {
			// This ensures only 1 or 0 ivs, and only 1 or 0 keys are specified.
			// As a side effect, guarantees the length of the argument list is exactly 2
			for arg in args.1 {
				if arg.is_key() {
					if key.is_some() {
						return Err(Error::new(arg.span(), "Key parameter defined twice"));
					}

					key = Some(arg.into_expr());
				} else {
					if iv.is_some() {
						return Err(Error::new(arg.span(), "IV parameter defined twice"));
					}

					iv = Some(arg.into_expr());
				}
			}
		}

		Ok((ctor, key, iv))
	}
}

/// An asymmetric encryption argument represents a public or private key.
/// Formatted as `public: $expr` or `private: $expr`, etc...
#[derive(Clone)]
pub enum SecretArgument {
	Public {
		kw: kw::public,
		colon: Token![:],
		public: Expr,
	},
	Private {
		kw: kw::private,
		colon: Token![:],
		private: Expr,
	},
}

impl SecretArgument {
	pub fn is_public(&self) -> bool {
		match self {
			Self::Public { .. } => true,
			_ => false,
		}
	}

	pub fn span(&self) -> Span {
		match self {
			Self::Public { kw, .. } => kw.span,
			Self::Private { kw, .. } => kw.span,
		}
	}

	pub fn into_expr(self) -> Expr {
		match self {
			Self::Public { public, .. } => public,
			Self::Private { private, .. } => private,
		}
	}
}

/// The constructor for an asymmetric encryption parameter. Can either be given by an expression,
/// or defined in a string such as `"2048-bit RSA/EBC/PKCS1"`
#[derive(Clone)]
pub enum SecretConstructor {
	Literal(AsymmEncryption),
	Expr(Expr),
}

#[derive(Clone)]
pub struct SecretData {
	pub ctor: SecretConstructor,
	pub args: Option<(Token![,], Punctuated<SecretArgument, Token![,]>)>,
}

impl SecretData {
	pub fn validate(self) -> syn::Result<(SecretConstructor, Option<Expr>, Option<Expr>)> {
		let ctor = self.ctor;

		let mut public = None;
		let mut private = None;
		if let Some(args) = self.args {
			// This ensures only 1 or 0 public keys, and only 1 or 0 private keys are specified.
			// As a side effect, guarantees the length of the argument list is exactly 2
			for arg in args.1 {
				if arg.is_public() {
					if public.is_some() {
						return Err(Error::new(arg.span(), "Public key parameter defined twice"));
					}

					public = Some(arg.into_expr());
				} else {
					if private.is_some() {
						return Err(Error::new(arg.span(), "Private key parameter defined twice"));
					}

					private = Some(arg.into_expr());
				}
			}
		}

		Ok((ctor, public, private))
	}
}

/// The constructor for a compression parameter. Can either be given by an expression,
/// or defined in a string such as `"ZLib/6"`
#[derive(Clone)]
pub enum CompressionConstructor {
	Literal(Compression),
	Expr(Expr),
}

#[derive(Clone)]
pub struct CompressionData {
	pub ctor: CompressionConstructor
}

#[derive(Clone)]
pub enum AsConversion {
	Simple(kw::simple),
	Convert(kw::convert),
}

/// Represents every kind of modifiers that can be applied to a [`ModTarget`].
/// Example: `$target: $modifier, $modifier, ...`
#[derive(Clone)]
pub enum Modifier {
	Fixed {
		kw: kw::fixed,
	},
	Leb128 {
		kw: kw::leb128,
	},
	BigEndian {
		kw: kw::big_endian,
	},
	LittleEndian {
		kw: kw::little_endian,
	},
	Max {
		kw: kw::max,
		eq: Token![=],
		max: Expr,
	},
	BitWidth {
		lit: LitInt,
		width: BitWidth
	},
}

/// Represents every possible target for a [`Modifier`].
#[derive(Clone, Display)]
pub enum ModTarget {
	#[display("num")]
	Num {
		kw: kw::num,
	},
	#[display("size")]
	Size {
		kw: kw::size,
	},
	#[display("variant")]
	Variant {
		kw: kw::variant,
	},
}

impl ModTarget {
	pub fn tier(&self) -> u32 {
		match self {
			Self::Num { .. } => 0,
			Self::Variant { .. } => 1,
			Self::Size { .. } => 2,
		}
	}

	fn peek(input: ParseStream) -> bool {
		input.peek(kw::num) ||
			input.peek(kw::size) ||
			input.peek(kw::variant)
	}

	fn span(&self) -> Span {
		match self {
			ModTarget::Num { kw, .. } => kw.span,
			ModTarget::Size { kw, .. } => kw.span,
			ModTarget::Variant { kw, .. } => kw.span,
		}
	}
}

/// Every possible flag that can be attached to a field or item.
#[derive(Clone)]
pub enum Flag {
	/// All the following flags only apply to the encoding step
	En {
		kw: kw::en,
	},
	/// All the following flags only apply to the decoding step
	De {
		kw: kw::de,
	},
	/// Changes the name of the `ende` crate used in the derivation process
	Crate {
		kw: Token![crate],
		colon: Token![:],
		crate_name: Ident,
	},
	/// The field or item should be encoded/decoded using serde.
	Serde {
		kw: kw::serde,
		crate_name: Option<(Token![:], Ident)>,
	},
	/// The field or item shouldn't be encoded/decoded. During decoding, it will fall back to
	/// the default for that field (can be overridden by the `default` flag).
	Skip {
		kw: kw::skip,
	},
	/// The field should only be encoded if the condition is true. If the condition is false
	/// while decoding, it will fall back to the default for that field
	/// (can be overridden by the `default` flag).
	If {
		kw: Token![if],
		colon: Token![:],
		expr: Expr,
	},
	/// The default value for a field that cannot be decoded for whatever reason is
	/// `Default::default()`. This flag allows changing that to whatever expression is specified.
	Default {
		kw: Token![default],
		colon: Token![:],
		expr: Expr,
	},
	/// The field should be encoded/decoded using the given expression. Only valid when the scope
	/// is specified.
	With {
		kw: kw::with,
		colon: Token![:],
		expr: Expr,
	},
	/// The field should be encoded/decoded as if it was of the given type. It should then be
	/// converted back to the appropriate type
	As {
		kw: Token![as],
		method: AsConversion,
		colon: Token![:],
		ty: Type,
	},
	/// Changes the flatten state variable. See the documentation for `Encode` and `Decode` for
	/// more info.
	Flatten {
		kw: kw::flatten,
		expr: Option<(Token![:], Expr)>,
	},
	/// Performs a check before encoding and after decoding a field. If the check fails, an error
	/// is returned. Allows specifying a custom error message with fmt arguments.
	Validate {
		kw: kw::validate,
		colon: Token![:],
		expr: Expr,
		fmt: Option<(Token![,], Formatting)>,
	},
	/// The field should be encoded/decoded using encryption.
	Encrypted {
		kw: kw::encrypted,
		data: Option<(Token![:], EncryptionData)>,
	},
	/// The field is a block of data encrypted using asymmetric encryption.
	Secret {
		kw: kw::secret,
		data: Option<(Token![:], SecretData)>,
	},
	/// The field should be encoded/decoded using compression.
	Compressed {
		kw: kw::compressed,
		data: Option<(Token![:], CompressionData)>,
	},
	/// The field has a number of modifiers attached to it.
	Modifiers {
		target: ModTarget,
		colon: Token![:],
		modifiers: Punctuated<Modifier, Token![,]>,
	},
}

impl Flag {
	pub fn span(&self) -> Span {
		match self {
			Flag::En { kw, .. } => kw.span,
			Flag::De { kw, .. } => kw.span,
			Flag::Crate { kw, .. } => kw.span,
			Flag::Serde { kw, .. } => kw.span,
			Flag::Skip { kw, .. } => kw.span,
			Flag::If { kw, .. } => kw.span,
			Flag::Default { kw, .. } => kw.span,
			Flag::With { kw, .. } => kw.span,
			Flag::As { kw, .. } => kw.span,
			Flag::Flatten { kw, .. } => kw.span,
			Flag::Validate { kw, .. } => kw.span,
			Flag::Encrypted { kw, .. } => kw.span,
			Flag::Secret { kw, .. } => kw.span,
			Flag::Compressed { kw, .. } => kw.span,
			Flag::Modifiers { target, .. } => target.span(),
		}
	}
}

/// Represents the `#[ende(... stuff ...)]` attribute
#[derive(Clone)]
pub struct EndeAttribute {
	pub flags: Punctuated<Flag, Token![;]>,
}

impl EndeAttribute {
	pub fn scope(&self) -> Scope {
		self.flags.iter().nth(0).map(|x| match x {
			Flag::En { .. } => Scope::Encode,
			Flag::De { .. } => Scope::Decode,
			_ => Scope::Both,
		}).unwrap_or(Scope::Both)
	}
}

/* ┌─────────────────────────────────────────────────────────────────────┐
** │The following section contains the parsing code for the above structs│
** └─────────────────────────────────────────────────────────────────────┘*/

impl Parse for ReprAttribute {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let inside;
		Ok(Self {
			kw: input.parse()?,
			paren: parenthesized!(inside in input),
			ty: inside.parse()?,
		})
	}
}

impl Parse for Formatting {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(Self {
			format: input.parse()?,
			args: if input.peek(Token![,]) {Some((
				input.parse()?,
				Punctuated::parse_separated_nonempty(input)?,
			))} else { None },
		})
	}
}

impl Parse for EncryptionArgument {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(kw::key) {
			Ok(Self::Key {
				kw: input.parse()?,
				colon: input.parse()?,
				key: input.parse()?,
			})
		} else if input.peek(kw::iv) {
			Ok(Self::Iv {
				kw: input.parse()?,
				colon: input.parse()?,
				iv: input.parse()?,
			})
		} else {
			Err(Error::new(input.span(), ENCRYPTION_USAGE))
		}
	}
}

impl Parse for EncryptionConstructor {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(LitStr) {
			let lit: LitStr = input.parse()?;
			let encryption = SymmEncryption::from_str(&lit.value())
				.map_err(|x| Error::new(lit.span(), x))?;

			Ok(Self::Literal(encryption))
		} else {
			Ok(Self::Expr(
				input.parse()?
			))
		}
	}
}

impl Parse for EncryptionData {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(Self {
			ctor: input.parse()?,
			args: if input.peek(Token![,]) {Some((
				input.parse()?,
				Punctuated::parse_separated_nonempty(input)?,
			))} else { None },
		})
	}
}

impl Parse for SecretArgument {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(kw::public) {
			Ok(Self::Public {
				kw: input.parse()?,
				colon: input.parse()?,
				public: input.parse()?,
			})
		} else if input.peek(kw::private) {
			Ok(Self::Private {
				kw: input.parse()?,
				colon: input.parse()?,
				private: input.parse()?,
			})
		} else {
			Err(Error::new(input.span(), SECRET_USAGE))
		}
	}
}

impl Parse for SecretConstructor {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(LitStr) {
			let lit: LitStr = input.parse()?;
			let encryption = AsymmEncryption::from_str(&lit.value())
				.map_err(|x| Error::new(lit.span(), x))?;

			Ok(Self::Literal(encryption))
		} else {
			Ok(Self::Expr(
				input.parse()?
			))
		}
	}
}

impl Parse for SecretData {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(Self {
			ctor: input.parse()?,
			args: if input.peek(Token![,]) {Some((
				input.parse()?,
				Punctuated::parse_separated_nonempty(input)?,
			))} else { None },
		})
	}
}

impl Parse for CompressionConstructor {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(LitStr) {
			let lit: LitStr = input.parse()?;
			let encryption = Compression::from_str(&lit.value())
				.map_err(|x| Error::new(lit.span(), x))?;

			Ok(Self::Literal(encryption))
		} else {
			Ok(Self::Expr(
				input.parse()?
			))
		}
	}
}

impl Parse for CompressionData {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(Self {
			ctor: input.parse()?,
		})
	}
}

impl Parse for AsConversion {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(kw::simple) {
			Ok(Self::Simple(input.parse()?))
		} else if input.peek(kw::convert) {
			Ok(Self::Convert(input.parse()?))
		} else {
			return Err(Error::new(input.span(), r#"Allowed "as" flag parameters:
			simple (performs conversion through the as keyword),
			convert (performs conversion through From and Into)"#))
		}
	}
}

impl Parse for Modifier {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(kw::fixed) {
			Ok(Self::Fixed {
				kw: input.parse()?,
			})
		} else if input.peek(kw::leb128) {
			Ok(Self::Leb128 {
				kw: input.parse()?,
			})
		} else if input.peek(kw::big_endian) {
			Ok(Self::BigEndian {
				kw: input.parse()?,
			})
		} else if input.peek(kw::little_endian) {
			Ok(Self::LittleEndian {
				kw: input.parse()?,
			})
		} else if input.peek(kw::max) {
			Ok(Self::Max {
				kw: input.parse()?,
				eq: input.parse()?,
				max: input.parse()?,
			})
		} else if input.peek(LitInt) {
			let lit: LitInt = input.parse()?;
			let width = match lit.base10_parse::<u8>()? {
				8 => BitWidth::Bit8,
				16 => BitWidth::Bit16,
				32 => BitWidth::Bit32,
				64 => BitWidth::Bit64,
				128 => BitWidth::Bit128,
				_ => return Err(Error::new(lit.span(), BITWIDTH_USAGE))
			};

			Ok(Self::BitWidth {
				lit,
				width,
			})
		} else {
			Err(Error::new(input.span(), MODIFIER_USAGE))
		}
	}
}

impl Parse for ModTarget {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(kw::num) {
			Ok(Self::Num {
				kw: input.parse()?,
			})
		} else if input.peek(kw::size) {
			Ok(Self::Size {
				kw: input.parse()?,
			})
		} else if input.peek(kw::variant) {
			Ok(Self::Variant {
				kw: input.parse()?,
			})
		} else {
			// We peek before parsing a Target, so this should be unreachable
			unreachable!()
		}
	}
}

impl Parse for Flag {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(kw::en) {
			Ok(Self::En {
				kw: input.parse()?,
			})
		} else if input.peek(kw::de) {
			Ok(Self::De {
				kw: input.parse()?,
			})
		} else if input.peek(kw::serde) {
			Ok(Self::Serde {
				kw: input.parse()?,
				crate_name: if input.peek(Token![:]) {Some((
					input.parse()?,
					input.parse()?,
				))} else { None },
			})
		} else if input.peek(Token![crate]) {
			Ok(Self::Crate {
				kw: input.parse()?,
				colon: input.parse()?,
				crate_name: input.parse()?,
			})
		} else if input.peek(kw::skip) {
			Ok(Self::Skip {
				kw: input.parse()?,
			})
		} else if input.peek(Token![if]) {
			Ok(Self::If {
				kw: input.parse()?,
				colon: input.parse()?,
				expr: input.parse()?,
			})
		} else if input.peek(Token![default]) {
			Ok(Self::Default {
				kw: input.parse()?,
				colon: input.parse()?,
				expr: input.parse()?,
			})
		} else if input.peek(kw::with) {
			Ok(Self::With {
				kw: input.parse()?,
				colon: input.parse()?,
				expr: input.parse()?,
			})
		} else if input.peek(Token![as]) {
			Ok(Self::As {
				kw: input.parse()?,
				method: input.parse()?,
				colon: input.parse()?,
				ty: input.parse()?,
			})
		} else if input.peek(kw::flatten) {
			Ok(Self::Flatten {
				kw: input.parse()?,
				expr: if input.peek(Token![:]) {Some((
					input.parse()?,
					input.parse()?,
				))} else { None },
			})
		} else if input.peek(kw::validate) {
			Ok(Self::Validate {
				kw: input.parse()?,
				colon: input.parse()?,
				expr: input.parse()?,
				fmt: if input.peek(Token![,]) {Some((
					input.parse()?,
					input.parse()?,
				))} else { None },
			})
		} else if input.peek(kw::encrypted) {
			Ok(Self::Encrypted {
				kw: input.parse()?,
				data: if input.peek(Token![:]) {Some((
					input.parse()?,
					input.parse()?,
				))} else { None },
			})
		} else if input.peek(kw::secret) {
			Ok(Self::Secret {
				kw: input.parse()?,
				data: if input.peek(Token![:]) {Some((
					input.parse()?,
					input.parse()?,
				))} else { None },
			})
		} else if input.peek(kw::compressed) {
			Ok(Self::Compressed {
				kw: input.parse()?,
				data: if input.peek(Token![:]) {Some((
					input.parse()?,
					input.parse()?,
				))} else { None },
			})
		} else if ModTarget::peek(input) {
			Ok(Self::Modifiers {
				target: input.parse()?,
				colon: input.parse()?,
				modifiers: Punctuated::parse_separated_nonempty(input)?,
			})
		} else {
			Err(Error::new(input.span(), FLAGS_USAGE))
		}
	}
}

impl Parse for EndeAttribute {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		Ok(Self {
			flags: Punctuated::parse_terminated(input)?
		})
	}
}