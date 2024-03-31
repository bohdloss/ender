mod parse;
mod enums;
mod ctxt;
mod generator;
mod flags;

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{DeriveInput, parse_macro_input};
use crate::ctxt::{Ctxt, Target};

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

/// Helper macro to derive ende::Encode for a `struct` or `enum`<br>
/// This macro supports a series of helper flags to aid customization<br>
/// <br>
/// All flags follow the following format:<br>
/// `#[ende(flag1; flag2; flag2; ...)]`<br>
/// <br>
/// The 2 special flags `en` and `de` can be used only as the first flag to indicate that
/// all the following flags only apply to the encoding process (`en`) or the
/// decoding process (`de`).<br>
/// <br>
/// If neither of those flags are specified, then it is assumed that the following
/// flags apply to both encoding and decoding<br>
/// <br>
/// The flags currently implemented are split into 3 groups:<br>
/// <br>
/// Modifiers temporarily change certain properties of the encoder and can be applied
/// to Fields or Items (the whole struct or enum).<br>
/// * `#[ende(target: big_endian, little_endian)]` - `target` can be `num`, `size`, `variant`
/// * `#[ende(target: fixed, leb128)]` - `target` can be `num`, `size`, `variant`
/// * `#[ende(target: 8, 16, 32, 64, 128)]` - `target` can be `size`, `variant`
/// * `#[ende(target: max_size = usize)]` - `target` can only be `size`<br>
/// <br>
/// Example:
/// ```ignore
/// use ende::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// #[ende(variant: little_endian, fixed, 32]
/// enum MyEnum {
///     VariantA {
///         #[ende(size: big_endian, fixed, 16, max_size = 100)]
///         field: String,
///     },
///     VariantB {
///         #[ende(num: little_endian, leb128)]
///         field: u64,
/// 	},
/// }
/// ```
/// Stream-Modifiers temporarily change the underlying reader/writer, and can be applied
/// to Fields or Items.<br>
/// * `#[ende(compressed: "{format}/{level}")]`
/// * `#[ende(encrypted: "{key_size}-bit {cipher}/{mode}/{padding}", iv: iv, key: key)]` -
/// If either the encryption method, the key or the iv are not specified,
/// they will be inferred from the [CryptoState][`ende::encryption::CryptoState`]
/// * `#[ende(secret: "{key_size}-bit {cipher}/{mode}/{padding}", public: public_key, private: private_key)]` -
/// While not being a proper stream modifier, it's worth listing this other method of encryption
/// which uses asymmetric encryption (or public-key encryption) like rsa to encode and decode a
/// block with padding. The field type must be `Vec<u8>` because that's what the
/// encryption and decryption function accepts. Just like the `encrypted` flag, if  either the
/// encryption method, the public key, or the private key are not specified, they will be inferred
/// from the [CryptoState][`ende::encryption::CryptoState`]
/// <br>
/// Example:
/// ```ignore
/// use ende::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// #[ende(compressed = "ZLib/6")]
/// struct MyStruct {
///     secret_key: Vec<u8>,
///     iv: Vec<u8>,
///     #[ende(encrypted: "128-bit AES/CBC", key: secret_key, iv: iv)]
///     super_secret_data: Vec<u8>,
///     super_extra_secret_rsa_key: Vec<u8>,
///     #[ende(secret: "2048-bit RSA/ECB/PKCS1", private: super_extra_secret_rsa_key)]
///     even_more_secret_data: RsaBlock,
/// }
/// ```
/// Helpers change how a field or item is encoded/decoded.<br>
/// * `#[ende(crate: $crate)]` - Overwrites the default crate name which is assumed to be `ende`
/// * `#[ende(serde: $crate)]` - Field will be serialized/deserialized with a serde compatibility layer.
/// Optionally, the serde crate name can be specified (useful if the serde crate was re-exported under
/// another name)
/// * `#[ende(with: $expr)]` - Uses the given expression to encode/decode a field.
/// Must be used with `en` or `de`.
/// * `#[ende(as simple/convert: $ty)]` - Converts the value of the field to `$ty` before encoding it
/// and back to the original field type after decoding it. When the parameter is set to `simple`,
/// the conversion is done through the `as` keyword. When the parameter is set to `convert`, the
/// conversion is done through the `From` and `Into` traits.
/// * `#[ende(if: $expr)]` - The field will only be encoded/decoded if the given expression
/// evaluates to true, otherwise the default value is computed
/// * `#[ende(default: $expr]` - Overrides the default fallback for when a value can't be
/// deserialized (`Default::default()`)
/// * `#[ende(skip)]` - Will not encode/decode this field.
/// When decoding, computes the default value
/// * `#[ende(validate: $expr, $format_string, $arg1, $arg2, $arg3, ...)]` - Before encoding/after decoding, returns an error if the
/// expression evaluates to false. The error message will use the given formatting (if present)
/// * `#[ende(flatten: $expr)]` - Indicates that the length of the given field (for example
/// a Vec or HashMap) doesn't need to be encoded/decoded, because it is known from the context.
/// Can also be used with an `Option` in conjunction with the `if` flag and without the `$expr`
/// to indicate that the presence of an optional value is known from the context.<br>
/// <br>
/// Example:
/// ```ignore
/// use ende::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// struct MyStruct {
///     /// Will come in handy later
///     name_present: bool,
///     /// Has Serialize/Deserialize implementations.
///     /// Also, we know that it will always be 16 bytes
///     #[ende(serde; flatten: 16)]
///     uuid: Uuid,
///     /// Just the string version of the uuid.
///     /// We can skip Encoding, and Decode it from the uuid
///     #[ende(en; skip)]
///     #[ende(de; with: uuid.to_string())]
///     uuid_string: String,
///     /// We know whether this is present from the context.
///     #[ende(flatten; if: *name_present)]
///     name: Option<String>,
///     /// Only present if the name is also present, but we want to provide a custom default!
///     #[ende(default: String::from("Smith"); if: *name_present)]
///     surname: String,
///     /// No-one allowed before 18!
///     #[ende(validate: *age >= 18, "User is too young: {}", age)]
///     age: u32
/// }
/// ```
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

	quote!(
		#[automatically_derived]
		impl #impl_generics #crate_name::Encode for #item_name #ty_generics #where_clause {
			fn encode<#encoder_generic: std::io::Write>(&self, #encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<()> {
				#body
			}
		}
	).into()
}

/// Helper macro to derive ende::Encode for a `struct` or `enum`<br>
/// This macro supports a series of helper flags to aid customization<br>
/// <br>
/// All flags follow the following format:<br>
/// `#[ende(flag1; flag2; flag2; ...)]`<br>
/// <br>
/// The 2 special flags `en` and `de` can be used only as the first flag to indicate that
/// all the following flags only apply to the encoding process (`en`) or the
/// decoding process (`de`).<br>
/// <br>
/// If neither of those flags are specified, then it is assumed that the following
/// flags apply to both encoding and decoding<br>
/// <br>
/// The flags currently implemented are split into 3 groups:<br>
/// <br>
/// Modifiers temporarily change certain properties of the encoder and can be applied
/// to Fields or Items (the whole struct or enum).<br>
/// * `#[ende(target: big_endian, little_endian)]` - `target` can be `num`, `size`, `variant`
/// * `#[ende(target: fixed, leb128)]` - `target` can be `num`, `size`, `variant`
/// * `#[ende(target: 8, 16, 32, 64, 128)]` - `target` can be `size`, `variant`
/// * `#[ende(target: max_size = usize)]` - `target` can only be `size`<br>
/// <br>
/// Example:
/// ```ignore
/// use ende::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// #[ende(variant: little_endian, fixed, 32]
/// enum MyEnum {
///     VariantA {
///         #[ende(size: big_endian, fixed, 16, max_size = 100)]
///         field: String,
///     },
///     VariantB {
///         #[ende(num: little_endian, leb128)]
///         field: u64,
/// 	},
/// }
/// ```
/// Stream-Modifiers temporarily change the underlying reader/writer, and can be applied
/// to Fields or Items.<br>
/// * `#[ende(compressed: "{format}/{level}")]`
/// * `#[ende(encrypted: "{key_size}-bit {cipher}/{mode}/{padding}", iv: iv, key: key)]` -
/// If either the encryption method, the key or the iv are not specified,
/// they will be inferred from the [CryptoState][`ende::encryption::CryptoState`]
/// * `#[ende(secret: "{key_size}-bit {cipher}/{mode}/{padding}", public: public_key, private: private_key)]` -
/// While not being a proper stream modifier, it's worth listing this other method of encryption
/// which uses asymmetric encryption (or public-key encryption) like rsa to encode and decode a
/// block with padding. The field type must be `Vec<u8>` because that's what the
/// encryption and decryption function accepts. Just like the `encrypted` flag, if  either the
/// encryption method, the public key, or the private key are not specified, they will be inferred
/// from the [CryptoState][`ende::encryption::CryptoState`]
/// <br>
/// Example:
/// ```ignore
/// use ende::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// #[ende(compressed = "ZLib/6")]
/// struct MyStruct {
///     secret_key: Vec<u8>,
///     iv: Vec<u8>,
///     #[ende(encrypted: "128-bit AES/CBC", key: secret_key, iv: iv)]
///     super_secret_data: Vec<u8>,
///     super_extra_secret_rsa_key: Vec<u8>,
///     #[ende(secret: "2048-bit RSA/ECB/PKCS1", private: super_extra_secret_rsa_key)]
///     even_more_secret_data: RsaBlock,
/// }
/// ```
/// Helpers change how a field or item is encoded/decoded.<br>
/// * `#[ende(crate: $crate)]` - Overwrites the default crate name which is assumed to be `ende`
/// * `#[ende(serde: $crate)]` - Field will be serialized/deserialized with a serde compatibility layer.
/// Optionally, the serde crate name can be specified (useful if the serde crate was re-exported under
/// another name)
/// * `#[ende(with: $expr)]` - Uses the given expression to encode/decode a field.
/// Must be used with `en` or `de`.
/// * `#[ende(as simple/convert: $ty)]` - Converts the value of the field to `$ty` before encoding it
/// and back to the original field type after decoding it. When the parameter is set to `simple`,
/// the conversion is done through the `as` keyword. When the parameter is set to `convert`, the
/// conversion is done through the `From` and `Into` traits.
/// * `#[ende(if: $expr)]` - The field will only be encoded/decoded if the given expression
/// evaluates to true, otherwise the default value is computed
/// * `#[ende(default: $expr]` - Overrides the default fallback for when a value can't be
/// deserialized (`Default::default()`)
/// * `#[ende(skip)]` - Will not encode/decode this field.
/// When decoding, computes the default value
/// * `#[ende(validate: $expr, $format_string, $arg1, $arg2, $arg3, ...)]` - Before encoding/after decoding, returns an error if the
/// expression evaluates to false. The error message will use the given formatting (if present)
/// * `#[ende(flatten: $expr)]` - Indicates that the length of the given field (for example
/// a Vec or HashMap) doesn't need to be encoded/decoded, because it is known from the context.
/// Can also be used with an `Option` in conjunction with the `if` flag and without the `$expr`
/// to indicate that the presence of an optional value is known from the context.<br>
/// <br>
/// Example:
/// ```ignore
/// use ende::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// struct MyStruct {
///     /// Will come in handy later
///     name_present: bool,
///     /// Has Serialize/Deserialize implementations.
///     /// Also, we know that it will always be 16 bytes
///     #[ende(serde; flatten: 16)]
///     uuid: Uuid,
///     /// Just the string version of the uuid.
///     /// We can skip Encoding, and Decode it from the uuid
///     #[ende(en; skip)]
///     #[ende(de; with: uuid.to_string())]
///     uuid_string: String,
///     /// We know whether this is present from the context.
///     #[ende(flatten; if: *name_present)]
///     name: Option<String>,
///     /// Only present if the name is also present, but we want to provide a custom default!
///     #[ende(default: String::from("Smith"); if: *name_present)]
///     surname: String,
///     /// No-one allowed before 18!
///     #[ende(validate: *age >= 18, "User is too young: {}", age)]
///     age: u32
/// }
/// ```
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

	quote!(
		#[automatically_derived]
		impl #impl_generics #crate_name::Decode for #item_name #ty_generics #where_clause {
			fn decode<#encoder_generic: std::io::Read>(#encoder: &mut #crate_name::Encoder<#encoder_generic>) -> #crate_name::EncodingResult<Self> {
				#body
			}
		}
	).into()
}