#![cfg_attr(doc_cfg, feature(doc_cfg))]
#![cfg_attr(feature = "unstable", feature(never_type))]
#![cfg_attr(feature = "unstable", feature(error_in_core))]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(test)]
mod test;

#[cfg(feature = "serde")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "serde")))]
pub mod serde;

pub mod impls;
pub mod facade;

use core::any::Any;
use core::fmt;
use core::fmt::Debug;
use core::hash::Hash;
use core::mem::replace;
use embedded_io::{Error, ErrorKind, ReadExactError};

pub use embedded_io::{Write, Read};

/// Helper macros to derive [`Encode`] and [`Decode`] for a `struct` or `enum`.<br>
/// This macro supports a series of helper flags to aid customization<br>
/// <br>
/// All flags follow the following format:<br>
/// `#[ende(flag1; flag2; flag2; ...)]`<br>
/// <br>
/// The 2 special flags `en` and `de`, called Scope flags, can be used only at the beginning
/// of the list to indicate that all the flags in the attribute declaration only apply to the
/// encoding process (`en`) or the decoding process (`de`).<br>
/// <br>
/// If neither of those flags are specified, then it is assumed that all the flags in the
/// attribute declaration apply to both encoding and decoding<br>
/// <br>
/// The flags currently implemented are split into 5 groups:<br>
/// <br>
/// # 1. Setting Modifiers
/// Setting-Modifier flags temporarily change certain settings of the encoder and can be applied
/// to Fields or Items (the whole struct or enum).<br>
/// Multiple can be specified at the same time, as long as they don't overlap.<br>
/// * `$target: big_endian, little_endian` - `$target` can be `num`, `size`, `variant`, `string`
/// * `$target: fixed, leb128, protobuf_wasteful, protobuf_zz` - `$target` can be `num`, `size`, `variant`
/// * `$target: 8, 16, 32, 64, 128` - `$target` can be `size`, `variant`
/// * `$target: max = $expr` - `$target` can only be `size`<br>
/// * `$target: ascii, utf_8, utf_16, utf_32` - `$target` can only be `string`<br>
/// * `$target: nul_term, len_prefix` - `$target` can only be `string`<br>
/// <br>
/// ### Example:
/// ```no_run
/// use ende::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// #[ende(crate: ende)]
/// /// The variants of this enum will be encoded in the little endian ordering,
/// /// using a fixed numerical encoding and a 32-bit width.
/// #[ende(variant: little_endian, fixed, 32)]
/// enum MyEnum {
///     VariantA {
///         /// The length of this String will be encoded using big endian ordering,
///         /// fixed numerical encoding and 16-bit width, with a max length of 100
///         #[ende(size: big_endian, fixed, 16, max = 100)]
///         field: String,
///         /// Encode this String to utf16 with a length prefix
///         #[ende(string: utf_16, len_prefix, big_endian)]
///         utf_16: String,
///         /// Encode this String as a null-terminated ascii string
///         #[ende(string: ascii, nul_term)]
///         ascii: String,
///     },
///     VariantB {
///         /// This number will be encoded using little endian ordering, and the
///         /// leb128 [NumEncoding][`ende::NumEncoding`]
///         #[ende(num: little_endian, leb128)]
///         field: u64,
///         #[ende(num: protobuf_zz)]
///         zigzag: i128,
/// 	},
/// }
/// ```
/// # 2. Stream Modifiers
/// Stream-Modifier flags temporarily change the underlying reader/writer, and can be applied
/// to Fields or Items.<br>
/// Note that the order in which stream modifiers are declared is very important:<br>
/// They are applied in the declaration order during encoding, but in the reverse order during
/// decoding, for consistency. However, the item-level modifiers take priority over the field-level
/// modifiers (see [example](#ambiguous-example)).<br>
/// * `redir: $path(...)` - Currently the only supported stream modifier, but more may
/// be added in the future. Uses the given path to find an encoding/decoding function which
/// alters the writer/reader and passes a modified encoder to a closure.<br>
/// This can be used to implement encryption, compression and other transformations of the
/// underlying stream, or even redirections to another stream.<br>
/// The implementation of a redir function can be non-trivial and the signature can be
/// cryptic, therefore it is recommended you only create your own stream transforms if you know what
/// you're doing, in which case you should take a look at the [facade module][`facade`].<br>
///     * If the scope is Encode, the path must be callable as `encode`.<br>
///     * If the scope is Decode, the path must be callable as `decode`.<br>
///     * If no scope is specified, the path must point to a module with encoding and decoding functions
/// with the same signatures as above.
/// ### Example:
/// ```no_run
/// use ende::{Encode, Decode};
/// use ende::facade::fake::*;
/// #[derive(Encode, Decode)]
/// #[ende(crate: ende)]
/// struct MyStruct {
///     secret_key: Vec<u8>,
///     iv: Vec<u8>,
///     /// While **encoding**, this field is compressed -> encrypted.
///     /// While **decoding**, this field is decrypted -> decompressed.
///     #[ende(redir: gzip(9))]
///     #[ende(redir: aes(iv, secret_key))]
///     super_secret_data: Vec<u8>,
/// }
/// ```
/// ### Ambiguous example:
/// ```no_run
/// use ende::{Encode, Decode};
/// use ende::facade::fake::*;
/// #[derive(Encode, Decode)]
/// #[ende(crate: ende)]
/// /// Because of the priority rules of items over fields, this is ambiguous, see below
/// #[ende(redir: gzip(9))]
/// struct MyStruct {
///     /// While **encoding**, this field is encrypted -> compressed.
///     /// While **decoding**, this field is decompressed -> decrypted.
///     ///
///     /// Because the "compressed" flag is declared before the "encrypted" flag, one might
///     /// think they are applied in that order. However, since the item-level flag takes priority,
///     /// it is applied *after* the field flag.
///     ///
///     /// According to your needs, this might not be what you want, so be careful when mixing
///     /// item-level and field-level stream modifiers.
///     #[ende(redir: aes(&[], &[]))]
///     super_secret_data: Vec<u8>,
/// }
/// ```
/// # 3. Function Modifiers
/// Function-Modifier flags change the function that is used to encode/decode a field or item.
/// * `serde: $crate` - Field will be serialized/deserialized with a serde compatibility layer.<br>
/// Optionally, the serde crate name can be specified (useful if the serde crate was re-exported under
/// another name).
/// * `with: $path` - Uses the given path to find the encoding/decoding function.<br>
///     * If the scope is Encode, the path must be callable as
/// `fn<T: Write>(&V, &mut ende::Encoder<T>) -> EncodingResult<()>`
/// where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
///     * If the scope is Decode, the path must be callable as
/// `fn<T: Read>(&mut ende::Encoder<T>) -> EncodingResult<V>`
/// where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
///     * If no scope is specified, the path must point to a module with encoding and decoding functions
/// with the same signatures as above.
/// ### Example:
/// ```no_run
/// use ende::{Encode, Decode};
/// use ende::facade::fake::rsa;
/// use uuid::Uuid;
///
/// #[derive(Encode, Decode)]
/// #[ende(crate: ende)]
/// struct Friends {
/// 	/// Has Serialize/Deserialize implementations, but no Encode/Decode implementations.
///     /// A perfect fit for integrating with serde!
///     #[ende(serde)]
///     uuid: Uuid,
///     /// Here we demonstrate how the with flag changes based on whether or not a scope
///     /// is declared. This:
///     #[ende(with: person_encoder)]
///     friend1: person_encoder::Person,
///     /// ...is equivalent to this!
///     #[ende(en; with: person_encoder::encode)]
///     #[ende(de; with: person_encoder::decode)]
///     friend2: person_encoder::Person,
///     /// Not the smartest way to store a private key!
///     private_key: Vec<u8>,
///     public_key: Vec<u8>,
///     /// This block of data will be encrypted before being encoded using the public key,
///     /// and decrypted after being decoded using the private key.
///     #[ende(with: rsa(public_key, private_key))]
///     even_more_secret_data: Vec<u8>,
/// }
///
/// mod person_encoder {
///     use ende::{Write, Read};
/// 	use ende::{Encoder, EncodingResult, Encode};
///
///     pub struct Person {
///         name: String,
///         surname: String,
///         age: u32,
///     }
///
/// 	pub fn encode<T: Write>(person: &Person, encoder: &mut Encoder<T>) -> EncodingResult<(), T::Error> {
///         person.name.encode(encoder)?;
///         person.surname.encode(encoder)?;
///         person.age.encode(encoder)?;
///         Ok(())
/// 	}
///
/// 	pub fn decode<T: Read>(encoder: &mut Encoder<T>) -> EncodingResult<Person, T::Error> {
///         Ok(Person {
///             name: encoder.decode_value()?,
///             surname: encoder.decode_value()?,
///             age: encoder.decode_value()?,
/// 		})
/// 	}
/// }
/// ```
/// # 4. Type Modifiers
/// Type-Modifier flags change the type of the value that's encoded, and change it back after
/// decoding it.<br>
/// * `as: $ty` - Converts the value of the field to `$ty` before encoding it
/// and back to the original field type after decoding it.<br>
/// The conversion is done through the `as` keyword.
/// * `into: $ty` - Converts the value of the field to `$ty` before encoding it
/// and back to the original field type after decoding it.<br>
/// The conversion is done through the `From` and `Into` traits.
/// ### Example:
/// ```no_run
/// use ende::{Encode, Decode};
///
/// #[derive(Encode, Decode)]
/// #[ende(crate: ende)]
/// struct Mountain {
///     /// Height is encoded as a `u16`, then decoded back to a `f64`.
///     /// These operations are performed using the `as` keyword.
///     #[ende(as: u16)]
///     height: f64,
///     /// Boulder is encoded as a `BigRock`, then decoded back to a `Boulder`.
///     /// This can be done because `BigRock` implements `From<Boulder>`, and
///     /// `Boulder` implements `From<BigRock>`.
///     ///
///     /// Note: `BigRock` is required to implement `Encode` and `Decode`,
///     /// but `Boulder` is not.
///     #[ende(into: BigRock)]
///     boulder: Boulder,
/// }
///
/// #[derive(Encode, Decode)]
/// #[ende(crate: ende)]
/// struct BigRock {
///     weight: f64
/// }
///
/// impl From<Boulder> for BigRock {
///     fn from(value: Boulder) -> Self {
/// 		Self {
///             weight: value.weight
/// 		}
///     }
/// }
///
/// #[derive(Clone)]
/// struct Boulder {
///     weight: f64,
///     radius: f64
/// }
///
/// impl From<BigRock> for Boulder {
///     fn from(value: BigRock) -> Self {
/// 		Self {
/// 			weight: value.weight,
///             radius: 1.0
/// 		}
/// 	}
/// }
/// ```
/// # 5. Helpers
/// Helper flags change certain parameters or add conditions for when a field
/// or item should be encoded/decoded.<br>
/// * `crate: $crate` - Overwrites the default crate name which is assumed to be `ende`.
/// Can only be applied to items.
/// * `if: $expr` - The field will only be encoded/decoded if the given expression
/// evaluates to true, otherwise the default value is computed
/// * `default: $expr` - Overrides the default fallback for when a value can't be
/// deserialized (`Default::default()`)
/// * `skip` - Will not encode/decode this field.
/// When decoding, computes the default value
/// * `validate: $expr, $format_string, $arg1, $arg2, $arg3, ...` - Before encoding/after decoding, returns an error if the
/// expression evaluates to false. The error message will use the given formatting (if present)
/// * `flatten: $expr` - Indicates that the length of the given field (for example
/// a Vec or HashMap) doesn't need to be encoded/decoded, because it is known from the context.
/// Can also be used with an `Option` in conjunction with the `if` flag and without the `$expr`
/// to indicate that the presence of an optional value is known from the context.<br>
/// <br>
/// ### Example:
/// ```no_run
/// use ende::{Encode, Decode};
/// use uuid::Uuid;
///
/// // Hehe >:3
/// extern crate ende as enderman;
///
/// #[derive(Encode, Decode)]
/// /// We specify the name of the re-exported ende crate.
/// #[ende(crate: enderman)]
/// struct PersonEntry {
///     /// Will come in handy later
///     name_present: bool,
///     /// Similar to the previous example, but with the addition of the flatten flag!
///     /// We know a Uuid is always 16 bytes long, so we omit writing/reading that data.
///     #[ende(serde; flatten: 16)]
///     uuid: Uuid,
/// 	/// Just the string version of the uuid, not present in the binary data.
///     /// Skip the Encoding step, and Decode it from the uuid.
///     #[ende(skip; default: uuid.to_string())]
/// 	uuid_string: String,
///     /// We know whether this is present from the context, therefore we don't write whether
///     /// the optional value is present, and when reading we assume it is.
///     /// Since the "if" flag is also present, the field will only be decoded if the expression
///     /// evaluates to true, making the previous operation safe
/// 	/// (no risk of decoding garbage data)
///     #[ende(flatten: some; if: *name_present)]
///     name: Option<String>,
///     /// Only present if the name is also present, but we want to provide a custom default!
///     #[ende(default: String::from("Smith"); if: *name_present)]
///     surname: String,
///     /// No-one allowed before 18!
///     #[ende(validate: *age >= 18, "User is too young: {}", age)]
///     age: u32,
///     /// This is temporary data, we don't care about including it in the binary format.
///     #[ende(skip; default: 100)]
///     health: u64,
/// }
/// ```
#[cfg(feature = "derive")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "derive")))]
pub use ende_derive::{Encode, Decode};

use parse_display::Display;

/// Encodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the given context.
pub fn encode_with<T: Write, V: Encode>(writer: T, context: Context, value: V) -> EncodingResult<(), T::Error> {
	let mut stream = Encoder::new(writer, context);
	value.encode(&mut stream)
}

/// Decodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the given context.
pub fn decode_with<T: Read, V: Decode>(reader: T, context: Context) -> EncodingResult<V, T::Error> {
	let mut stream = Encoder::new(reader, context);
	V::decode(&mut stream)
}

/// Controls the endianness of a numerical value. Endianness is just
/// the order in which the value's bytes are written.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
pub enum Endianness {
	/// Least significant byte first
	LittleEndian,
	/// Most significant byte first
	BigEndian
}

/// Controls the encoding of a numerical value. For instance, controls whether the numbers
/// are compressed through a var-int format or if the entire length of their value is encoded.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
pub enum NumEncoding {
	/// The value's bits are encoded as-is according to the [`Endianness`].
	Fixed,
	/// The value's bits are encoded according to the [ULEB128](https://en.wikipedia.org/wiki/LEB128#Unsigned_LEB128)
	/// (Little Endian Base 128) standard if unsigned, or [LEB128](https://en.wikipedia.org/wiki/LEB128#Signed_LEB128)
	/// standard if signed.<br>As the name suggests, the bytes are encoded in little endian order,
	/// ignoring the [`Endianness`].
	Leb128,
	/// The value's bits are encoded according to
	/// [Protobuf's varint encoding](https://protobuf.dev/programming-guides/encoding/),
	/// where unsigned values are encoded in the same way as [Leb128][`NumEncoding::Leb128`],
	/// and signed values are encoded as a reinterpret-cast of the bits to unsigned,
	/// possibly wasting all the var-int length to encode the leading 1s.<br>
	/// This encoding method is not ideal to encode negative numbers and is provided merely for
	/// compatibility concerns.<br>
	/// The bytes are encoded in little endian order, ignoring the [`Endianness`].
	ProtobufWasteful,
	/// The value's bits are encoded according to
	/// [Protobuf's varint encoding](https://protobuf.dev/programming-guides/encoding/),
	/// where unsigned values are encoded in the same way as [Leb128][`NumEncoding::Leb128`],
	/// and signed values are encoded as an unsigned value with its least significant bit
	/// carrying the sign.<br>
	/// The bytes are encoded in little endian order, ignoring the [`Endianness`].
	ProtobufZigzag,
}

/// How many bits a size or enum variant will occupy in the binary format. If the value
/// contains more bits, they will be trimmed (lost), so change this value with care
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
pub enum BitWidth {
	/// Max 8 bits per value
	Bit8,
	/// Max 16 bits per value
	Bit16,
	/// Max 32 bits per value
	Bit32,
	/// Max 64 bits per value
	Bit64,
	/// Max 128 bits per value
	Bit128
}

/// The encoding method used for strings.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
pub enum StrEncoding {
	/// See [ASCII](https://en.wikipedia.org/wiki/ASCII)
	Ascii,
	/// See [UTF-8](https://en.wikipedia.org/wiki/UTF-8)
	Utf8,
	/// See [UTF-16](https://en.wikipedia.org/wiki/UTF-16)
	Utf16,
	/// See [UTF-32](https://en.wikipedia.org/wiki/UTF-32)
	Utf32,
}

/// The encoding method used for string sizes.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
pub enum StrLenEncoding {
	/// The string is prefixed by its length **in bytes**, encoded according to the [`SizeRepr`].
	LenPrefixed,
	/// The end of the string is indicated by one or more bytes with the value `0`.
	NullTerminated,
}

/// Controls the binary representation of numbers (different from sizes and enum variants).
/// Specifically, controls the [`Endianness`] and [`NumEncoding`].
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("endianness = {endianness}, encoding = {num_encoding}")]
pub struct NumRepr {
	pub endianness: Endianness,
	pub num_encoding: NumEncoding,
}

impl NumRepr {
	/// Returns the default numerical representation: little endian with fixed encoding
	pub const fn new() -> Self {
		Self {
			endianness: Endianness::LittleEndian,
			num_encoding: NumEncoding::Fixed
		}
	}
}

impl Default for NumRepr {
	fn default() -> Self {
		Self::new()
	}
}

/// Controls the binary representation of sizes.
/// Specifically, controls the [`Endianness`], the [`NumEncoding`], the [`BitWidth`],
/// and the greatest encodable/decodable size before an error is thrown
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("endianness = {endianness} , encoding = {num_encoding}, bit_width = {width}, max_size = {max_size}")]
pub struct SizeRepr {
	pub endianness: Endianness,
	pub num_encoding: NumEncoding,
	pub width: BitWidth,
	pub max_size: usize
}

impl SizeRepr {
	/// Returns the default size representation: little endian, fixed encoding, 64 bit width,
	/// and the max size set to `usize::MAX`
	pub const fn new() -> Self {
		Self {
			endianness: Endianness::LittleEndian,
			num_encoding: NumEncoding::Fixed,
			width: BitWidth::Bit64,
			max_size: usize::MAX,
		}
	}
}

impl Default for SizeRepr {
	fn default() -> Self {
		Self::new()
	}
}

/// Controls the binary representation of enum variants.
/// Specifically, controls the [`Endianness`], the [`NumEncoding`], and the [`BitWidth`].
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("endianness = {endianness} , encoding = {num_encoding}, bit_width = {width}")]
pub struct VariantRepr {
	pub endianness: Endianness,
	pub num_encoding: NumEncoding,
	pub width: BitWidth
}

impl VariantRepr {
	/// Returns the default variant representation: little endian, fixed encoding and 32 bit width
	pub const fn new() -> Self {
		Self {
			endianness: Endianness::LittleEndian,
			num_encoding: NumEncoding::Fixed,
			width: BitWidth::Bit32
		}
	}
}

impl Default for VariantRepr {
	fn default() -> Self {
		Self::new()
	}
}

/// Controls the binary representation of strings.
/// Specifically, controls the [`StrEncoding`] of strings and chars, the [`StrLenEncoding`],
/// to determine whether they are encoded as c-like strings (null-terminated) or length prefixed,
/// and the [`Endianness`] in which the encoded bytes are ordered.<br>
/// Keep in mind not all encodings support null terminated strings, because
/// the encoding format may have the capability to contain nulls.<br>
/// In such cases, the encoding process will produce an error in case the encoded string contains
/// null characters, and the end of the string is encoded as a sequence of nulls of the appropriate
/// length (1 byte for UTF-8 and ASCII, 2 bytes for UTF-16, 4 bytes for UTF-32)
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("str_encoding = {str_encoding} , len_encoding = {len_encoding}, endianness = {endianness}")]
pub struct StringRepr {
	pub str_encoding: StrEncoding,
	pub len_encoding: StrLenEncoding,
	pub endianness: Endianness,
}

impl StringRepr {
	/// Returns the default string representation: utf-8, length-prefixed, little_endian
	pub const fn new() -> Self {
		Self {
			str_encoding: StrEncoding::Utf8,
			len_encoding: StrLenEncoding::LenPrefixed,
			endianness: Endianness::LittleEndian,
		}
	}
}

impl Default for StringRepr {
	fn default() -> Self {
		Self::new()
	}
}

/// An aggregation of [`NumRepr`], [`SizeRepr`], [`VariantRepr`], [`StringRepr`]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("num_repr = ({num_repr}), size_repr = ({size_repr}), variant_repr = ({variant_repr}), string_repr = ({string_repr})")]
pub struct BinSettings {
	pub num_repr: NumRepr,
	pub size_repr: SizeRepr,
	pub variant_repr: VariantRepr,
	pub string_repr: StringRepr,
}

impl BinSettings {
	/// Returns the default options containing the default for each representation.
	/// See: [`NumRepr::new`], [`SizeRepr::new`], [`VariantRepr::new`], [`StringRepr::new`]
	pub const fn new() -> Self {
		Self {
			num_repr: NumRepr::new(),
			size_repr: SizeRepr::new(),
			variant_repr: VariantRepr::new(),
			string_repr: StringRepr::new(),
		}
	}
}

impl Default for BinSettings {
	fn default() -> Self {
		Self::new()
	}
}

/// The state of the encoder, including its options and a `flatten` state variable
#[derive(Copy, Clone, Debug)]
pub struct Context<'a> {
	/// User provided data. This can be purposed for storing any kind of data,
	/// like cryptographic keys that are unknown to the data structures but known
	/// at a higher level.
	pub user: Option<&'a dyn Any>,
	/// The actual settings, which determine the numerical representations and the string
	/// representations. <br>Implementations of [`Encode`] and [`Decode`] are required to
	/// preserve the state of the settings, even though they are allowed to temporarily modify it.<br>
	/// In case of an error occurring, no guarantee is made about the state of the settings:
	/// for this reason it's good practice to store a copy of the settings somewhere.
	pub settings: BinSettings,
	/// The flatten state variable. When present, for `Option` it indicates in Encode mode
	/// not to write whether the optional is present, and in Decode mode that it is present (without
	/// checking), for `Vec`, `HashMap` and other data structures with a length it indicates in
	/// Encode mode not to write said length, and in Decode mode the length itself.
	pub flatten: Option<usize>,
}

impl<'a> Context<'a> {
	/// Constructs the default encoder state. Options will be set to default, flatten to None.
	pub fn new() -> Self {
		Self {
			user: None,
			settings: BinSettings::new(),
			flatten: None,
		}
	}

	/// Just like [`Self::new`] but uses the given settings instead of the default
	pub fn with_settings(settings: BinSettings) -> Self {
		Self {
			user: None,
			settings,
			flatten: None,
		}
	}

	/// Resets the context to its defaults, then overwrites the options with the given options.
	pub fn reset(&mut self, options: BinSettings) {
		self.settings = options;
		self.flatten = None;
	}

	/// Returns the state of the `flatten` variable, consuming it.
	pub fn flatten(&mut self) -> Option<usize> {
		replace(&mut self.flatten, None)
	}
}

/// The base type for encoding/decoding. References a stream, and a [`Context`].<br>
/// It's recommended to wrap the stream in a [`std::io::BufReader`] or [`std::io::BufWriter`],
/// because many small write and read calls will be made
pub struct Encoder<'a, T>{
	/// The underlying stream
	pub stream: T,
	/// The state
	pub ctxt: Context<'a>,
}

impl<'a, T> Encoder<'a, T> {
	/// Wraps the given stream and state.
	pub fn new(stream: T, ctxt: Context<'a>) -> Self {
		Self {
			stream,
			ctxt
		}
	}

	/// Replaces the underlying stream with the new one, returning the previous value
	pub fn swap_stream(&mut self, new: T) -> T {
		replace(&mut self.stream, new)
	}
}

impl<T: Write> Encoder<'_, T> {
	/// Method for convenience.<br>
	/// Encodes a value using `self` as the encoder.<br>
	/// This method is not magic - it is literally defined as `value.encode(self)`
	pub fn encode_value<V: Encode>(&mut self, value: V) -> EncodingResult<(), T::Error> {
		value.encode(self)
	}
}

impl<T: Read> Encoder<'_, T> {
	/// Method for convenience.<br>
	/// Decodes a value using `self` as the decoder.<br>
	/// This method is not magic - it is literally defined as `V::decode(self)`
	pub fn decode_value<V: Decode>(&mut self) -> EncodingResult<V, T::Error> {
		V::decode(self)
	}
}

impl<'a, T> Encoder<'a, T> {
	pub fn finish(self) -> (T, Context<'a>) {
		(self.stream, self.ctxt)
	}
}

macro_rules! make_write_fns {
    (
	    type $uty:ty {
		    pub u_write: $u_write:ident,
		    pub u_write_direct: $u_write_direct:ident,
		    priv u_write_size: $u_write_size:ident,
		    priv u_write_variant: $u_write_variant:ident,
		    priv uleb128_encode: $uleb128_encode:ident
		    $(,)?
	    },
	    type $ity:ty {
		    pub i_write: $i_write:ident,
		    pub i_write_direct: $i_write_direct:ident,
		    priv i_write_size: $i_write_size:ident,
		    priv i_write_variant: $i_write_variant:ident,
		    priv leb128_encode: $leb128_encode:ident
		    $(,)?
	    }$(,)?
    ) => {
	    fn $uleb128_encode(&mut self, value: $uty) -> EncodingResult<(), T::Error> {
		    let mut shifted = value;
	        let mut byte = [u8::MAX; 1];
	        let mut more = true;
	        while more {
		        byte[0] = shifted as u8 & 0b01111111;
		        shifted >>= 7;
		        
		        // Is the next shifted value worth writing?
		        if shifted != 0 {
			        byte[0] |= 0b10000000;
		        } else {
			        more = false;
		        }
		        self.stream.write_all(&byte)?;
			}
		    Ok(())
	    }
	    
	    #[doc = "Encodes a `"]
	    #[doc = stringify!($uty)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $u_write(&mut self, value: $uty) -> EncodingResult<(), T::Error> {
		    self.$u_write_direct(value, self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }
	    
	    fn $u_write_size(&mut self, value: $uty) -> EncodingResult<(), T::Error> {
		    self.$u_write_direct(value, self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }
	    
	    fn $u_write_variant(&mut self, value: $uty) -> EncodingResult<(), T::Error> {
		    self.$u_write_direct(value, self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }

        pub fn $u_write_direct(&mut self, value: $uty, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<(), T::Error> {
	        match num_encoding {
		        NumEncoding::Fixed => {
			        let bytes: [u8; core::mem::size_of::<$uty>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write_all(&bytes)?;
		        },
		        NumEncoding::Leb128 | NumEncoding::ProtobufWasteful | NumEncoding::ProtobufZigzag => {
			        self.$uleb128_encode(value)?;
		        }
	        }
            Ok(())
        }
	    
	    fn $leb128_encode(&mut self, value: $ity) -> EncodingResult<(), T::Error> {
		        let mut shifted = value;
		        let mut byte = [0u8; 1];
		        let mut more = true;
		        while more {
			        byte[0] = shifted as u8 & 0b0111_1111;
			        shifted >>= 7;
			        
			        // Is the next shifted value worth writing?
			        let neg = (byte[0] & 0b0100_0000) != 0;
			        if (neg && shifted != -1) || (!neg && shifted != 0) {
				        byte[0] |= 0b1000_0000;
			        } else {
				        more = false;
			        }
			        self.stream.write_all(&byte)?;
				}
		        Ok(())
	        }
	    
	    #[doc = "Encodes a `"]
	    #[doc = stringify!($ity)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $i_write(&mut self, value: $ity) -> EncodingResult<(), T::Error> {
		    self.$i_write_direct(value, self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }
	    
	    fn $i_write_size(&mut self, value: $ity) -> EncodingResult<(), T::Error> {
		    self.$i_write_direct(value, self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }
	    
	    fn $i_write_variant(&mut self, value: $ity) -> EncodingResult<(), T::Error> {
		    self.$i_write_direct(value, self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }

        pub fn $i_write_direct(&mut self, value: $ity, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<(), T::Error> {
		    match num_encoding {
		        NumEncoding::Fixed => {
			        let bytes: [u8; core::mem::size_of::<$ity>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write_all(&bytes)?;
		        },
		        NumEncoding::Leb128 => {
			        self.$leb128_encode(value)?;
		        },
		        NumEncoding::ProtobufWasteful => {
			        let unsigned = <$uty>::from_ne_bytes(value.to_ne_bytes());
			        self.$uleb128_encode(unsigned)?;
		        }
			    NumEncoding::ProtobufZigzag => {
			        let shifted = (value << 1) ^ (value >> (<$ity>::BITS - 1));
			        let unsigned = <$uty>::from_ne_bytes(shifted.to_ne_bytes());
			        self.$uleb128_encode(unsigned)?;
		        }
	        }
            Ok(())
        }
    };
}

macro_rules! make_read_fns {
    (
	    type $uty:ty {
		    pub u_read: $u_read:ident,
		    pub u_read_direct: $u_read_direct:ident,
		    priv u_read_size: $u_read_size:ident,
		    priv u_read_variant: $u_read_variant:ident,
		    priv uleb128_decode: $uleb128_decode:ident
		    $(,)?
	    },
	    type $ity:ty {
		    pub i_read: $i_read:ident,
		    pub i_read_direct: $i_read_direct:ident,
		    priv i_read_size: $i_read_size:ident,
		    priv i_read_variant: $i_read_variant:ident,
		    priv leb128_decode: $leb128_decode:ident
		    $(,)?
	    }
	    $(,)?
    ) => {
	    fn $uleb128_decode(&mut self) -> EncodingResult<$uty, T::Error> {
			    let mut result: $uty = 0;
		        let mut byte = [0u8; 1];
		        let mut shift: u8 = 0;
		        loop {
			        if shift >= <$uty>::BITS as u8 {
				        return Err(EncodingError::VarIntError);
			        }
			        
		            self.stream.read_exact(&mut byte)?;
			        result |= (byte[0] & 0b0111_1111) as $uty << shift;
			        shift += 7;
			        
			        if (byte[0] & 0b1000_0000) == 0 {
				        break;
			        }
				}
		        Ok(result)
		    }
	    
	    #[doc = "Decodes a `"]
	    #[doc = stringify!($uty)]
	    #[doc = "` from the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $u_read(&mut self) -> EncodingResult<$uty, T::Error> {
		    self.$u_read_direct(self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }
	    
	    fn $u_read_size(&mut self) -> EncodingResult<$uty, T::Error> {
		    self.$u_read_direct(self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }
	    
	    fn $u_read_variant(&mut self) -> EncodingResult<$uty, T::Error> {
		    self.$u_read_direct(self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }
	    
        pub fn $u_read_direct(&mut self, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<$uty, T::Error> {
		    Ok(match num_encoding {
		        NumEncoding::Fixed => {
			        let mut bytes: [u8; core::mem::size_of::<$uty>()] = [0u8; core::mem::size_of::<$uty>()];
		            self.stream.read_exact(&mut bytes)?;

		            match endianness {
			            Endianness::BigEndian => <$uty>::from_be_bytes(bytes),
			            Endianness::LittleEndian => <$uty>::from_le_bytes(bytes)
		            }
		        }
		        NumEncoding::Leb128 | NumEncoding::ProtobufWasteful | NumEncoding::ProtobufZigzag => {
			        self.$uleb128_decode()?
		        }
	        })
        }
	    
	     fn $leb128_decode(&mut self) -> EncodingResult<$ity, T::Error> {
			    let mut result: $ity = 0;
		        let mut byte = [0u8; 1];
		        let mut shift: u8 = 0;
		        loop {
			        if shift >= <$ity>::BITS as u8 {
				        return Err(EncodingError::VarIntError);
			        }
			        
		            self.stream.read_exact(&mut byte)?;
			        result |= (byte[0] & 0b0111_1111) as $ity << shift;
			        shift += 7;
			        
			        if (byte[0] & 0b1000_0000) == 0 {
				        break;
			        }
				}
		        
		        if shift < <$ity>::BITS as u8 && (byte[0] & 0b0100_0000) != 0 {
			        result |= (!0 << shift);
		        }
		        
		        Ok(result)
		    }
	    
	    #[doc = "Decodes a `"]
	    #[doc = stringify!($ity)]
	    #[doc = "` from the underlying stream, according to the endianness and numerical encoding in the encoder's context"]
	    pub fn $i_read(&mut self) -> EncodingResult<$ity, T::Error> {
		    self.$i_read_direct(self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }
	    
	    fn $i_read_size(&mut self) -> EncodingResult<$ity, T::Error> {
		    self.$i_read_direct(self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }
	    
	    fn $i_read_variant(&mut self) -> EncodingResult<$ity, T::Error> {
		    self.$i_read_direct(self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }
	    
        pub fn $i_read_direct(&mut self, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<$ity, T::Error> {
	        Ok(match num_encoding {
		        NumEncoding::Fixed => {
			        let mut bytes: [u8; core::mem::size_of::<$ity>()] = [0u8; core::mem::size_of::<$ity>()];
		            self.stream.read_exact(&mut bytes)?;

		            match endianness {
			            Endianness::BigEndian => <$ity>::from_be_bytes(bytes),
			            Endianness::LittleEndian => <$ity>::from_le_bytes(bytes)
		            }
		        }
		        NumEncoding::Leb128 => {
			        self.$leb128_decode()?
		        }
		        NumEncoding::ProtobufWasteful => {
			        let unsigned = self.$uleb128_decode()?;
			        <$ity>::from_ne_bytes(unsigned.to_ne_bytes())
		        }
		        NumEncoding::ProtobufZigzag => {
			        let unsigned = self.$uleb128_decode()?;
			        let neg = (unsigned & 1) != 0;
			        let transformed = if neg {
				        !(unsigned >> 1)
			        } else {
				        unsigned >> 1
			        };
			        
			        <$ity>::from_ne_bytes(transformed.to_ne_bytes())
		        }
	        })
        }
    };
}

impl<T: Write> Encoder<'_, T> {
	make_write_fns!{
		type u8 {
			pub u_write: write_u8,
			pub u_write_direct: write_u8_direct,
			priv u_write_size: _write_u8_size,
			priv u_write_variant: _write_u8_variant,
			priv uleb128_encode: uleb128_encode_u8,
		},
		type i8 {
			pub i_write: write_i8,
			pub i_write_direct: write_i8_direct,
			priv i_write_size: _write_i8_size,
			priv i_write_variant: _write_i8_variant,
			priv leb128_encode: leb128_encode_i8,
		},
	}
	make_write_fns!{
		type u16 {
			pub u_write: write_u16,
			pub u_write_direct: write_u16_direct,
			priv u_write_size: _write_u16_size,
			priv u_write_variant: _write_u16_variant,
			priv uleb128_encode: uleb128_encode_u16,
		},
		type i16 {
			pub i_write: write_i16,
			pub i_write_direct: write_i16_direct,
			priv i_write_size: _write_i16_size,
			priv i_write_variant: _write_i16_variant,
			priv leb128_encode: leb128_encode_i16,
		},
	}
	make_write_fns!{
		type u32 {
			pub u_write: write_u32,
			pub u_write_direct: write_u32_direct,
			priv u_write_size: _write_u32_size,
			priv u_write_variant: _write_u32_variant,
			priv uleb128_encode: uleb128_encode_u32,
		},
		type i32 {
			pub i_write: write_i32,
			pub i_write_direct: write_i32_direct,
			priv i_write_size: _write_i32_size,
			priv i_write_variant: _write_i32_variant,
			priv leb128_encode: leb128_encode_i32,
		},
	}
	make_write_fns!{
		type u64 {
			pub u_write: write_u64,
			pub u_write_direct: write_u64_direct,
			priv u_write_size: _write_u64_size,
			priv u_write_variant: _write_u64_variant,
			priv uleb128_encode: uleb128_encode_u64,
		},
		type i64 {
			pub i_write: write_i64,
			pub i_write_direct: write_i64_direct,
			priv i_write_size: _write_i64_size,
			priv i_write_variant: _write_i64_variant,
			priv leb128_encode: leb128_encode_i64,
		},
	}
	make_write_fns!{
		type u128 {
			pub u_write: write_u128,
			pub u_write_direct: write_u128_direct,
			priv u_write_size: _write_u128_size,
			priv u_write_variant: _write_u128_variant,
			priv uleb128_encode: uleb128_encode_u128,
		},
		type i128 {
			pub i_write: write_i128,
			pub i_write_direct: write_i128_direct,
			priv i_write_size: _write_i128_size,
			priv i_write_variant: _write_i128_variant,
			priv leb128_encode: leb128_encode_i128,
		},
	}

	/// Encodes a length. If the flatten attribute is set to Some, this function checks
	/// if the value matches but then returns immediately without writing,
	/// otherwise it will behave identically to [`Self::write_usize`], with an additional
	/// check that the length does not exceed the max size defined in the encoder's state.
	pub fn write_length(&mut self, value: usize) -> EncodingResult<(), T::Error> {
		if let Some(flatten) = self.ctxt.flatten() {
			if flatten != value {
				return Err(EncodingError::FlattenError(FlattenError::LenMismatch {
					expected: flatten,
					got: value,
				}));
			}
			Ok(())
		} else {
			if value > self.ctxt.settings.size_repr.max_size {
				return Err(EncodingError::MaxLengthExceeded {
					max: self.ctxt.settings.size_repr.max_size,
					requested: value
				})
			}
			self.write_usize(value)
		}
	}

	/// Encodes a `usize` to the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn write_usize(&mut self, value: usize) -> EncodingResult<(), T::Error> {
		match self.ctxt.settings.size_repr.width {
			BitWidth::Bit8 => self._write_u8_size(value as _),
			BitWidth::Bit16 => self._write_u16_size(value as _),
			BitWidth::Bit32 => self._write_u32_size(value as _),
			BitWidth::Bit64 => self._write_u64_size(value as _),
			BitWidth::Bit128 => self._write_u128_size(value as _),
		}
	}

	/// Encodes a `isize` to the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn write_isize(&mut self, value: isize) -> EncodingResult<(), T::Error> {
		match self.ctxt.settings.size_repr.width {
			BitWidth::Bit8 => self._write_i8_size(value as _),
			BitWidth::Bit16 => self._write_i16_size(value as _),
			BitWidth::Bit32 => self._write_i32_size(value as _),
			BitWidth::Bit64 => self._write_i64_size(value as _),
			BitWidth::Bit128 => self._write_i128_size(value as _),
		}
	}

	/// Encodes an unsigned enum variant to the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn write_uvariant(&mut self, value: u128) -> EncodingResult<(), T::Error> {
		match self.ctxt.settings.variant_repr.width {
			BitWidth::Bit8 => self._write_u8_variant(value as _),
			BitWidth::Bit16 => self._write_u16_variant(value as _),
			BitWidth::Bit32 => self._write_u32_variant(value as _),
			BitWidth::Bit64 => self._write_u64_variant(value as _),
			BitWidth::Bit128 => self._write_u128_variant(value as _),
		}
	}

	/// Encodes a signed enum variant to the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn write_ivariant(&mut self, value: i128) -> EncodingResult<(), T::Error> {
		match self.ctxt.settings.variant_repr.width {
			BitWidth::Bit8 => self._write_i8_variant(value as _),
			BitWidth::Bit16 => self._write_i16_variant(value as _),
			BitWidth::Bit32 => self._write_i32_variant(value as _),
			BitWidth::Bit64 => self._write_i64_variant(value as _),
			BitWidth::Bit128 => self._write_i128_variant(value as _),
		}
	}

	/// Encodes the boolean state of a value.
	/// If the flatten attribute is set to Some, this function checks
	/// if the value matches but then returns immediately without writing,
	/// otherwise it will behave identically to [`Self::write_bool`].
	pub fn write_state(&mut self, value: bool) -> EncodingResult<(), T::Error> {
		if let Some(flatten) = self.ctxt.flatten() {
			if (flatten != 0) != value {
				return Err(EncodingError::FlattenError(FlattenError::BoolMismatch {
					expected: flatten != 0,
					got: value,
				}));
			}
			Ok(())
		} else {
			self.write_bool(value)
		}
	}

	/// Encodes a `bool` to the underlying stream, ignoring any encoding option.
	/// It is guaranteed that, if `value` is `true`, a single u8 will be written to the
	/// underlying stream with the value `1`, and if `value` is `false`, with a value of `0`
	pub fn write_bool(&mut self, value: bool) -> EncodingResult<(), T::Error> {
		self.write_u8_direct(value as u8, NumEncoding::Fixed, Endianness::LittleEndian)
	}

	/// Encodes a `char` to the underlying stream, according to the endianness and string encoding
	/// in the encoder's state.
	pub fn write_char(&mut self, value: char) -> EncodingResult<(), T::Error> {
		let endianness = self.ctxt.settings.string_repr.endianness;
		match self.ctxt.settings.string_repr.str_encoding {
			StrEncoding::Ascii => {
				if !value.is_ascii() {
					return Err(EncodingError::StringError(StringError::InvalidAscii));
				}

				self.write_u8_direct(value as u8, NumEncoding::Fixed, Endianness::LittleEndian)
			}
			StrEncoding::Utf8 => {
				let mut buf = [0u8; 4];
				let len = value.encode_utf8(&mut buf).len();

				self.write_raw_bytes(&buf[..len])
			}
			StrEncoding::Utf16 => {
				let mut buf = [0u16; 2];
				let len = value.encode_utf16(&mut buf).len();

				for block in buf[..len].iter() {
					self.write_u16_direct(*block, NumEncoding::Fixed, endianness)?;
				}
				Ok(())
			}
			StrEncoding::Utf32 => {
				self.write_u32_direct(value as u32, NumEncoding::Fixed, endianness)
			}
		}
	}

	/// Encodes a `f32` to the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `Self::write_u32(value.to_bits())` with the numeric
	/// encoding set to Fixed
	pub fn write_f32(&mut self, value: f32) -> EncodingResult<(), T::Error> {
		self.write_u32_direct(value.to_bits(), NumEncoding::Fixed, self.ctxt.settings.num_repr.endianness)
	}

	/// Encodes a `f64` to the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `Self::write_u64(value.to_bits())` with the numeric
	/// encoding set to Fixed
	pub fn write_f64(&mut self, value: f64) -> EncodingResult<(), T::Error> {
		self.write_u64_direct(value.to_bits(), NumEncoding::Fixed, self.ctxt.settings.num_repr.endianness)
	}

	/// Encodes a string to the underlying stream, according to the endianness,
	/// string encoding and string-length encoding in the encoder's state
	pub fn write_str(&mut self, string: &str) -> EncodingResult<(), T::Error> {
		let endianness = self.ctxt.settings.string_repr.endianness;
		let null_term = match self.ctxt.settings.string_repr.len_encoding {
			StrLenEncoding::LenPrefixed => {
				self.write_length(string.len())?;
				false
			}
			StrLenEncoding::NullTerminated => {
				// Check for nulls inside the string
				for x in string.as_bytes() {
					if *x == 0 {
						return Err(EncodingError::StringError(StringError::InvalidCString));
					}
				}
				true
			},
		};
		match self.ctxt.settings.string_repr.str_encoding {
			StrEncoding::Ascii => {
				if !string.is_ascii() {
					return Err(EncodingError::StringError(StringError::InvalidAscii));
				}
				self.write_raw_bytes(string.as_bytes())?;
				if null_term {
					self.write_raw_bytes(&[0])?;
				}
 			}
			StrEncoding::Utf8 => {
				// Rust strings are guaranteed to be utf-8 encoded
				self.write_raw_bytes(string.as_bytes())?;
				if null_term {
					self.write_raw_bytes(&[0])?;
				}
			}
			StrEncoding::Utf16 => {
				for block in string.encode_utf16() {
					if null_term && block == 0 {
						return Err(EncodingError::StringError(StringError::InvalidCString));
					}
					self.write_u16_direct(block, NumEncoding::Fixed, endianness)?;
				}
				if null_term {
					self.write_raw_bytes(&[0, 0])?;
				}
			}
			StrEncoding::Utf32 => {
				for block in string.chars() {
					if null_term && block as u32 == 0 {
						return Err(EncodingError::StringError(StringError::InvalidCString));
					}
					self.write_u32_direct(block as u32, NumEncoding::Fixed, endianness)?;
				}
				if null_term {
					self.write_raw_bytes(&[0, 0, 0, 0])?;
				}
			}
		}
		Ok(())
	}

	/// Writes the given slice to the underlying stream as-is.
	pub fn write_raw_bytes(&mut self, bytes: &[u8]) -> EncodingResult<(), T::Error> {
		Ok(self.stream.write_all(bytes)?)
	}
}

impl<T: Read> Encoder<'_, T> {
	make_read_fns!{
		type u8 {
			pub u_read: read_u8,
			pub u_read_direct: read_u8_direct,
			priv u_read_size: _read_u8_size,
			priv u_read_variant: _read_u8_variant,
			priv uleb128_decode: uleb128_decode_u8,
		},
		type i8 {
			pub i_read: read_i8,
			pub i_read_direct: read_i8_direct,
			priv i_read_size: _read_i8_size,
			priv i_read_variant: _read_i8_variant,
			priv leb128_decode: leb128_decode_i8,
		},
	}
	make_read_fns!{
		type u16 {
			pub u_read: read_u16,
			pub u_read_direct: read_u16_direct,
			priv u_read_size: _read_u16_size,
			priv u_read_variant: _read_u16_variant,
			priv uleb128_decode: uleb128_decode_u16,
		},
		type i16 {
			pub i_read: read_i16,
			pub i_read_direct: read_i16_direct,
			priv i_read_size: _read_i16_size,
			priv i_read_variant: _read_i16_variant,
			priv leb128_decode: leb128_decode_i16,
		},
	}
	make_read_fns!{
		type u32 {
			pub u_read: read_u32,
			pub u_read_direct: read_u32_direct,
			priv u_read_size: _read_u32_size,
			priv u_read_variant: _read_u32_variant,
			priv uleb128_decode: uleb128_decode_u32,
		},
		type i32 {
			pub i_read: read_i32,
			pub i_read_direct: read_i32_direct,
			priv i_read_size: _read_i32_size,
			priv i_read_variant: _read_i32_variant,
			priv leb128_decode: leb128_decode_i32,
		},
	}
	make_read_fns!{
		type u64 {
			pub u_read: read_u64,
			pub u_read_direct: read_u64_direct,
			priv u_read_size: _read_u64_size,
			priv u_read_variant: _read_u64_variant,
			priv uleb128_decode: uleb128_decode_u64,
		},
		type i64 {
			pub i_read: read_i64,
			pub i_read_direct: read_i64_direct,
			priv i_read_size: _read_i64_size,
			priv i_read_variant: _read_i64_variant,
			priv leb128_decode: leb128_decode_i64,
		},
	}
	make_read_fns!{
		type u128 {
			pub u_read: read_u128,
			pub u_read_direct: read_u128_direct,
			priv u_read_size: _read_u128_size,
			priv u_read_variant: _read_u128_variant,
			priv uleb128_decode: uleb128_decode_u128,
		},
		type i128 {
			pub i_read: read_i128,
			pub i_read_direct: read_i128_direct,
			priv i_read_size: _read_i128_size,
			priv i_read_variant: _read_i128_variant,
			priv leb128_decode: leb128_decode_i128,
		},
	}

	/// Decodes a length. If the flatten attribute is set to Some, this function
	/// will return its value without reading, otherwise it will behave identically to
	/// [`Self::read_usize`], with an additional check that the length does not exceed the
	/// max size defined in the encoder's state.
	pub fn read_length(&mut self) -> EncodingResult<usize, T::Error> {
		if let Some(length) = self.ctxt.flatten() {
			Ok(length)
		} else {
			let value = self.read_usize()?;
			if value > self.ctxt.settings.size_repr.max_size {
				return Err(EncodingError::MaxLengthExceeded {
					max: self.ctxt.settings.size_repr.max_size,
					requested: value
				})
			}
			Ok(value)
		}
	}

	/// Decodes a `usize` from the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn read_usize(&mut self) -> EncodingResult<usize, T::Error> {
		Ok(match self.ctxt.settings.size_repr.width {
			BitWidth::Bit8 => self._read_u8_size()? as usize,
			BitWidth::Bit16 => self._read_u16_size()? as usize,
			BitWidth::Bit32 => self._read_u32_size()? as usize,
			BitWidth::Bit64 => self._read_u64_size()? as usize,
			BitWidth::Bit128 => self._read_u128_size()? as usize,
		})
	}

	/// Decodes a `isize` from the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn read_isize(&mut self) -> EncodingResult<isize, T::Error> {
		Ok(match self.ctxt.settings.size_repr.width {
			BitWidth::Bit8 => self._read_i8_size()? as isize,
			BitWidth::Bit16 => self._read_i16_size()? as isize,
			BitWidth::Bit32 => self._read_i32_size()? as isize,
			BitWidth::Bit64 => self._read_i64_size()? as isize,
			BitWidth::Bit128 => self._read_i128_size()? as isize,
		})
	}

	/// Decodes an unsigned enum variant from the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn read_uvariant(&mut self) -> EncodingResult<u128, T::Error> {
		Ok(match self.ctxt.settings.variant_repr.width {
			BitWidth::Bit8 => self._read_u8_variant()? as _,
			BitWidth::Bit16 => self._read_u16_variant()? as _,
			BitWidth::Bit32 => self._read_u32_variant()? as _,
			BitWidth::Bit64 => self._read_u64_variant()? as _,
			BitWidth::Bit128 => self._read_u128_variant()? as _,
		})
	}

	/// Decodes a signed enum variant from the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn read_ivariant(&mut self) -> EncodingResult<i128, T::Error> {
		Ok(match self.ctxt.settings.variant_repr.width {
			BitWidth::Bit8 => self._read_i8_variant()? as _,
			BitWidth::Bit16 => self._read_i16_variant()? as _,
			BitWidth::Bit32 => self._read_i32_variant()? as _,
			BitWidth::Bit64 => self._read_i64_variant()? as _,
			BitWidth::Bit128 => self._read_i128_variant()? as _,
		})
	}

	/// Decodes the boolean state of a value. If the flatten attribute is set to Some,
	/// this function will return its value (converted to a bool) without reading from the
	/// underlying stream, otherwise it will behave identically to [`Self::read_bool`].
	pub fn read_state(&mut self) -> EncodingResult<bool, T::Error> {
		if let Some(length) = self.ctxt.flatten() {
			match length {
				0 => Ok(false),
				1 => Ok(true),
				_ => Err(EncodingError::FlattenError(FlattenError::InvalidBool))
			}
		} else {
			self.read_bool()
		}
	}

	/// Decodes a `bool` from the underlying stream, ignoring any encoding option.
	/// It is guaranteed that, one u8 is read from the underlying stream and, if
	/// it's equal to `1`, `true` is returned, if it's equal to `0`, `false` is returned,
	/// if it's equal to any other value, `InvalidBool` error will be returned
	pub fn read_bool(&mut self) -> EncodingResult<bool, T::Error> {
		match self.read_u8_direct(NumEncoding::Fixed, Endianness::LittleEndian)? {
			0 => Ok(false),
			1 => Ok(true),
			_ => Err(EncodingError::InvalidBool)
		}
	}

	/// Decodes a `char` from the underlying stream, according to the endianness and string encoding
	/// in the encoder's state.
	pub fn read_char(&mut self) -> EncodingResult<char, T::Error> {
		let endianness = self.ctxt.settings.string_repr.endianness;
		match self.ctxt.settings.string_repr.str_encoding {
			StrEncoding::Ascii => {
				let buf = self.read_u8_direct(NumEncoding::Fixed, Endianness::LittleEndian)?;
				let ch = char::from_u32(buf as u32).ok_or(StringError::InvalidAscii)?;
				
				if !ch.is_ascii() {
					return Err(EncodingError::StringError(StringError::InvalidAscii));
				}

				Ok(ch)
			}
			StrEncoding::Utf8 => {
				// See https://en.wikipedia.org/wiki/UTF-8#Encoding
				let mut buf = self.read_u8_direct(NumEncoding::Fixed, Endianness::LittleEndian)?;
				let (add, rshift) = if (buf & (0b1000_0000)) == 0 {
					(0usize, 1u32)
				} else {
					let leading = buf.leading_ones();
					if leading == 1 || leading > 4 {
						// Most likely malformed utf-8
						return Err(EncodingError::StringError(StringError::InvalidUtf8));
					}
					(leading as usize - 1, leading + 1)
				};
				
				let mut ch: u32 = ((u8::MAX >> rshift) & buf) as u32;
				
				let mut shift = 0;
				for _ in 0..add {
					buf = self.read_u8_direct(NumEncoding::Fixed, Endianness::LittleEndian)?;
					
					if buf.leading_ones() != 1 {
						// Most likely malformed utf-8
						return Err(EncodingError::StringError(StringError::InvalidUtf8));
					}
					
					shift += 6;
					ch = (ch << shift) | ((buf & 0b0011_1111) as u32);
				}
				
				Ok(char::from_u32(ch).ok_or(StringError::InvalidUtf8)?)
			}
			StrEncoding::Utf16 => {
				// See https://en.wikipedia.org/wiki/UTF-16#Code_points_from_U+010000_to_U+10FFFF
				let buf = self.read_u16_direct(NumEncoding::Fixed, endianness)?;
				let ch;
				
				// This is a high surrogate
				if 0xD800 <= buf && buf <= 0xDBFF {
					let high_surrogate = buf;
					let low_surrogate = self.read_u16_direct(NumEncoding::Fixed, endianness)?;
					
					if !(0xDC00 <= low_surrogate && low_surrogate <= 0xDFFF) {
						// First character was in the high surrogate range,
						// but the second character wasn't in the low surrogate range
						return Err(EncodingError::StringError(StringError::InvalidUtf16));
					}

					const LOW_TEN_BITS: u16 = 0b0000_0011_1111_1111;
					
					let high_bits = ((high_surrogate - 0xD800) & LOW_TEN_BITS) as u32;
					let low_bits = ((low_surrogate - 0xDC00) & LOW_TEN_BITS) as u32;
					
					ch = (high_bits << 10) | low_bits;
				} else if 0xDC00 <= buf && buf <= 0xDFFF {
					// First character was in the low surrogate range
					return Err(EncodingError::StringError(StringError::InvalidUtf16));
				} else {
					ch = buf as u32;
				}
				
				Ok(char::from_u32(ch).ok_or(StringError::InvalidUtf16)?)
			}
			StrEncoding::Utf32 => {
				let ch = self.read_u32_direct(NumEncoding::Fixed, endianness)?;
				Ok(char::from_u32(ch).ok_or(StringError::InvalidUtf32)?)
			}
		}
	}

	/// Decodes a `f32` from the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `f32::from_bits(self.read_u32())` with the numeric
	/// encoding set to Fixed
	pub fn read_f32(&mut self) -> EncodingResult<f32, T::Error> {
		Ok(f32::from_bits(self.read_u32_direct(NumEncoding::Fixed, self.ctxt.settings.num_repr.endianness)?))
	}

	/// Decodes a `f64` from the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `f64::from_bits(self.read_u64())` with the numeric
	/// encoding set to Fixed
	pub fn read_f64(&mut self) -> EncodingResult<f64, T::Error> {
		Ok(f64::from_bits(self.read_u64_direct(NumEncoding::Fixed, self.ctxt.settings.num_repr.endianness)?))
	}

	/// Decodes a String from the underlying stream, according to the endianness,
	/// string encoding and string-length encoding in the encoder's state
	#[cfg(feature = "alloc")]
	pub fn read_string(&mut self) -> EncodingResult<alloc::string::String, T::Error> {
		use alloc::string::*;
		use alloc::vec;
		use alloc::vec::Vec;
		
		let endianness = self.ctxt.settings.string_repr.endianness;
		let len_encoding = self.ctxt.settings.string_repr.len_encoding;
		
		match self.ctxt.settings.string_repr.str_encoding {
			StrEncoding::Ascii => {
				let buffer = match len_encoding {
					StrLenEncoding::LenPrefixed => {
						let bytes = self.read_length()?;
						let mut buffer = vec![0u8; bytes];
						self.read_raw_bytes(&mut buffer)?;
						buffer
					}
					StrLenEncoding::NullTerminated => {
						let mut buffer = Vec::new();
						let mut val = [0u8; 1];
						while { self.read_raw_bytes(&mut val)?; val[0] != 0 } {
							buffer.push(val[0]);
						}
						buffer
					},
				};
				
				let string = String::from_utf8(buffer).map_err(|_| StringError::InvalidAscii)?;

				if !string.is_ascii() {
					return Err(EncodingError::StringError(StringError::InvalidAscii));
				}

				Ok(string)
			}
			StrEncoding::Utf8 => {
				let buffer = match len_encoding {
					StrLenEncoding::LenPrefixed => {
						let bytes = self.read_length()?;
						let mut buffer = vec![0u8; bytes];
						self.read_raw_bytes(&mut buffer)?;
						buffer
					}
					StrLenEncoding::NullTerminated => {
						let mut buffer = Vec::new();
						let mut val = [0u8; 1];
						while { self.read_raw_bytes(&mut val)?; val[0] != 0 } {
							buffer.push(val[0]);
						}
						buffer
					},
				};
				
				// Rust strings are guaranteed to be utf-8 encoded
				Ok(String::from_utf8(buffer).map_err(|_| StringError::InvalidUtf8)?)
			}
			StrEncoding::Utf16 => {
				let buffer = match len_encoding {
					StrLenEncoding::LenPrefixed => {
						let bytes = self.read_length()?;
						if bytes % 2 != 0 {
							return Err(EncodingError::StringError(StringError::InvalidUtf16));
						}
						let mut buffer = Vec::with_capacity(bytes);
						for _ in 0..(bytes / 2) {
							buffer.push(self.read_u16_direct(NumEncoding::Fixed, endianness)?);
						}
						buffer
					}
					StrLenEncoding::NullTerminated => {
						let mut buffer = Vec::new();
						loop {
							let val = self.read_u16_direct(NumEncoding::Fixed, endianness)?;
							if val == 0 {
								break;
							}
							
							buffer.push(val)
						}
						buffer
					},
				};
				
				Ok(String::from_utf16(&buffer).map_err(|_| StringError::InvalidUtf16)?)
			}
			StrEncoding::Utf32 => {
				let buffer = match len_encoding {
					StrLenEncoding::LenPrefixed => {
						let bytes = self.read_length()?;
						if bytes % 4 != 0 {
							return Err(EncodingError::StringError(StringError::InvalidUtf16));
						}
						let mut buffer = Vec::with_capacity(bytes);
						for _ in 0..(bytes / 4) {
							buffer.push(char::from_u32(self.read_u32_direct(NumEncoding::Fixed, endianness)?).ok_or(StringError::InvalidUtf32)?);
						}
						buffer
					}
					StrLenEncoding::NullTerminated => {
						let mut buffer = Vec::new();
						loop {
							let val = self.read_u32_direct(NumEncoding::Fixed, endianness)?;
							if val == 0 {
								break;
							}

							buffer.push(char::from_u32(val).ok_or(StringError::InvalidUtf32)?)
						}
						buffer
					},
				};

				Ok(buffer.iter().collect())
			}
		}
	}

	/// Reads `buf.len()` bytes from the stream to the buffer as-is.
	pub fn read_raw_bytes(&mut self, buf: &mut [u8]) -> EncodingResult<(), T::Error> {
		Ok(self.stream.read_exact(buf)?)
	}
}

/// Represents any kind of error that can happen during encoding and decoding
#[derive(Debug, Display)]
pub enum EncodingError<E: Error> {
	/// Generic IO error
	#[display("IO Error occurred: {:0?}")]
	IOError(
		E
	),
	/// The end of the file or buffer was reached but more data was expected
	#[display("Unexpected end of file/buffer")]
	UnexpectedEOF,
	/// A var-int was malformed and could not be decoded
	#[display("Malformed var-int encoding")]
	VarIntError,
	/// An invalid character value was read
	#[display("Invalid char value")]
	InvalidChar,
	/// A value other than `1` or `0` was read while decoding a `bool`
	#[display("Invalid bool value")]
	InvalidBool,
	/// An attempt was made to encode or decode a string, but *something* went wrong.
	#[display("String error: {0}")]
	StringError(
		StringError
	),
	/// Tried to write or read a length greater than the max
	#[display("A length of {requested} exceeded the max allowed value of {max}")]
	MaxLengthExceeded {
		max: usize,
		requested: usize
	},
	/// Tried to decode an unrecognized enum variant
	#[display("Unrecognized enum variant")]
	InvalidVariant,
	/// An attempt was made to flatten an option or result, but the inner value was unexpected.
	/// Example: `#[ende(flatten: some)]` applied on an `Option` containing the `None` variant
	#[display("Flatten error: {0}")]
	FlattenError(
		FlattenError
	),
	/// An attempt was made to lock a RefCell/Mutex/RwLock or similar, but it failed.
	#[display("Lock error: couldn't lock a RefCell/Mutex/RwLock or similar")]
	LockError,
	/// A piece of data couldn't be borrowed from the encoder. This is a recoverable error,
	/// meaning the decoding operation can be attempted again with a non-borrowing function.
	#[display("Borrow error: zero-copy decoding not supported for this value")]
	BorrowError,
	/// A `#[ende(validate = ...)]` check failed
	#[display("Validation error: {0}")]
	ValidationError(
		#[cfg(feature = "alloc")]
		alloc::string::String,
		#[cfg(not(feature = "alloc"))]
		&'static str,
	),
	/// A generic serde error occurred
	#[cfg(all(feature = "serde", feature = "alloc"))]
	#[cfg_attr(doc_cfg, doc(cfg(feature = "serde")))]
	#[display("Serde error: {0}")]
	SerdeError(
		alloc::string::String
	),
	/// A generic serde error occurred
	#[cfg(all(feature = "serde", not(feature = "alloc")))]
	#[cfg_attr(doc_cfg, doc(cfg(feature = "serde")))]
	#[display("Serde error")]
	SerdeError,
}

impl<E: Error> EncodingError<E> {
	pub fn validation_error<'a>(fmt: fmt::Arguments<'a>) -> Self {
		#[cfg(feature = "alloc")]
		{
			use alloc::string::ToString;
			Self::ValidationError(fmt.to_string())
		}
		#[cfg(not(feature = "alloc"))]
		{
			if let Some(str) = fmt.as_str() {
				Self::ValidationError(str)
			} else {
				Self::ValidationError("Unknown")
			}
		}
	}
}

impl<T: Error> Error for EncodingError<T> {
	fn kind(&self) -> ErrorKind {
		match self {
			EncodingError::IOError(io_error) => io_error.kind(),
			EncodingError::UnexpectedEOF => ErrorKind::Other,
			EncodingError::FlattenError(_) => ErrorKind::InvalidInput,
			EncodingError::LockError => ErrorKind::Other,
			EncodingError::BorrowError => ErrorKind::Other,
			#[cfg(all(feature = "serde", feature = "alloc"))]
			EncodingError::SerdeError(_) => ErrorKind::Other,
			#[cfg(all(feature = "serde", not(feature = "alloc")))]
			EncodingError::SerdeError => ErrorKind::Other,
			_ => ErrorKind::InvalidData,
		}
	}
}

#[cfg(feature = "unstable")]
impl<E: Error> core::error::Error for EncodingError<E> {}

#[cfg(all(not(feature = "unstable"), feature = "std"))]
impl<E: Error> std::error::Error for EncodingError<E> {}

impl<E: Error> From<E> for EncodingError<E> {
	fn from(value: E) -> Self {
		Self::IOError(value)
	}
}

impl<E: Error> From<ReadExactError<E>> for EncodingError<E> {
	fn from(value: ReadExactError<E>) -> Self {
		match value {
			ReadExactError::UnexpectedEof => Self::UnexpectedEOF,
			ReadExactError::Other(io_error) => Self::IOError(io_error),
		}
	}
}

impl<E: Error> From<StringError> for EncodingError<E> {
	fn from(value: StringError) -> Self {
		Self::StringError(value)
	}
}

impl<E: Error> From<FlattenError> for EncodingError<E> {
	fn from(value: FlattenError) -> Self {
		Self::FlattenError(value)
	}
}
/// Represents an error occurred while encoding or decoding a string, including intermediate
/// conversion errors and the presence of null bytes in unexpected scenarios.
#[derive(Debug, Display)]
pub enum StringError {
	/// A string contained non-ascii characters
	#[display("Invalid ASCII characters in string data")]
	InvalidAscii,
	/// A string couldn't be converted to-from utf8 (necessary step for the rust string type)
	#[display("Invalid UTF-8 characters in string data")]
	InvalidUtf8,
	/// A string contained invalid UTF-16 data
	#[display("Invalid UTF-16 characters in string data")]
	InvalidUtf16,
	/// A string contained invalid UTF-32 data
	#[display("Invalid UTF-32 characters in string data")]
	InvalidUtf32,
	/// A c-like string contained zeroes
	#[display("Null-terminated string contained a null *inside*")]
	InvalidCString,
}

#[cfg(feature = "unstable")]
impl core::error::Error for StringError {}

#[cfg(all(not(feature = "unstable"), feature = "std"))]
impl std::error::Error for StringError {}

/// Represents an error related to the "flatten" functionality, with potentially useful diagnostics
#[derive(Debug, Display)]
pub enum FlattenError {
	/// A value other than `1` or `0` was read from the `flatten` state variable
	#[display("Invalid bool value")]
	InvalidBool,
	#[display("Boolean state mismatch: expected {expected}, got {got}")]
	BoolMismatch {
		expected: bool,
		got: bool,
	},
	#[display("Length mismatch: expected {expected}, got {got}")]
	LenMismatch {
		expected: usize,
		got: usize,
	}
}

#[cfg(feature = "unstable")]
impl core::error::Error for FlattenError {}

#[cfg(all(not(feature = "unstable"), feature = "std"))]
impl std::error::Error for FlattenError {}

/// A convenience alias to `Result<T, EncodingError>`
pub type EncodingResult<T, E> = Result<T, EncodingError<E>>;

/// A binary data structure specification which can be **encoded** into its binary representation.
pub trait Encode {
	/// Encodes `self` into its binary format.<br>
	/// As a baseline, calling `encode` multiple times on the same value without
	/// changing the encoder settings or the value itself in-between calls must produce
	/// the same output.<br>
	/// If the result is Ok,
	/// implementations should guarantee that the state of the encoder
	/// is preserved. If the result is Err,
	/// no guarantees are made about the state of the encoder,
	/// and users should reset it before reuse.<br>
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error>;
}

/// A binary data structure specification which can be **decoded** from its binary representation.
pub trait Decode: Sized {
	/// Decodes `Self` from a binary format.<br>
	/// As a baseline, calling `decode` multiple times without changing the
	/// encoder settings or the underlying binary data in-between calls must produce
	/// the same output.<br>
	/// If the result is Ok,
	/// implementations should guarantee that the state of the encoder
	/// is preserved. If the result is Err,
	/// no guarantees are made about the state of the encoder,
	/// and users should reset it before reuse.<br>
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error>;
}