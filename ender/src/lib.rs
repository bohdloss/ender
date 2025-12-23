#![cfg_attr(feature = "unstable", feature(doc_cfg))]
#![cfg_attr(feature = "unstable", feature(never_type))]
#![cfg_attr(not(feature = "std"), no_std)]

//! A rust **EN**coding and **DE**coding library for writing custom protocols and file formats.
//!
//! It aims to be **intuitive**, **expandable** and **correct**.
//!
//! # Example
//!
//! ```rust
//! # use ender::*;
//! # use ender::io::*;
//! # #[derive(Debug, PartialEq, Encode, Decode)]
//! # #[ender(crate: ender)]
//! # struct Person {
//! # 	name: String,
//! #    age: u64,
//! #    height: f32,
//! #    eye_color: EyeColor,
//! # }
//! #
//! # #[derive(Debug, PartialEq, Encode, Decode)]
//! # #[ender(crate: ender)]
//! # enum EyeColor {
//! #    Brown,
//! #    Blue,
//! #    Green,
//! #    Gray,
//! #    Black
//! # }
//! #
//! # fn hidden() -> EncodingResult<()> {
//! let mut the_matrix = vec![0u8; 256];
//!
//! // Summon John into existence!
//! let john = Person {
//!     name: String::from("John"),
//!     age: 35,
//! 	height: 1.75,
//! 	eye_color: EyeColor::Brown
//! };
//!
//! // Encode John into the matrix
//! encode_with(SliceMut::new(&mut the_matrix), Context::default(), &john)?;
//!
//! // Bring him back
//! let john_2: Person = decode_with(Slice::new(&the_matrix), Context::default())?;
//!
//! // But is he really the same John?
//! assert_eq!(john, john_2);
//! # Ok(())
//! # }
//! # fn main() { hidden().unwrap() }
//! ```
//!
//! # Encoding format
//!
//! The encoding process aims at being **correct** and **unsurprising**.
//!
//!
//!
//! Ende supports a series of options, which can be changed *during*
//! the encoding/decoding process to get certain parts of a binary format
//! to be represented exactly how you want them.
//!
//! This can be done in a manual implementation as well as with the derive
//! macro, using the custom attributes provided.
//!
//! Certain types also support "flattening", which means omitting information
//! known from the context.
//! For instance, you can omit writing whether an `Option` is present if
//! that information is already stored somewhere else in the file format.
//!
//! For *integer primitives*, *usize*, and *enum variants* you can customize the endianness,
//! the numerical encoding (read: var-ints), the bit-width (how many bytes
//! does a `usize` or enum variant take up in your encoding format?),
//! the max-size (to prevent maliciously crafted binary formats to cause
//! unlimited-size allocations).
//!
//! ### Var-int format
//! - Fixed - not var-int, simply encode the number as-is
//! - Leb128
//! - Protobuf - both its zigzag and "wasteful" variants
//!
//! ### String formats
//! As for strings, currently length-prefixed, null-terminated (with and without
//! a maximum length) strings are supported, as well as the following encoding formats.
//! - Ascii
//! - Utf8
//! - Utf16
//! - Utf32
//! - Windows1252
//!
//! If you need a new var-int encoding or string encoding added, feel free
//! to open a PR!
//!
//! # Motivation
//!
//! One of the main reasons I made this library is because I found myself
//! needing more sophisticate macros and runtime flexibility for existing
//! binary formats.
//!
//! While for example [`bincode`](https://crates.io/crates/bincode) is perfectly
//! ok for many applications, `ender` was made with compatibility with existing
//! data formats in mind.
//!
//! For this very purpose, many internal details of the encoder are exposed
//! through settings or the derive macros themselves, for the purpose of fine-tuning
//! the data format exactly how you want it, while providing an easy-to-understand interface.
//!
//! # Deriving
//!
//! A big selling point of `ender` are its macros, which allow you to heavily
//! customize the codegen through a series of attributes.
//! To learn more about those, check out `DERIVE.md` in this crate's repository root.
//!
//! # MSRV
//!
//! This crate will always target the latest version of rust, in order
//! to get access to new features as soon as they are released and
//! update the code accordingly if beneficial.
//! Of course, breaking API changes will be accompanied by a major version
//! bump.
//!
//! # Future plans
//!
//! I plan on adding support for `async` io through a feature gate.

#[cfg(feature = "alloc")]
extern crate alloc;

use core::any::Any;
use core::fmt::Debug;
use core::hash::Hash;
use core::mem::replace;

use parse_display::Display;

/// # The Ender Derivonomicon
///
/// The following is a full and up-to-date breakdown of the `Encode` and `Decode` derive macros,
/// their attributes, how they work and all the possible ways you can modify the codegen.
///
/// ## Flags Essentials
///
/// All flags follow the following format:
/// `#[ender(flag1; flag2; flag2; ...)]`
///
/// The 2 special flags `en` and `de`, called Scope flags, can be used only at the beginning
/// of the list to indicate that all the flags in the attribute declaration only apply to the
/// encoding process (`en`) or the decoding process (`de`).
///
/// If neither of those flags are specified, then it is assumed that all the flags in the
/// attribute declaration apply to both encoding and decoding.
///
/// Whenever a flag is said to accept an `$expr`, this means any expression is accepted,
/// and that it will have access to an immutable reference to all the fields that have been
/// decoded so far (actually all the fields while encoding), but the `validate` flag additionally
/// provides a reference to the field it is applied on.<br>
/// If the fields are part of a tuple struct/variant, the references will be named `m{idx}` where `idx` are the
/// indexes of the tuple fields (E.G. `m0`, `m1`, ...), otherwise their names will match those of the fields themselves.
///
/// A seeking impl is an implementation of [`Encode`] or [`Decode`] that has an additional [`Seek`]
/// trait bound
/// When a flag is said to be a `seek` flag, it means that when used anywhere it will switch the
/// impl to a seeking impl.
///
/// A borrowing impl is an implementation of [`Encode`] or [`Decode`] that has a [`BorrowRead`]
/// trait bound rather than a [`Read`] one.
/// When a flag is said to be a `borrow` flag, it means that when used anywhere it will switch the
/// impl to a borrowing impl.
///
/// Borrowing and seeking impls can be combined.
///
/// The flags currently implemented are split into 5 groups:
/// # 1. Setting Modifiers
/// Setting-Modifier flags temporarily change certain settings of the encoder and can be applied
/// to Fields or Items (the whole struct or enum).<br>
/// Multiple can be specified at the same time, as long as they don't overlap.<br>
/// All setting modifiers follow the `$target: $mod1, $mod2, ...` pattern.
///
/// - Endianness modifiers: `big_endian`, `little_endian`
///   - Available targets:
///     - `num`
///     - `size`
///     - `variant`
///     - `string`
/// - Numerical encoding modifiers: `fixed`, `leb128`, `protobuf_wasteful`, `protobuf_zz`
///   - Available targets:
///     - `num`,
///     - `size`,
///     - `variant`
/// - Bit-width modifiers: `bit8`, `bit16`, `bit32`, `bit64`, `bit128`
///   - Available targets:
///     - `size`
///     - `variant`
/// - Max-size modifier: `max = $expr`
///   - Available targets:
///     - `size`
/// - String encoding modifier: `ascii`, `utf8`, `utf16`, `utf32`, `windows1252`
///   - Available targets:
///     - `string`
/// - String length encoding modifier: `len_prefix`, `null_term`, `null_term($max:expr)`
///   - Available targets:
///     - `string`
///     <br>
/// ### Example:
/// ```rust
/// # use ender::{Encode, Decode};
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// /// The variants of this enum will be encoded in the little endian ordering,
/// /// using a fixed numerical encoding and a 32-bit width.
/// #[ender(variant: little_endian, fixed, bit32)]
/// enum MyEnum {
///     VariantA {
///         /// The length of this String will be encoded using big endian ordering,
///         /// fixed numerical encoding and 16-bit width, with a max length of 100
///         #[ender(size: big_endian, fixed, bit16, max = 100)]
///         field: String,
///         /// Encode this String with utf16 big endian, and prefix it with its length
///         #[ender(string: utf16, big_endian, len_prefix)]
///         utf_16: String,
///         /// Encode this String as an utf8 null-terminated string
///         #[ender(string: utf8, null_term)]
///         utf_8: String,
///         /// Encode this String as an utf8 null-terminated string with a fixed length of `15`
///         /// Exactly `15` bytes will be read in all cases, and the length of the string is given
///         /// by the first occurrence of a null byte or the `15` byte mark.
///         #[ender(string: utf8, null_term(15))]
///         max_15: String,
///     },
///     VariantB {
///         /// This number will be encoded using little endian ordering, and the
///         /// leb128 [NumEncoding][`ender::NumEncoding`]
///         #[ender(num: little_endian, leb128)]
///         field: u64,
///         #[ender(num: protobuf_zz)]
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
/// modifiers (see [ambiguous example](#ambiguous-example)).<br>
/// * `redir: $path(...)` - Uses the given path to find an encoding/decoding function which
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
/// * `ptr $seek: $expr` - This is a `seek` flag. Seeks to the location given by $expr
/// (which must be of type usize or isize) relative to $seek - which can be
/// "start", "end" or "cur"rrent - before encoding/decoding this field, then seeks back to the
/// previous location.
/// ### Example:
/// ```rust
/// # use ender::{Encode, Decode};
/// # use ender::facade::fake::*;
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// struct MyStruct {
///     secret_key: Vec<u8>,
///     iv: Vec<u8>,
///     /// While **encoding**, this field is compressed -> encrypted.
///     /// While **decoding**, this field is decrypted -> decompressed.
///     #[ender(redir: gzip(9))]
///     #[ender(redir: aes(iv, secret_key))]
///     super_secret_data: Vec<u8>,
///     file_pointer: usize,
///     /// Marks the current offset, seeks to `file_pointer` bytes from the start of the file,
///     /// encodes/decodes the field, then seeks back.
///     #[ender(ptr start: *file_pointer)]
///     apple_count: u64,
///     /// This field is effectively laid *right after* `file_pointer`
///     /// in the binary representation.
///     other_data: i32,
/// }
/// ```
/// ### Ambiguous example:
/// ```rust
/// # use ender::{Encode, Decode};
/// # use ender::facade::fake::*;
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// /// Because of the priority rules of items over fields, this is ambiguous, see below
/// #[ender(redir: gzip(9))]
/// struct MyStruct {
///     /// While **encoding**, this field is encrypted -> compressed.
///     /// While **decoding**, this field is decompressed -> decrypted.
///     /// Because the "compressed" flag is declared before the "encrypted" flag, one might
///     /// think they are applied in that order. However, since the item-level flag takes priority,
///     /// it is applied *after* the field flag.
///     ///
///     /// According to your needs, this might not be what you want, so be careful when mixing
///     /// item-level and field-level stream modifiers.
///     #[ender(redir: aes(&[], &[]))]
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
/// `fn<T: Write>(&V, &mut ender::Encoder<T>) -> EncodingResult<()>`
/// where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
///     * If the scope is Decode, the path must be callable as
/// `fn<T: Read>(&mut ender::Encoder<T>) -> EncodingResult<V>`
/// where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
///     * If no scope is specified, the path must point to a module with encoding and decoding functions
/// with the same signatures as above.
/// ### Example:
/// ```rust
/// # use ender::{Encode, Decode};
/// # use ender::facade::fake::rsa;
/// # use uuid::Uuid;
/// # use person_encoder::Person;
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// struct Friends {
/// 	/// Has Serialize/Deserialize implementations, but no Encode/Decode implementations.
///     /// A perfect fit for integrating with serde!
///     #[ender(serde)]
///     uuid: Uuid,
///     /// Here we demonstrate how the with flag changes based on whether a scope
///     /// is declared. This:
///     #[ender(with: person_encoder)]
///     friend1: Person,
///     /// ...is equivalent to this!
///     #[ender(en; with: person_encoder::encode)]
///     #[ender(de; with: person_encoder::decode)]
///     friend2: Person,
///     /// Not the smartest way to store a private key!
///     private_key: Vec<u8>,
///     public_key: Vec<u8>,
///     /// This block of data will be encrypted before being encoded using the public key,
///     /// and decrypted after being decoded using the private key.
///     #[ender(with: rsa(public_key, private_key))]
///     even_more_secret_data: Vec<u8>,
/// }
/// mod person_encoder {
/// #    use ender::io::{Write, Read};
/// #    use ender::{Encoder, EncodingResult, Encode};
/// #
/// #     pub struct Person {
/// #        name: String,
/// #        surname: String,
/// #        age: u32,
/// #     }
/// #
///      pub fn encode<T: Write>(person: &Person, encoder: &mut Encoder<T>) -> EncodingResult<()> {
///          /* ... */
/// #        person.name.encode(encoder)?;
/// #        person.surname.encode(encoder)?;
/// #        person.age.encode(encoder)?;
/// #        Ok(())
///      }
/// #
///      pub fn decode<T: Read>(encoder: &mut Encoder<T>) -> EncodingResult<Person> {
///          /* ... */
/// #        Ok(Person {
/// #            name: encoder.decode_value()?,
/// #            surname: encoder.decode_value()?,
/// #            age: encoder.decode_value()?,
/// #   	})
///      }
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
/// The conversion is done through the `Into` trait.
/// * `from: $ty` - Converts the value of the field to `$ty` before encoding it
/// and back to the original field type after decoding it.<br>
/// The conversion is done through the `From` trait.
/// ### Example:
/// ```rust
/// # use ender::{Encode, Decode};
/// #
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// struct Mountain {
///     /// Height is encoded as a `u16`, then decoded back to a `f64`.
///     /// These operations are performed using the `as` keyword.
///     #[ender(as: u16)]
///     height: f64,
///     /// Boulder is encoded as a `BigRock`, then decoded back to a `Boulder`.
///     /// This can be done because `BigRock` implements `From<Boulder>`, and
///     /// `Boulder` implements `From<BigRock>`.
///     #[ender(into: BigRock)]
///     boulder: Boulder,
/// }
///
/// /// Note: `BigRock` is required to implement `Encode` and `Decode`,
/// /// but `Boulder` is not.
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// struct BigRock {
///     weight: f64
/// }
///
/// /* From<Boulder> and From<BigRock> impls here... */
/// # impl From<Boulder> for BigRock {
/// #    fn from(value: Boulder) -> Self {
/// #   	Self {
/// #            weight: value.weight
/// #   	}
/// #    }
/// # }
/// #
/// # #[derive(Clone)]
/// # struct Boulder {
/// #    weight: f64,
/// #    radius: f64
/// # }
/// #
/// # impl From<BigRock> for Boulder {
/// #    fn from(value: BigRock) -> Self {
/// #   	Self {
/// #   		weight: value.weight,
/// #           radius: 1.0
/// #   	}
/// #    }
/// # }
/// ```
/// # 5. Helpers
/// Helper flags change certain parameters or add conditions for when a field
/// or item should be encoded/decoded.<br>
/// * `crate: $crate` - Overwrites the default crate name which is assumed to be `ender`.
/// Can only be applied to items.
/// * `if: $expr` - The field will only be encoded/decoded if the given expression
/// evaluates to true, otherwise the default value is computed.
/// * `default: $expr` - Overrides the default fallback for when a value can't be
/// decoded, which is `Default::default()`
/// * `skip` - Will not encode/decode this field.
/// When decoding, computes the default value.
/// * `validate: $expr, $format_string, $arg1, $arg2, $arg3, ...` - Before encoding/after decoding, returns an error if the
/// expression evaluates to false. The error message will use the given formatting (if present).
/// * `flatten: $expr` - Indicates that the length of the given field (for example
/// a Vec or HashMap) doesn't need to be encoded/decoded, because it is known from the context.
/// Can also be used with an `Option` in conjunction with the `if` flag and without the `$expr`
/// to indicate that the presence of an optional value is known from the context.
/// * `borrow: $lif1, $lif2, $lif3, ...` - This is a `borrow` flag. Indicates this field
/// should be decoded using its borrowing decode implementation, and allows you to optionally specify a
/// set of lifetimes to override those normally inferred by the macro. These lifetimes will be bound
/// to the lifetime of the encoder's data.
/// * `goto $seek: $expr` - This is a `seek` flag. Indicates a jump to a different stream position
/// before encoding this field or item.
/// $seek can be any of "start", "end" or "cur", while $expr must produce a value of
/// type usize or isize relative to $seek.<br>
/// If you need the stream position to be restored after encoding/decoding the field, see the
/// `ptr` *stream modifier`.
/// * `pos_tracker: $ident` - This is a `seek` flag. Stores the current stream position in a
/// variable with the given name.
/// Note that the position is stored *before* the `ptr` and `goto` flags, if any.
/// * `pull $temp as $ty: $var <= $expr` - Attempts to retrieve the `user` field from the context and
/// downcast it to `$ty`, early returning an error if it fails, and assigns it to the temporary variable `$temp`.
/// The `$expr` is executed and its value stored in a local variable `$val`.
/// This is useful for reading data from the context so that it is later available to other attributes.
/// This flag is applied *before* `push`
/// * `push $temp as $ty: $expr` - Attempts to retrieve the `user` field from the context and
/// downcast it to `$ty`, early returning an error if it fails, and assigns it to the temporary variable `$temp`.
/// The `$expr` is executed and its value ignored.
/// This is useful for writing data to the context.
/// * `seeking` - This is a `seek` flag. Does nothing, but simply forces a seeking impl to be used.
/// This can only be applied to the whole item, as it doesn't make sense on individual fields.
/// <br>
/// ### Example:
///
/// ```rust
/// # use std::borrow::Cow;
/// # use ender::{Encode, Decode};
/// # use uuid::Uuid;
/// /// Hehe >:3
/// extern crate ender as enderman;
///
/// #[derive(Encode, Decode)]
/// /// We specify the name of the re-exported ender crate.
/// #[ender(crate: enderman)]
/// /// We specify this should use a seeking impl
/// /// This is redundant of course, but we include it for completeness :P
/// #[ender(seeking)]
/// struct PersonEntry<'record> {
///   /// Will come in handy later
///   name_present: bool,
///   /// Similar to the previous example, but with the addition of the flatten flag!
///   /// We know a Uuid is always 16 bytes long, so we omit writing/reading that data.
///   #[ender(serde; flatten size: 16)]
///   uuid: Uuid,
///   /// Just the string version of the uuid, not present in the binary data.
///   /// Skip the Encoding step, and Decode it from the uuid.
///   #[ender(skip; default: uuid.to_string())]
///   uuid_string: String,
///   /// We know whether this is present from the context, therefore we don't write whether
///   /// the optional value is present, and when reading we assume it is.
///   /// Since the "if" flag is also present, the field will only be decoded if the expression
///   /// evaluates to true, making the previous operation safe
///   /// (no risk of decoding garbage data)
///   #[ender(flatten bool: true; if: * name_present)]
///   name: Option<String>,
///   /// Contains a file offset to the rest of the data
///   pointer_to_data: usize,
///   /// Go to the location in the specified file offset from this point onwards.
///   ///
///   /// This might be too long to clone from the decoder, so we borrow it instead.
///   /// Decode impl -> Cow::Owned -- (NOT YET - WILL WORK WHEN SPECIALIZATION IS STABILIZED)
///   /// BorrowDecode impl -> Cow::Borrowed
///   /// The macro will infer the borrow lifetime to be `'record`.
///   #[ender(goto start: *pointer_to_data; borrow)]
///   criminal_record: Cow<'record, str>,
///   /// Only present if the name is also present, but we want to provide a custom default!
///   #[ender(default: String::from("Smith"); if: * name_present)]
///   surname: String,
///   /// No-one allowed before 18!
///   #[ender(validate: * age >= 18, "User is too young: {}", age)]
///   age: u32,
///   /// This is temporary data, we don't care about including it in the binary format.
///   #[ender(skip; default: 100)]
///   health: u64,
/// }
/// ```
/// # Relationship between seek flags
///
/// ```
/// # use ender::{Decode, Encode};
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// /// This is the same...
/// struct Ptr {
///     pointer: usize,
///     #[ender(ptr start: *pointer)]
///     data: /* ... */
/// #   (),
/// }
///
/// #[derive(Encode, Decode)]
/// # #[ender(crate: ender)]
/// /// As this!
/// struct Goto {
///     pointer: usize,
///     #[ender(pos_tracker: current)]
///     #[ender(goto start: *pointer)]
///     data: /* ... */
/// #   (),
///     #[ender(goto start: current)]
///     seek_back: (),
/// }
/// ```
#[cfg(feature = "derive")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "derive")))]
pub use ender_derive::{Decode, Encode};
pub use error::*;
pub use opaque::*;
pub use convenience::*;

use crate::io::{BorrowRead, Read, Seek, SeekFrom, SizeLimit, SizeTrack, Write, Zero};

#[cfg(test)]
mod test;

mod error;
pub mod facade;
mod impls;
pub mod io;
mod opaque;
#[cfg(feature = "serde")]
mod serde;
mod source;
mod windows1252;
mod convenience;

/// Controls the endianness of a numerical value. Endianness is just
/// the order in which the value's bytes are written.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default, Display)]
pub enum Endianness {
    /// Least significant byte first
    LittleEndian,
    /// Most significant byte first
    #[default]
    BigEndian,
}

impl Endianness {
    /// Returns the native endianness for the target system.
    #[inline]
    pub const fn native() -> Self {
        #[cfg(target_endian = "little")]
        {
            Self::LittleEndian
        }
        #[cfg(target_endian = "big")]
        {
            Self::BigEndian
        }
    }
}

/// Controls the encoding of a numerical value. For instance, controls whether the numbers
/// are compressed through a var-int format or if the entire length of their value is encoded.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default, Display)]
#[non_exhaustive]
pub enum NumEncoding {
    /// The value's bits are encoded as-is according to the [`Endianness`].
    #[default]
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

impl NumEncoding {
    /// Determines whether a slice encoded with this encoding can be directly borrowed.
    ///
    /// Currently only returns true with the [`Fixed`][`NumEncoding::Fixed`] variant
    #[inline]
    pub const fn borrowable(&self) -> bool {
        match self {
            NumEncoding::Fixed => true,
            _ => false,
        }
    }
}

/// How many bits a size or enum variant will occupy in the binary format. If the value
/// contains more bits, they will be trimmed (lost), so change this value with care
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug, Default, Display)]
pub enum BitWidth {
    /// Max 8 bits per value
    #[display("8Bit")]
    Bit8,
    /// Max 16 bits per value
    #[display("16Bit")]
    Bit16,
    /// Max 32 bits per value
    #[display("32Bit")]
    Bit32,
    /// Max 64 bits per value
    #[display("64Bit")]
    #[default]
    Bit64,
    /// Max 128 bits per value
    #[display("128Bit")]
    Bit128,
}

impl BitWidth {
    /// Returns the native bit-width of the [`usize`] and [`isize`] types for the target system.
    ///
    /// [`usize`]: prim@usize
    /// [`isize`]: prim@isize
    #[inline]
    pub const fn native() -> Self {
        #[cfg(target_pointer_width = "64")]
        {
            Self::Bit64
        }
        #[cfg(target_pointer_width = "32")]
        {
            Self::Bit32
        }
        #[cfg(target_pointer_width = "16")]
        {
            Self::Bit16
        }
    }

    /// Returns the number of bits represented by this bit-width.
    ///
    /// # Example
    ///
    /// ```rust
    /// use ender::BitWidth;
    /// let sixteen = BitWidth::Bit16;
    ///
    /// assert_eq!(sixteen.bits(), 16);
    /// ```
    #[inline]
    pub const fn bits(&self) -> usize {
        match self {
            BitWidth::Bit8 => 8,
            BitWidth::Bit16 => 16,
            BitWidth::Bit32 => 32,
            BitWidth::Bit64 => 64,
            BitWidth::Bit128 => 128,
        }
    }

    /// Returns the number of bytes represented by this bit-width.
    ///
    /// # Example
    ///
    /// ```rust
    /// use ender::BitWidth;
    /// let eight_bits = BitWidth::Bit8;
    /// let thirtytwo_bits = BitWidth::Bit32;
    ///
    /// assert_eq!(eight_bits.bytes(), 1);
    /// assert_eq!(thirtytwo_bits.bytes(), 4);
    /// ```
    #[inline]
    pub const fn bytes(&self) -> usize {
        match self {
            BitWidth::Bit8 => 1,
            BitWidth::Bit16 => 2,
            BitWidth::Bit32 => 4,
            BitWidth::Bit64 => 8,
            BitWidth::Bit128 => 16,
        }
    }
}

/// The encoding method use for the length of a string.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default, Display)]
pub enum StrLen {
    /// The length of the string is stored before its contents as an `usize`.
    #[default]
    LengthPrefixed,
    /// The length of the string is obtained by finding the first occurrence of `n`
    /// number of null bytes, where `n` is the length in bytes of one code unit for
    /// the given string encoding (see [`StrEncoding::bytes`]).
    NullTerminated,
    /// Like [`StrLen::NullTerminated`], but the string always occupies `n` bytes,
    /// where the last bytes are filled with null-bytes if the length of the string after
    /// being encoded is less than `n` bytes.
    #[display("NullTerminatedFixed({0})")]
    NullTerminatedFixed(usize),
}

/// The encoding method used for strings and chars.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default, Display)]
#[non_exhaustive]
pub enum StrEncoding {
    Ascii,
    /// See [UTF-8](https://en.wikipedia.org/wiki/UTF-8)
    #[default]
    Utf8,
    /// See [UTF-16](https://en.wikipedia.org/wiki/UTF-16)
    Utf16,
    /// See [UTF-32](https://en.wikipedia.org/wiki/UTF-32)
    Utf32,
    Windows1252,
}

impl StrEncoding {
    /// Returns the number of bytes of each **code unit** for this encoding.
    #[inline]
    pub const fn bytes(&self) -> usize {
        match self {
            StrEncoding::Ascii => 1,
            StrEncoding::Utf8 => 1,
            StrEncoding::Utf16 => 2,
            StrEncoding::Utf32 => 4,
            StrEncoding::Windows1252 => 1,
        }
    }
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
    #[inline]
    pub const fn new() -> Self {
        Self {
            endianness: Endianness::LittleEndian,
            num_encoding: NumEncoding::Fixed,
        }
    }

    /// Sets the **endianness**, then returns self.
    #[inline]
    pub const fn endianness(mut self, endiannes: Endianness) -> Self {
        self.endianness = endiannes;
        self
    }

    /// Sets the **numerical encoding**, then returns self.
    #[inline]
    pub const fn num_encoding(mut self, num_encoding: NumEncoding) -> Self {
        self.num_encoding = num_encoding;
        self
    }
}

impl Default for NumRepr {
    #[inline]
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
    pub max_size: usize,
}

impl SizeRepr {
    /// Returns the default size representation: little endian, fixed encoding, 64 bit width,
    /// and the max size set to `usize::MAX`
    #[inline]
    pub const fn new() -> Self {
        Self {
            endianness: Endianness::LittleEndian,
            num_encoding: NumEncoding::Fixed,
            width: BitWidth::Bit64,
            max_size: usize::MAX,
        }
    }

    /// Sets the **endianness**, then returns self.
    #[inline]
    pub const fn endianness(mut self, endiannes: Endianness) -> Self {
        self.endianness = endiannes;
        self
    }

    /// Sets the **numerical encoding**, then returns self.
    #[inline]
    pub const fn num_encoding(mut self, num_encoding: NumEncoding) -> Self {
        self.num_encoding = num_encoding;
        self
    }

    /// Sets the **bit width**, then returns self.
    #[inline]
    pub const fn bit_width(mut self, bit_width: BitWidth) -> Self {
        self.width = bit_width;
        self
    }

    /// Sets the **max size**, then returns self.
    #[inline]
    pub const fn max_size(mut self, max_size: usize) -> Self {
        self.max_size = max_size;
        self
    }
}

impl Default for SizeRepr {
    #[inline]
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
    pub width: BitWidth,
}

impl VariantRepr {
    /// Returns the default variant representation: little endian, fixed encoding and 32 bit width
    #[inline]
    pub const fn new() -> Self {
        Self {
            endianness: Endianness::LittleEndian,
            num_encoding: NumEncoding::Fixed,
            width: BitWidth::Bit32,
        }
    }

    /// Sets the **endianness**, then returns self.
    #[inline]
    pub const fn endianness(mut self, endiannes: Endianness) -> Self {
        self.endianness = endiannes;
        self
    }

    /// Sets the **numerical encoding**, then returns self.
    #[inline]
    pub const fn num_encoding(mut self, num_encoding: NumEncoding) -> Self {
        self.num_encoding = num_encoding;
        self
    }

    /// Sets the **bit width**, then returns self.
    #[inline]
    pub const fn bit_width(mut self, bit_width: BitWidth) -> Self {
        self.width = bit_width;
        self
    }
}

impl Default for VariantRepr {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

/// Controls the binary representation of strings.
/// Specifically, controls the [`StrEncoding`] of strings and chars and the [`Endianness`]
/// in which the encoded bytes are ordered.
///
/// Keep in mind not all encodings support null terminated strings, because
/// the encoding format may have the capability to contain nulls.<br>
/// In such cases, the encoding process will produce an error in case the encoded string contains
/// null characters, and the end of the string is encoded as a sequence of nulls of the appropriate
/// length (1 byte for UTF-8 and ASCII, 2 bytes for UTF-16, 4 bytes for UTF-32)
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("encoding = {encoding}, endianness = {endianness}, len = {len}")]
pub struct StringRepr {
    pub encoding: StrEncoding,
    pub endianness: Endianness,
    pub len: StrLen,
}

impl StringRepr {
    /// Returns the default string representation: utf-8, length-prefixed, little_endian
    #[inline]
    pub const fn new() -> Self {
        Self {
            encoding: StrEncoding::Utf8,
            endianness: Endianness::LittleEndian,
            len: StrLen::LengthPrefixed,
        }
    }

    /// Sets the **string encoding**, then returns self.
    #[inline]
    pub const fn str_encoding(mut self, str_encoding: StrEncoding) -> Self {
        self.encoding = str_encoding;
        self
    }
    
    /// Sets the **endianness**, then returns self.
    #[inline]
    pub const fn endianness(mut self, endiannes: Endianness) -> Self {
        self.endianness = endiannes;
        self
    }

    /// Sets the **string length encoding**, then returns self.
    #[inline]
    pub const fn len_encoding(mut self, len_encoding: StrLen) -> Self {
        self.len = len_encoding;
        self
    }
}

impl Default for StringRepr {
    #[inline]
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
    #[inline]
    pub const fn new() -> Self {
        Self {
            num_repr: NumRepr::new(),
            size_repr: SizeRepr::new(),
            variant_repr: VariantRepr::new(),
            string_repr: StringRepr::new(),
        }
    }

    /// Sets the **number** representation settings, then returns self.
    #[inline]
    pub const fn num_repr(mut self, num_repr: NumRepr) -> Self {
        self.num_repr = num_repr;
        self
    }

    /// Sets the **size** representation settings, then returns self.
    #[inline]
    pub const fn size_repr(mut self, size_repr: SizeRepr) -> Self {
        self.size_repr = size_repr;
        self
    }

    /// Sets the **variant** representation settings, then returns self.
    #[inline]
    pub const fn variant_repr(mut self, variant_repr: VariantRepr) -> Self {
        self.variant_repr = variant_repr;
        self
    }

    /// Sets the **string** representation settings, then returns self.
    #[inline]
    pub const fn string_repr(mut self, string_repr: StringRepr) -> Self {
        self.string_repr = string_repr;
        self
    }
}

impl Default for BinSettings {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

/// The state of the encoder, including its options and a `flatten` state variable
#[derive(Copy, Clone, Debug, Default)]
#[non_exhaustive]
pub struct Context<'a> {
    /// User provided data. This can be purposed for storing any kind of data,
    /// like cryptographic keys that are unknown to the data structures but known
    /// at a higher level.
    pub user: Option<&'a dyn Any>,
    /// The actual settings, which determine the numerical representations and the string
    /// representations.
    ///
    /// Implementations of [`Encode`] and [`Decode`] are required to
    /// preserve the state of the settings, even though they are allowed to temporarily modify it.
    ///
    /// In case of an error occurring, no guarantee is made about the state of the settings:
    /// for this reason it's good practice to store a copy of the settings somewhere.
    pub settings: BinSettings,
    /// The `bool` flatten state variable.
    ///
    /// When present, for `Option`, `Result` and any `bool` it indicates,
    /// while **Encoding** not to write the value,
    /// and while **Decoding** it contains the boolean value itself
    /// (it won't be read from the stream).
    pub bool_flatten: Option<bool>,
    /// The Variant flatten state variable.
    ///
    /// When present, for any enum it indicates,
    /// while **Encoding** not to write the value of the discriminant,
    /// and while **Decoding** it contains the discriminant value itself
    /// (it won't be read from the stream).
    pub variant_flatten: Option<Opaque>,
    /// The `usize` flatten state variable.
    ///
    /// When present, for `Vec`, `HashMap` and other data
    /// structures with a length it indicates while **Encoding** not to write said length,
    /// and while **Decoding** it contains the length itself
    /// (it won't be read from the stream).
    pub size_flatten: Option<usize>,
}

impl<'a> Context<'a> {
    /// Constructs the default encoder state. Options will be set to default, flatten to None.
    #[inline]
    pub const fn new() -> Self {
        Self {
            user: None,
            settings: BinSettings::new(),
            bool_flatten: None,
            variant_flatten: None,
            size_flatten: None,
        }
    }

    /// Replaces the settings with `settings`, then returns self.
    #[inline]
    pub const fn settings(mut self, settings: BinSettings) -> Self {
        self.settings = settings;
        self
    }

    /// Replaces the user data with `data`, then returns self.
    #[inline]
    pub const fn user_data<'b>(self, data: &'b dyn Any) -> Context<'b> {
        let this = Context {
            user: Some(data),
            settings: self.settings,
            bool_flatten: self.bool_flatten,
            variant_flatten: self.variant_flatten,
            size_flatten: self.size_flatten,
        };
        this
    }

    /// Replaces the bool flatten state variable with `Some(value)`, then returns self.
    #[inline]
    pub const fn bool_flatten(mut self, value: bool) -> Self {
        self.bool_flatten = Some(value);
        self
    }

    /// Replaces the variant flatten state variable with `Some(value)`, then returns self.
    /// 
    /// **Beware of providing an [`Opaque`] with an integer literal without a specific type!**
    /// 
    /// By default, rust will infer `i32` as the type, thus it will be converted to a *Signed*
    /// enum variant value, and you will get a (bad) surprise when you try to then encode/decode
    /// an enum that uses *Unsigned* variant discriminants (most enums).
    /// 
    /// How to avoid this?
    /// - Write the type in the num literal E.G. `Opaque::from(3u32)` or `Opaque::from(3 as u32)`
    /// - Better yet, use an explicit opaque value ([`Opaque::signed`], [`Opaque::unsigned`])
    #[inline]
    pub const fn variant_flatten(mut self, value: Opaque) -> Self
    {
        self.variant_flatten = Some(value);
        self
    }

    /// Replaces the size flatten state variable with `Some(value)`, then returns self.
    #[inline]
    pub const fn size_flatten(mut self, value: usize) -> Self {
        self.size_flatten = Some(value);
        self
    }
    
    /// Just like [`Self::new`] but uses the given settings instead of the default.
    #[inline]
    pub fn with_settings(settings: BinSettings) -> Self {
        Self {
            user: None,
            settings,
            bool_flatten: None,
            variant_flatten: None,
            size_flatten: None,
        }
    }

    /// Just like [`Self::new`] but uses the given settings instead of the default
    /// and the given user data.
    #[inline]
    pub fn with_user_data(settings: BinSettings, data: &'a dyn Any) -> Self {
        Self {
            user: Some(data),
            settings,
            bool_flatten: None,
            variant_flatten: None,
            size_flatten: None,
        }
    }

    /// Resets the context to its defaults, then overwrites the options with the given options.
    #[inline]
    pub fn reset(&mut self, options: BinSettings) {
        self.settings = options;
        self.bool_flatten = None;
        self.variant_flatten = None;
        self.size_flatten = None;
    }

    /// Returns the state of the [`bool`] flatten variable, consuming it.
    #[inline]
    pub fn consume_bool_flatten(&mut self) -> Option<bool> {
        replace(&mut self.bool_flatten, None)
    }

    /// Returns the state of the Variant flatten variable, consuming it.
    #[inline]
    pub fn consume_variant_flatten(&mut self) -> Option<Opaque> {
        replace(&mut self.variant_flatten, None)
    }

    /// Returns the state of the [`usize`] flatten variable, consuming it.
    #[inline]
    pub fn consume_size_flatten(&mut self) -> Option<usize> {
        replace(&mut self.size_flatten, None)
    }
}

/// The base type for encoding/decoding. Wraps a stream, and a [`Context`].<br>
/// It's recommended to wrap the stream in a [`std::io::BufReader`] or [`std::io::BufWriter`],
/// because many small write and read calls will be made.
#[derive(Clone)]
#[non_exhaustive]
pub struct Encoder<'a, T> {
    /// The underlying stream
    pub stream: T,
    /// The state
    pub ctxt: Context<'a>,
    /// The stack
    #[cfg(feature = "debug")]
    pub stack: source::Stack,
}

macro_rules! debug_fn {
    ($fn_name:ident, $variant_name:ident ( $ty:ty )) => {
        #[inline]
        pub fn $fn_name<F, R>(&mut self, f: F, s: $ty) -> EncodingResult<R>
        where
            F: FnOnce(&mut Encoder<T>) -> EncodingResult<R>,
        {
            #[cfg(feature = "debug")]
            {
                #[cfg(feature = "alloc")]
                {
                    self.stack.frames.push(source::Frame::$variant_name(s));
                    let r = f(self)?;
                    self.stack.frames.pop();
                    Ok(r)
                }
                #[cfg(not(feature = "alloc"))]
                {
                    let last_frame = self.stack.last_frame;
                    self.stack.last_frame = source::Frame::$variant_name(s);
                    let r = f(self)?;
                    self.stack.last_frame = last_frame;
                    Ok(r)
                }
            }
            #[cfg(not(feature = "debug"))]
            {
                let _ = s;
                f(self)
            }
        }
    };
}

impl<'a, T> Encoder<'a, T> {
    /// Wraps the given stream and state.
    #[inline]
    pub fn new(stream: T, ctxt: Context<'a>) -> Self {
        Self {
            stream,
            ctxt,
            #[cfg(feature = "debug")]
            stack: source::Stack::new(),
        }
    }

    /// Replaces the underlying stream with the new one, returning the previous value
    #[inline]
    pub fn swap_stream(&mut self, new: T) -> T {
        replace(&mut self.stream, new)
    }

    /// Retrieves the user data and attempts to cast it to the given concrete type,
    /// returning a validation error in case the types don't match or no user data is stored.
    #[inline]
    pub fn user_data<U: Any>(&self) -> EncodingResult<&U> {
        self.ctxt
            .user
            .ok_or(val_error!("User-data requested, but none is present"))
            .and_then(|x| {
                x.downcast_ref().ok_or(val_error!(
                    "User-data doesnt match the requested concrete type of {}",
                    core::any::type_name::<U>()
                ))
            })
    }

    debug_fn!(with_item, Item(&'static str));
    debug_fn!(with_variant, Variant(&'static str));
    debug_fn!(with_field, Field(&'static str));
    debug_fn!(with_index, Index(usize));
}

impl<T: Write> Encoder<'_, T> {
    /// Method for convenience.
    ///
    /// Encodes a value using `self` as the encoder.
    ///
    /// This method is not magic - it is literally defined as `value.encode(self)`
    #[inline]
    pub fn encode_value<V: Encode<T>>(&mut self, value: V) -> EncodingResult<()> {
        value.encode(self)
    }
}

impl<T: Read> Encoder<'_, T> {
    /// Method for convenience.
    ///
    /// Decodes a value using `self` as the decoder.
    ///
    /// This method is not magic - it is literally defined as `V::decode(self)`
    #[inline]
    pub fn decode_value<V: Decode<T>>(&mut self) -> EncodingResult<V> {
        V::decode(self)
    }
}

impl<'a, T> Encoder<'a, T> {
    #[inline]
    pub fn finish(self) -> (T, Context<'a>) {
        (self.stream, self.ctxt)
    }
}

macro_rules! make_write_fns {
    (
	    type $uty:ty {
		    pub u_write: $u_write:ident,
		    pub u_write_direct: $u_write_direct:ident,
		    priv uleb128_encode: $uleb128_encode:ident
		    $(,)?
	    },
	    type $ity:ty {
		    pub i_write: $i_write:ident,
		    pub i_write_direct: $i_write_direct:ident,
		    priv leb128_encode: $leb128_encode:ident
		    $(,)?
	    }$(,)?
    ) => {
	    fn $uleb128_encode(&mut self, value: $uty) -> EncodingResult<()> {
		    let mut shifted = value;
	        let mut more = true;
	        while more {
		        let mut byte: u8 = shifted as u8 & 0b01111111;
		        shifted >>= 7;

		        // Is the next shifted value worth writing?
		        if shifted != 0 {
			        byte |= 0b10000000;
		        } else {
			        more = false;
		        }
		        self.write_byte(byte)?;
			}
		    Ok(())
	    }

	    #[doc = "Encodes a `"]
	    #[doc = stringify!($uty)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
        #[inline]
	    pub fn $u_write(&mut self, value: $uty) -> EncodingResult<()> {
		    self.$u_write_direct(value, self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    #[inline]
        pub fn $u_write_direct(&mut self, value: $uty, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<()> {
	        match num_encoding {
		        NumEncoding::Fixed => {
			        let bytes: [u8; core::mem::size_of::<$uty>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write(&bytes)?;
		        },
		        NumEncoding::Leb128 | NumEncoding::ProtobufWasteful | NumEncoding::ProtobufZigzag => {
			        self.$uleb128_encode(value)?;
		        }
	        }
            Ok(())
        }

	    fn $leb128_encode(&mut self, value: $ity) -> EncodingResult<()> {
		        let mut shifted = value;
		        let mut more = true;
		        while more {
			        let mut byte = shifted as u8 & 0b0111_1111;
			        shifted >>= 7;

			        // Is the next shifted value worth writing?
			        let neg = (byte & 0b0100_0000) != 0;
			        if (neg && shifted != -1) || (!neg && shifted != 0) {
				        byte |= 0b1000_0000;
			        } else {
				        more = false;
			        }
			        self.write_byte(byte)?;
				}
		        Ok(())
	        }

	    #[doc = "Encodes a `"]
	    #[doc = stringify!($ity)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
        #[inline]
	    pub fn $i_write(&mut self, value: $ity) -> EncodingResult<()> {
		    self.$i_write_direct(value, self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    #[inline]
        pub fn $i_write_direct(&mut self, value: $ity, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<()> {
		    match num_encoding {
		        NumEncoding::Fixed => {
			        let bytes: [u8; core::mem::size_of::<$ity>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write(&bytes)?;
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

impl<T: Write> Encoder<'_, T> {
    make_write_fns! {
        type u8 {
            pub u_write: write_u8,
            pub u_write_direct: write_u8_with,
            priv uleb128_encode: uleb128_encode_u8,
        },
        type i8 {
            pub i_write: write_i8,
            pub i_write_direct: write_i8_with,
            priv leb128_encode: leb128_encode_i8,
        },
    }
    make_write_fns! {
        type u16 {
            pub u_write: write_u16,
            pub u_write_direct: write_u16_with,
            priv uleb128_encode: uleb128_encode_u16,
        },
        type i16 {
            pub i_write: write_i16,
            pub i_write_direct: write_i16_with,
            priv leb128_encode: leb128_encode_i16,
        },
    }
    make_write_fns! {
        type u32 {
            pub u_write: write_u32,
            pub u_write_direct: write_u32_with,
            priv uleb128_encode: uleb128_encode_u32,
        },
        type i32 {
            pub i_write: write_i32,
            pub i_write_direct: write_i32_with,
            priv leb128_encode: leb128_encode_i32,
        },
    }
    make_write_fns! {
        type u64 {
            pub u_write: write_u64,
            pub u_write_direct: write_u64_with,
            priv uleb128_encode: uleb128_encode_u64,
        },
        type i64 {
            pub i_write: write_i64,
            pub i_write_direct: write_i64_with,
            priv leb128_encode: leb128_encode_i64,
        },
    }
    make_write_fns! {
        type u128 {
            pub u_write: write_u128,
            pub u_write_direct: write_u128_with,
            priv uleb128_encode: uleb128_encode_u128,
        },
        type i128 {
            pub i_write: write_i128,
            pub i_write_direct: write_i128_with,
            priv leb128_encode: leb128_encode_i128,
        },
    }

    /// Encodes an `usize`.
    ///
    /// If the size flatten variable is set to `Some`,
    /// this function checks that the value matches but then returns
    /// immediately without writing, otherwise it will encode the given `usize`
    /// to the underlying stream according to the endianness, numerical
    /// encoding and bit-width in the encoder's state, with an additional
    /// check that the value does not exceed the max size.
    #[inline]
    pub fn write_usize(&mut self, value: usize) -> EncodingResult<()> {
        if let Some(size) = self.ctxt.consume_size_flatten() {
            if size != value {
                return Err(EncodingError::FlattenError(FlattenError::LenMismatch {
                    expected: size,
                    got: value,
                }));
            }
            Ok(())
        } else {
            if value > self.ctxt.settings.size_repr.max_size {
                return Err(EncodingError::MaxSizeExceeded {
                    max: self.ctxt.settings.size_repr.max_size,
                    requested: value,
                });
            }
            let encoding = self.ctxt.settings.size_repr.num_encoding;
            let endianness = self.ctxt.settings.size_repr.endianness;

            // `Opaque` already converts conversion errors to `EncodingError`
            let opaque = Opaque::from(value);

            match self.ctxt.settings.size_repr.width {
                BitWidth::Bit8 => self.write_u8_with(opaque.try_into()?, encoding, endianness),
                BitWidth::Bit16 => self.write_u16_with(opaque.try_into()?, encoding, endianness),
                BitWidth::Bit32 => self.write_u32_with(opaque.try_into()?, encoding, endianness),
                BitWidth::Bit64 => self.write_u64_with(opaque.try_into()?, encoding, endianness),
                BitWidth::Bit128 => self.write_u128_with(opaque.try_into()?, encoding, endianness),
            }
        }
    }

    /// Encodes a `isize` to the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state.
    #[inline]
    pub fn write_isize(&mut self, value: isize) -> EncodingResult<()> {
        let encoding = self.ctxt.settings.size_repr.num_encoding;
        let endianness = self.ctxt.settings.size_repr.endianness;

        // `Opaque` already converts conversion errors to `EncodingError`
        let opaque = Opaque::from(value);

        match self.ctxt.settings.size_repr.width {
            BitWidth::Bit8 => self.write_i8_with(opaque.try_into()?, encoding, endianness),
            BitWidth::Bit16 => self.write_i16_with(opaque.try_into()?, encoding, endianness),
            BitWidth::Bit32 => self.write_i32_with(opaque.try_into()?, encoding, endianness),
            BitWidth::Bit64 => self.write_i64_with(opaque.try_into()?, encoding, endianness),
            BitWidth::Bit128 => self.write_i128_with(opaque.try_into()?, encoding, endianness),
        }
    }

    /// Encodes an unsigned `Variant`.
    ///
    /// If the `Variant` flatten variable is set to `Some`,
    /// this function checks that the value matches but then returns
    /// immediately without writing, otherwise it will encode the given `Variant`
    /// to the underlying stream according to the endianness, numerical
    /// encoding and bit-width in the encoder's state.
    #[inline]
    #[allow(private_bounds)]
    pub fn write_uvariant<V>(&mut self, value: V) -> EncodingResult<()>
    where
        Opaque: From<V>,
        V: Sign<Sign = Unsigned>,
    {
        let value = Opaque::from(value);
        if let Some(variant) = self.ctxt.consume_variant_flatten() {
            if value != variant {
                return Err(FlattenError::VariantMismatch {
                    expected: variant,
                    got: value,
                }
                .into());
            }
            Ok(())
        } else {
            let width = self.ctxt.settings.variant_repr.width;
            let encoding = self.ctxt.settings.variant_repr.num_encoding;
            let endianness = self.ctxt.settings.variant_repr.endianness;
            match width {
                BitWidth::Bit8 => self.write_u8_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit16 => self.write_u16_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit32 => self.write_u32_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit64 => self.write_u64_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit128 => self.write_u128_with(value.try_into()?, encoding, endianness),
            }
        }
    }

    /// Encodes a signed `Variant`.
    ///
    /// If the `Variant` flatten variable is set to `Some`,
    /// this function checks that the value matches but then returns
    /// immediately without writing, otherwise it will encode the given `Variant`
    /// to the underlying stream according to the endianness, numerical
    /// encoding and bit-width in the encoder's state.
    #[inline]
    #[allow(private_bounds)]
    pub fn write_ivariant<V>(&mut self, value: V) -> EncodingResult<()>
    where
        Opaque: From<V>,
        V: Sign<Sign = Signed>,
    {
        let value = Opaque::from(value);
        if let Some(variant) = self.ctxt.consume_variant_flatten() {
            if value != variant {
                return Err(FlattenError::VariantMismatch {
                    expected: variant,
                    got: value,
                }
                .into());
            }
            Ok(())
        } else {
            let width = self.ctxt.settings.variant_repr.width;
            let encoding = self.ctxt.settings.variant_repr.num_encoding;
            let endianness = self.ctxt.settings.variant_repr.endianness;
            match width {
                BitWidth::Bit8 => self.write_i8_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit16 => self.write_i16_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit32 => self.write_i32_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit64 => self.write_i64_with(value.try_into()?, encoding, endianness),
                BitWidth::Bit128 => self.write_i128_with(value.try_into()?, encoding, endianness),
            }
        }
    }

    /// Encodes a boolean value.
    ///
    /// It is guaranteed that, if `value` is `true`, a single u8 will be written to the
    /// underlying stream with the value `1`, and if `value` is `false`, with a value of `0`.
    ///
    /// If the `bool` flatten variable is set to `Some`,
    /// this function checks that the value matches but then returns
    /// immediately without writing, otherwise it will encode the given `bool`
    /// as described above.
    #[inline]
    pub fn write_bool(&mut self, value: bool) -> EncodingResult<()> {
        if let Some(boolean) = self.ctxt.consume_bool_flatten() {
            if boolean != value {
                return Err(FlattenError::BoolMismatch {
                    expected: boolean,
                    got: value,
                }
                .into());
            }
            Ok(())
        } else {
            self.write_byte(if value { 1 } else { 0 })
        }
    }

    /// Encodes a `char` to the underlying stream, according to the endianness and string encoding
    /// in the encoder's state.
    #[inline]
    pub fn write_char(&mut self, value: char) -> EncodingResult<()> {
        if value == '\0' {
            self.write_char_or_null(None)
        } else {
            self.write_char_or_null(Some(value))
        }
    }

    /// You MUST guarantee that char is never '\0'
    #[inline]
    fn write_char_or_null(&mut self, value: Option<char>) -> EncodingResult<()> {
        if let Some(value) = value {
            let endianness = self.ctxt.settings.string_repr.endianness;
            match self.ctxt.settings.string_repr.encoding {
                StrEncoding::Ascii => {
                    if !value.is_ascii() {
                        return Err(StringError::InvalidChar.into());
                    }

                    self.write_byte(value as u8)?;
                }
                StrEncoding::Utf8 => {
                    let mut buf = [0u8; 4];
                    let len = value.encode_utf8(&mut buf).len();

                    self.write_bytes(&buf[..len])?;
                }
                StrEncoding::Utf16 => {
                    let mut buf = [0u16; 2];
                    let len = value.encode_utf16(&mut buf).len();

                    for block in buf[..len].iter() {
                        self.write_u16_with(*block, NumEncoding::Fixed, endianness)?;
                    }
                }
                StrEncoding::Utf32 => {
                    self.write_u32_with(value as u32, NumEncoding::Fixed, endianness)?;
                }
                StrEncoding::Windows1252 => {
                    let encoded = windows1252::dec_to_enc(value);
                    if let Some(encoded) = encoded {
                        self.write_byte(encoded)?;
                    } else {
                        return Err(StringError::InvalidChar.into());
                    }
                }
            }
        } else {
            for _ in 0..self.ctxt.settings.string_repr.encoding.bytes() {
                self.write_byte(0)?;
            }
        }
        Ok(())
    }

    /// Encodes a `f32` to the underlying stream, ignoring the numeric encoding but respecting
    /// the endianness. Equivalent of `Self::write_u32(value.to_bits())` with the numeric
    /// encoding set to Fixed
    #[inline]
    pub fn write_f32(&mut self, value: f32) -> EncodingResult<()> {
        self.write_u32_with(
            value.to_bits(),
            NumEncoding::Fixed,
            self.ctxt.settings.num_repr.endianness,
        )
    }

    /// Encodes a `f64` to the underlying stream, ignoring the numeric encoding but respecting
    /// the endianness. Equivalent of `Self::write_u64(value.to_bits())` with the numeric
    /// encoding set to Fixed
    #[inline]
    pub fn write_f64(&mut self, value: f64) -> EncodingResult<()> {
        self.write_u64_with(
            value.to_bits(),
            NumEncoding::Fixed,
            self.ctxt.settings.num_repr.endianness,
        )
    }

    /// Encodes a string to the underlying stream, according to the endianness,
    /// string encoding and string-length encoding in the encoder's state.
    /// Anything whose chars can be iterated over is considered a string.
    ///
    /// # Example
    ///
    /// ```
    /// use ender::{Context, Encoder};
    /// use ender::io::Zero;
    ///
    /// let mut encoder = Encoder::new(Zero, Context::new());
    /// encoder.write_str("Hello, world!".chars()).unwrap();
    /// ```
    #[inline]
    pub fn write_str<S>(&mut self, string: S) -> EncodingResult<()>
    where
        S: IntoIterator<Item = char, IntoIter: Clone>,
    {
        let chars = string.into_iter();

        match self.ctxt.settings.string_repr.len {
            StrLen::LengthPrefixed => {
                // We don't know the length of the string in advance

                // Create a fake encoder that simply keeps track of the length
                let mut sz_encoder = Encoder::new(SizeTrack::new(Zero), self.ctxt);
                for ch in chars.clone() {
                    sz_encoder.write_char(ch)?;
                }
                let size = sz_encoder.finish().0.size_written();

                // Now encode the length and the string data
                self.write_usize(size)?;
                for ch in chars {
                    self.write_char(ch)?;
                }
            }
            StrLen::NullTerminated => {
                for ch in chars {
                    self.write_char(ch)?;
                }
                self.write_char_or_null(None)?;
            }
            StrLen::NullTerminatedFixed(max) => {
                let mut capped = Encoder::new(SizeLimit::new(&mut self.stream, max, 0), self.ctxt);
                for ch in chars {
                    match capped.write_char(ch) {
                        Err(EncodingError::UnexpectedEnd) => {
                            Err(EncodingError::StringError(StringError::TooLong))
                        }
                        any => any,
                    }?;
                }

                // Fill the rest with zeroes
                for _ in 0..capped.stream.remaining_writable() {
                    self.write_byte(0)?;
                }
            }
        }

        Ok(())
    }

    /// Writes a single byte to the underlying stream as-is.
    #[inline]
    pub fn write_byte(&mut self, byte: u8) -> EncodingResult<()> {
        self.stream.write(&[byte])
    }

    /// Writes the given slice to the underlying stream as-is.
    #[inline]
    pub fn write_bytes(&mut self, bytes: &[u8]) -> EncodingResult<()> {
        self.stream.write(bytes)
    }
}

macro_rules! make_read_fns {
    (
	    type $uty:ty {
		    pub u_read: $u_read:ident,
		    pub u_read_direct: $u_read_direct:ident,
		    priv uleb128_decode: $uleb128_decode:ident
		    $(,)?
	    },
	    type $ity:ty {
		    pub i_read: $i_read:ident,
		    pub i_read_direct: $i_read_direct:ident,
		    priv leb128_decode: $leb128_decode:ident
		    $(,)?
	    }
	    $(,)?
    ) => {
	    fn $uleb128_decode(&mut self) -> EncodingResult<$uty> {
			    let mut result: $uty = 0;
		        let mut shift: u8 = 0;
		        loop {
			        if shift >= <$uty>::BITS as u8 {
				        return Err(EncodingError::VarIntError);
			        }

		            let byte = self.read_byte()?;
			        result |= (byte & 0b0111_1111) as $uty << shift;
			        shift += 7;

			        if (byte & 0b1000_0000) == 0 {
				        break;
			        }
				}
		        Ok(result)
		    }

	    #[doc = "Decodes a `"]
	    #[doc = stringify!($uty)]
	    #[doc = "` from the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
        #[inline]
	    pub fn $u_read(&mut self) -> EncodingResult<$uty> {
		    self.$u_read_direct(self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    #[inline]
        pub fn $u_read_direct(&mut self, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<$uty> {
		    Ok(match num_encoding {
		        NumEncoding::Fixed => {
			        let mut bytes: [u8; core::mem::size_of::<$uty>()] = [0u8; core::mem::size_of::<$uty>()];
		            self.stream.read(&mut bytes)?;

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

	     fn $leb128_decode(&mut self) -> EncodingResult<$ity> {
			    let mut result: $ity = 0;
		        let mut byte;
		        let mut shift: u8 = 0;
		        loop {
			        if shift >= <$ity>::BITS as u8 {
				        return Err(EncodingError::VarIntError);
			        }

		            byte = self.read_byte()?;
			        result |= (byte & 0b0111_1111) as $ity << shift;
			        shift += 7;

			        if (byte & 0b1000_0000) == 0 {
				        break;
			        }
				}

		        if shift < <$ity>::BITS as u8 && (byte & 0b0100_0000) != 0 {
			        result |= (!0 << shift);
		        }

		        Ok(result)
		    }

	    #[doc = "Decodes a `"]
	    #[doc = stringify!($ity)]
	    #[doc = "` from the underlying stream, according to the endianness and numerical encoding in the encoder's context"]
        #[inline]
	    pub fn $i_read(&mut self) -> EncodingResult<$ity> {
		    self.$i_read_direct(self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    #[inline]
        pub fn $i_read_direct(&mut self, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<$ity> {
	        Ok(match num_encoding {
		        NumEncoding::Fixed => {
			        let mut bytes: [u8; core::mem::size_of::<$ity>()] = [0u8; core::mem::size_of::<$ity>()];
		            self.stream.read(&mut bytes)?;

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

impl<T: Read> Encoder<'_, T> {
    make_read_fns! {
        type u8 {
            pub u_read: read_u8,
            pub u_read_direct: read_u8_with,
            priv uleb128_decode: uleb128_decode_u8,
        },
        type i8 {
            pub i_read: read_i8,
            pub i_read_direct: read_i8_with,
            priv leb128_decode: leb128_decode_i8,
        },
    }
    make_read_fns! {
        type u16 {
            pub u_read: read_u16,
            pub u_read_direct: read_u16_with,
            priv uleb128_decode: uleb128_decode_u16,
        },
        type i16 {
            pub i_read: read_i16,
            pub i_read_direct: read_i16_with,
            priv leb128_decode: leb128_decode_i16,
        },
    }
    make_read_fns! {
        type u32 {
            pub u_read: read_u32,
            pub u_read_direct: read_u32_with,
            priv uleb128_decode: uleb128_decode_u32,
        },
        type i32 {
            pub i_read: read_i32,
            pub i_read_direct: read_i32_with,
            priv leb128_decode: leb128_decode_i32,
        },
    }
    make_read_fns! {
        type u64 {
            pub u_read: read_u64,
            pub u_read_direct: read_u64_with,
            priv uleb128_decode: uleb128_decode_u64,
        },
        type i64 {
            pub i_read: read_i64,
            pub i_read_direct: read_i64_with,
            priv leb128_decode: leb128_decode_i64,
        },
    }
    make_read_fns! {
        type u128 {
            pub u_read: read_u128,
            pub u_read_direct: read_u128_with,
            priv uleb128_decode: uleb128_decode_u128,
        },
        type i128 {
            pub i_read: read_i128,
            pub i_read_direct: read_i128_with,
            priv leb128_decode: leb128_decode_i128,
        },
    }

    /// Decodes an `usize`.
    ///
    /// If the `usize` flatten variable is set to `Some`, this function
    /// will return its value without reading, otherwise it will decode an `usize`
    /// from the underlying stream, according to the endianness, numerical encoding
    /// and bit-width in the encoder's state, with an additional check that the size
    /// does not exceed the max size.
    #[inline]
    pub fn read_usize(&mut self) -> EncodingResult<usize> {
        if let Some(size) = self.ctxt.consume_size_flatten() {
            Ok(size)
        } else {
            let encoding = self.ctxt.settings.size_repr.num_encoding;
            let endianness = self.ctxt.settings.size_repr.endianness;
            let value = match self.ctxt.settings.size_repr.width {
                BitWidth::Bit8 => Opaque::from(self.read_u8_with(encoding, endianness)?),
                BitWidth::Bit16 => Opaque::from(self.read_u16_with(encoding, endianness)?),
                BitWidth::Bit32 => Opaque::from(self.read_u32_with(encoding, endianness)?),
                BitWidth::Bit64 => Opaque::from(self.read_u64_with(encoding, endianness)?),
                BitWidth::Bit128 => Opaque::from(self.read_u128_with(encoding, endianness)?),
            }
            .try_into()?;

            if value > self.ctxt.settings.size_repr.max_size {
                return Err(EncodingError::MaxSizeExceeded {
                    max: self.ctxt.settings.size_repr.max_size,
                    requested: value,
                });
            }
            Ok(value)
        }
    }

    /// Decodes a `isize` from the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    #[inline]
    pub fn read_isize(&mut self) -> EncodingResult<isize> {
        let encoding = self.ctxt.settings.size_repr.num_encoding;
        let endianness = self.ctxt.settings.size_repr.endianness;
        match self.ctxt.settings.size_repr.width {
            BitWidth::Bit8 => Opaque::from(self.read_i8_with(encoding, endianness)?),
            BitWidth::Bit16 => Opaque::from(self.read_i16_with(encoding, endianness)?),
            BitWidth::Bit32 => Opaque::from(self.read_i32_with(encoding, endianness)?),
            BitWidth::Bit64 => Opaque::from(self.read_i64_with(encoding, endianness)?),
            BitWidth::Bit128 => Opaque::from(self.read_i128_with(encoding, endianness)?),
        }
        .try_into()
    }

    /// Decodes an unsigned `Variant`.
    ///
    /// If the `Variant` flatten variable is set to `Some`, this function
    /// will return its value without reading, otherwise it will decode a `Variant`
    /// from the underlying stream, according to the endianness, numerical encoding
    /// and bit-width in the encoder's state.
    #[inline]
    #[allow(private_bounds)]
    pub fn read_uvariant<V>(&mut self) -> EncodingResult<V>
    where
        V: Sign<Sign = Unsigned>,
        Opaque: TryInto<V, Error = EncodingError>,
    {
        if let Some(variant) = self.ctxt.consume_variant_flatten() {
            variant.try_into()
        } else {
            let width = self.ctxt.settings.variant_repr.width;
            let encoding = self.ctxt.settings.variant_repr.num_encoding;
            let endianness = self.ctxt.settings.variant_repr.endianness;

            match width {
                BitWidth::Bit8 => Opaque::from(self.read_u8_with(encoding, endianness)?),
                BitWidth::Bit16 => Opaque::from(self.read_u16_with(encoding, endianness)?),
                BitWidth::Bit32 => Opaque::from(self.read_u32_with(encoding, endianness)?),
                BitWidth::Bit64 => Opaque::from(self.read_u64_with(encoding, endianness)?),
                BitWidth::Bit128 => Opaque::from(self.read_u128_with(encoding, endianness)?),
            }
            .try_into()
        }
    }

    /// Decodes a signed `Variant`.
    ///
    /// If the `Variant` flatten variable is set to `Some`, this function
    /// will return its value without reading, otherwise it will decode a `Variant`
    /// from the underlying stream, according to the endianness, numerical encoding
    /// and bit-width in the encoder's state.
    #[inline]
    #[allow(private_bounds)]
    pub fn read_ivariant<V>(&mut self) -> EncodingResult<V>
    where
        V: Sign<Sign = Signed>,
        Opaque: TryInto<V, Error = EncodingError>,
    {
        if let Some(variant) = self.ctxt.consume_variant_flatten() {
            variant.try_into()
        } else {
            let width = self.ctxt.settings.variant_repr.width;
            let encoding = self.ctxt.settings.variant_repr.num_encoding;
            let endianness = self.ctxt.settings.variant_repr.endianness;

            match width {
                BitWidth::Bit8 => Opaque::from(self.read_u8_with(encoding, endianness)?),
                BitWidth::Bit16 => Opaque::from(self.read_i16_with(encoding, endianness)?),
                BitWidth::Bit32 => Opaque::from(self.read_i32_with(encoding, endianness)?),
                BitWidth::Bit64 => Opaque::from(self.read_i64_with(encoding, endianness)?),
                BitWidth::Bit128 => Opaque::from(self.read_i128_with(encoding, endianness)?),
            }
            .try_into()
        }
    }

    /// Decodes a boolean value.
    ///
    /// It is guaranteed that, one `u8` is read from the underlying stream and, if
    /// it's equal to `1`, `true` is returned, if it's equal to `0`, `false` is returned,
    /// for any other value an [`InvalidBool`][`EncodingError::InvalidBool`]
    /// error will be returned.
    ///
    /// If the `bool` flatten variable is set to `Some`,
    /// then its value is returned without reading,
    /// otherwise the boolean is decoded as described above.
    #[inline]
    pub fn read_bool(&mut self) -> EncodingResult<bool> {
        if let Some(boolean) = self.ctxt.consume_bool_flatten() {
            Ok(boolean)
        } else {
            match self.read_byte()? {
                0 => Ok(false),
                1 => Ok(true),
                _ => Err(EncodingError::InvalidBool),
            }
        }
    }

    /// Decodes a `char` from the underlying stream, according to the endianness and string encoding
    /// in the encoder's state.
    #[inline]
    pub fn read_char(&mut self) -> EncodingResult<char> {
        Ok(self.read_char_or_null()?.unwrap_or('\0'))
    }

    #[inline]
    fn read_char_or_null(&mut self) -> EncodingResult<Option<char>> {
        let endianness = self.ctxt.settings.string_repr.endianness;
        match self.ctxt.settings.string_repr.encoding {
            StrEncoding::Ascii => {
                let ch = self.read_byte()?;
                if ch == 0 {
                    return Ok(None);
                }
                if !ch.is_ascii() {
                    return Err(StringError::InvalidChar.into());
                }
                let ch = char::from_u32(ch as u32);

                // PANIC SAFETY
                //
                // We check that the character is ascii before converting it to a char
                // and if it is, we return *BEFORE* converting it, so `ch` is ALWAYS
                // Some at this point
                Ok(Some(ch.unwrap()))
            }
            StrEncoding::Utf8 => {
                // See https://en.wikipedia.org/wiki/UTF-8#Encoding
                let mut buf = self.read_byte()?;
                if buf == 0 {
                    return Ok(None);
                }
                let (add, rshift) = if buf.leading_ones() == 0 {
                    (0usize, 1u32)
                } else {
                    let leading = buf.leading_ones();
                    if leading == 1 || leading > 4 {
                        // The first byte was either a continuation byte
                        // or forward declared more than 3 continuation bytes
                        return Err(StringError::InvalidChar.into());
                    }
                    (leading as usize - 1, leading + 1)
                };

                let mut ch: u32 = ((u8::MAX >> rshift) & buf) as u32;

                let mut shift = 0;
                for _ in 0..add {
                    buf = self.read_byte()?;

                    if buf.leading_ones() != 1 {
                        // This byte was not a continuation byte, but we expected it to be
                        return Err(StringError::InvalidChar.into());
                    }

                    shift += 6;
                    ch = (ch << shift) | ((buf & 0b0011_1111) as u32);
                }

                Ok(Some(char::from_u32(ch).ok_or(
                    EncodingError::StringError(StringError::InvalidChar),
                )?))
            }
            StrEncoding::Utf16 => {
                // See https://en.wikipedia.org/wiki/UTF-16#Code_points_from_U+010000_to_U+10FFFF
                let buf = self.read_u16_with(NumEncoding::Fixed, endianness)?;
                if buf == 0 {
                    return Ok(None);
                }
                let ch;

                // This is a high surrogate
                if 0xD800 <= buf && buf <= 0xDBFF {
                    let high_surrogate = buf;
                    let low_surrogate = self.read_u16_with(NumEncoding::Fixed, endianness)?;

                    if !(0xDC00 <= low_surrogate && low_surrogate <= 0xDFFF) {
                        // First character was in the high surrogate range,
                        // but the second character wasn't in the low surrogate range
                        return Err(StringError::InvalidChar.into());
                    }

                    const LOW_TEN_BITS: u16 = 0b0000_0011_1111_1111;

                    let high_bits = ((high_surrogate - 0xD800) & LOW_TEN_BITS) as u32;
                    let low_bits = ((low_surrogate - 0xDC00) & LOW_TEN_BITS) as u32;

                    ch = (high_bits << 10) | low_bits;
                } else if 0xDC00 <= buf && buf <= 0xDFFF {
                    // First character was in the low surrogate range
                    return Err(StringError::InvalidChar.into());
                } else {
                    ch = buf as u32;
                }

                Ok(Some(char::from_u32(ch).ok_or(
                    EncodingError::StringError(StringError::InvalidChar),
                )?))
            }
            StrEncoding::Utf32 => {
                let buf = self.read_u32_with(NumEncoding::Fixed, endianness)?;
                if buf == 0 {
                    return Ok(None);
                }

                Ok(Some(char::from_u32(buf).ok_or(
                    EncodingError::StringError(StringError::InvalidChar),
                )?))
            }
            StrEncoding::Windows1252 => {
                let buf = self.read_byte()?;
                if buf == 0 {
                    return Ok(None);
                }

                if let Some(ch) = windows1252::enc_to_dec(buf) {
                    Ok(Some(ch))
                } else {
                    Err(EncodingError::StringError(StringError::InvalidChar))
                }
            }
        }
    }

    /// Decodes a `f32` from the underlying stream, ignoring the numeric encoding but respecting
    /// the endianness. Equivalent of `f32::from_bits(self.read_u32())` with the numeric
    /// encoding set to [`NumEncoding::Fixed`].
    #[inline]
    pub fn read_f32(&mut self) -> EncodingResult<f32> {
        Ok(f32::from_bits(self.read_u32_with(
            NumEncoding::Fixed,
            self.ctxt.settings.num_repr.endianness,
        )?))
    }

    /// Decodes a `f64` from the underlying stream, ignoring the numeric encoding but respecting
    /// the endianness. Equivalent of `f64::from_bits(self.read_u64())` with the numeric
    /// encoding set to [`NumEncoding::Fixed`].
    #[inline]
    pub fn read_f64(&mut self) -> EncodingResult<f64> {
        Ok(f64::from_bits(self.read_u64_with(
            NumEncoding::Fixed,
            self.ctxt.settings.num_repr.endianness,
        )?))
    }

    /// Decodes a String from the underlying stream, according to the endianness,
    /// and string encoding in the encoder's state.
    #[inline]
    pub fn read_str<S>(&mut self) -> EncodingResult<S>
    where
        S: FromIterator<char>,
    {
        struct LenPrefixCharIter<'iter, 'user, T: Read> {
            encoder: Encoder<'user, SizeLimit<&'iter mut T>>,
        }

        impl<'iter, 'user, T: Read> Iterator for LenPrefixCharIter<'iter, 'user, T> {
            type Item = EncodingResult<char>;
            fn next(&mut self) -> Option<Self::Item> {
                if self.encoder.stream.remaining_readable() == 0 {
                    // We expect 0 more bytes
                    // This means we can end the iterator by returning None
                    return None;
                };
                
                Some(self.encoder.read_char())
            }
        }

        struct NullTermCharIter<'iter, 'user, T: Read> {
            encoder: &'iter mut Encoder<'user, T>,
        }

        impl<T: Read> Iterator for NullTermCharIter<'_, '_, T> {
            type Item = EncodingResult<char>;
            fn next(&mut self) -> Option<Self::Item> {
                match self.encoder.read_char_or_null() {
                    Ok(None) => {
                        // Null character found
                        None // => STOP!
                    }
                    Ok(Some(x)) => {
                        // Just a char
                        Some(Ok(x)) // ==> Continue
                    }
                    Err(x) => {
                        // An unrelated error occurred
                        Some(Err(x)) // ==> "Continue" but actually early return an error
                    }
                }
            }
        }

        struct NullTermWithMaxCharIter<'iter, 'user, T: Read> {
            encoder: Encoder<'user, SizeLimit<&'iter mut T>>,
        }

        impl<T: Read> Iterator for NullTermWithMaxCharIter<'_, '_, T> {
            type Item = EncodingResult<char>;
            fn next(&mut self) -> Option<Self::Item> {
                if self.encoder.stream.remaining_readable() == 0 {
                    // We reached the maximum amount of bytes we can read
                    // Simply end the iterator
                    return None;
                }
                match self.encoder.read_char_or_null() {
                    Ok(None) => {
                        // Null character found
                        // Read all the remaining nulls
                        for _ in 0..self.encoder.stream.remaining_readable() {
                            let z = match self.encoder.read_byte() {
                                Ok(z) => z,
                                Err(err) => return Some(Err(err)),
                            };
                            if z != 0 {
                                return Some(Err(EncodingError::StringError(
                                    StringError::MissingNull,
                                )));
                            }
                        }

                        None // => STOP!
                    }
                    Ok(Some(x)) => {
                        // Just a char
                        Some(Ok(x)) // ==> Continue
                    }
                    Err(x) => {
                        // An unrelated error occurred
                        Some(Err(x)) // ==> "Continue" but actually early return an error
                    }
                }
            }
        }

        match self.ctxt.settings.string_repr.len {
            StrLen::LengthPrefixed => {
                let length = self.read_usize()?;
                let iter = LenPrefixCharIter {
                    encoder: Encoder::new(SizeLimit::new(&mut self.stream, 0, length), self.ctxt),
                };

                iter.collect()
            }
            StrLen::NullTerminated => {
                let iter = NullTermCharIter { encoder: self };
                iter.collect()
            }
            StrLen::NullTerminatedFixed(max) => {
                let iter = NullTermWithMaxCharIter {
                    encoder: Encoder::new(SizeLimit::new(&mut self.stream, 0, max), self.ctxt),
                };
                iter.collect()
            }
        }
    }

    /// Reads a single byte from the stream.
    #[inline]
    pub fn read_byte(&mut self) -> EncodingResult<u8> {
        let mut buf = [0u8; 1];
        self.stream.read(&mut buf)?;
        Ok(buf[0])
    }

    /// Reads `buf.len()` bytes from the stream to the buffer as-is.
    #[inline]
    pub fn read_bytes(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        self.stream.read(buf)
    }
}

macro_rules! make_borrow_slice_fn {
    ($name:ident -> $ty:ty) => {
        #[doc = "Borrows a `"]
        #[doc = stringify!($ty)]
        #[doc = "` slice of `length` length from the encoder, checking"]
        #[doc = "that the [`Endianness`] and alignment match those of the system"]
        #[doc = "and that the [`NumEncoding`] is [`borrowable`][`NumEncoding::borrowable`]"]
        #[inline]
        pub fn $name(
            &mut self,
            length: usize,
            num_encoding: NumEncoding,
            endianness: Endianness,
        ) -> EncodingResult<&'data [$ty]> {
            // Assert the num encoding is borrowable
            if !num_encoding.borrowable() {
                return Err(EncodingError::BorrowError(
                    BorrowError::NonBorrowableNumEncoding { num_encoding },
                ));
            }

            // Assert the endianness matches, else we would be borrowing garbage-looking data.
            if endianness != Endianness::native() {
                return Err(EncodingError::BorrowError(
                    BorrowError::EndiannessMismatch {
                        found: endianness,
                        system: Endianness::native(),
                    },
                ));
            }

            const BYTES: usize = core::mem::size_of::<$ty>();

            let u8_slice: &[u8] = self.stream.borrow_read(length * BYTES)?;

            // Depending on the alignment of the target system, this might fail.
            let conv: &[$ty] = bytemuck::try_cast_slice(u8_slice)
                .map_err(|_| EncodingError::BorrowError(BorrowError::AlignmentMismatch))?;
            Ok(conv)
        }
    };
}

impl<'data, T: BorrowRead<'data>> Encoder<'_, T> {
    /// Returns a reference to `len` bytes in the stream, without advancing it.
    ///
    /// This forwards the call to [`BorrowRead::peek`], meaning multiple calls
    /// produce the same output.
    ///
    /// # Example
    ///
    /// ```
    /// use ender::{Context, Encoder};
    /// use ender::io::Slice;
    ///
    /// let slice = [0, 7, 15, 42, 2];
    /// let encoder = Encoder::new(Slice::new(&slice), Context::new());
    ///
    /// let first_peek = encoder.peek_bytes(3).unwrap();
    /// let second_peek = encoder.peek_bytes(3).unwrap();
    ///
    /// assert_eq!(first_peek, second_peek);
    /// ```
    #[inline]
    pub fn peek_bytes(&self, len: usize) -> EncodingResult<&'data [u8]> {
        self.stream.peek(len)
    }

    /// Borrows a `u8` slice of length `length` from the encoder,
    /// without performing any additional checks.
    #[inline]
    pub fn borrow_byte_slice(&mut self, len: usize) -> EncodingResult<&'data [u8]> {
        self.stream.borrow_read(len)
    }

    /// Borrows a `u8` slice of length `length` from the encoder,
    /// checking that the [`NumEncoding`] is [`borrowable`][`NumEncoding::borrowable`].
    #[inline]
    pub fn borrow_u8_slice(
        &mut self,
        len: usize,
        num_encoding: NumEncoding,
    ) -> EncodingResult<&'data [u8]> {
        // Assert the num encoding is borrowable
        if !num_encoding.borrowable() {
            return Err(EncodingError::BorrowError(
                BorrowError::NonBorrowableNumEncoding { num_encoding },
            ));
        }

        self.stream.borrow_read(len)
    }

    make_borrow_slice_fn!(borrow_u16_slice -> u16);
    make_borrow_slice_fn!(borrow_u32_slice -> u32);
    make_borrow_slice_fn!(borrow_u64_slice -> u64);
    make_borrow_slice_fn!(borrow_u128_slice -> u128);

    /// Borrows a `u8` slice of length `length` from the encoder,
    /// checking that the [`NumEncoding`] is [`borrowable`][`NumEncoding::borrowable`].
    #[inline]
    pub fn borrow_i8_slice(
        &mut self,
        len: usize,
        num_encoding: NumEncoding,
    ) -> EncodingResult<&'data [i8]> {
        // Assert the num encoding is borrowable
        if !num_encoding.borrowable() {
            return Err(EncodingError::BorrowError(
                BorrowError::NonBorrowableNumEncoding { num_encoding },
            ));
        }

        let u8_slice: &[u8] = self.stream.borrow_read(len)?;
        Ok(bytemuck::try_cast_slice(u8_slice).map_err(|_| BorrowError::AlignmentMismatch)?)
    }

    make_borrow_slice_fn!(borrow_i16_slice -> i16);
    make_borrow_slice_fn!(borrow_i32_slice -> i32);
    make_borrow_slice_fn!(borrow_i64_slice -> i64);
    make_borrow_slice_fn!(borrow_i128_slice -> i128);

    make_borrow_slice_fn!(borrow_f32_slice -> f32);
    make_borrow_slice_fn!(borrow_f64_slice -> f64);

    /// Borrows a `usize` slice of length `length` from the encoder.
    ///
    /// Checks that the [`Endianness`] and [`BitWidth`] match those of the target system,
    /// and that the [`NumEncoding`] is [`borrowable`][`NumEncoding::borrowable`]
    #[inline]
    pub fn borrow_usize_slice(
        &mut self,
        len: usize,
        num_encoding: NumEncoding,
        endianness: Endianness,
        bit_width: BitWidth,
    ) -> EncodingResult<&'data [usize]> {
        // Assert the num encoding is borrowable
        if !num_encoding.borrowable() {
            return Err(EncodingError::BorrowError(
                BorrowError::NonBorrowableNumEncoding { num_encoding },
            ));
        }

        // If the system endianness doesn't match, we would be borrowing
        // garbage-looking data
        if endianness != Endianness::native() {
            return Err(EncodingError::BorrowError(
                BorrowError::EndiannessMismatch {
                    found: endianness,
                    system: Endianness::native(),
                },
            ));
        }

        // Again, if the system bit width doesn't match, we would be borrowing a different
        // number of bytes than what the user expects
        if bit_width != BitWidth::native() {
            return Err(EncodingError::BorrowError(BorrowError::BitWidthMismatch {
                found: bit_width,
                system: BitWidth::native(),
            }));
        }

        let u8_slice = self.stream.borrow_read(bit_width.bytes() * len)?;

        // Depending on the alignment of the target system, this might fail.
        let conv: &[usize] = bytemuck::try_cast_slice(u8_slice)
            .map_err(|_| EncodingError::BorrowError(BorrowError::AlignmentMismatch))?;

        // Check that none of the elements exceed the max size
        for &elem in conv {
            let max = self.ctxt.settings.size_repr.max_size;
            if elem > max {
                return Err(EncodingError::MaxSizeExceeded {
                    requested: elem,
                    max,
                });
            }
        }

        Ok(conv)
    }

    /// Borrows a `isize` slice of length `length` from the encoder.
    ///
    /// Checks that the [`Endianness`] and [`BitWidth`] match those of the target system,
    /// and that the [`NumEncoding`] is [`borrowable`][`NumEncoding::borrowable`]
    #[inline]
    pub fn borrow_isize_slice(
        &mut self,
        len: usize,
        num_encoding: NumEncoding,
        endianness: Endianness,
        bit_width: BitWidth,
    ) -> EncodingResult<&'data [isize]> {
        // Assert the num encoding is borrowable
        if !num_encoding.borrowable() {
            return Err(EncodingError::BorrowError(
                BorrowError::NonBorrowableNumEncoding { num_encoding },
            ));
        }

        // If the system endianness doesn't match, we would be borrowing
        // garbage-looking data
        if endianness != Endianness::native() {
            return Err(EncodingError::BorrowError(
                BorrowError::EndiannessMismatch {
                    found: endianness,
                    system: Endianness::native(),
                },
            ));
        }

        // Again, if the system bit width doesn't match, we would be borrowing a different
        // number of bytes than what the user expects
        if bit_width != BitWidth::native() {
            return Err(EncodingError::BorrowError(BorrowError::BitWidthMismatch {
                found: bit_width,
                system: BitWidth::native(),
            }));
        }

        let u8_slice = self.stream.borrow_read(bit_width.bytes() * len)?;

        // Depending on the alignment of the target system, this might fail.
        let conv: &[isize] = bytemuck::try_cast_slice(u8_slice)
            .map_err(|_| EncodingError::BorrowError(BorrowError::AlignmentMismatch))?;
        Ok(conv)
    }
}

impl<T: Seek> Encoder<'_, T> {
    /// Returns the current stream position as a byte offset from the start.
    #[inline]
    pub fn stream_position(&mut self) -> EncodingResult<usize> {
        self.stream.seek(SeekFrom::POSITION)
    }

    /// Performs a seek operation on the underlying stream using the given `seek`
    /// argument.
    #[inline]
    pub fn seek(&mut self, seek: SeekFrom) -> EncodingResult<usize> {
        self.stream.seek(seek)
    }

    /// Performs a seek operation on the underlying stream using the given `seek`
    /// argument, calls the closure, then seeks back to the original position.
    // Notice the `StreamModifier` signature
    #[inline]
    pub fn with_seek<F, R>(&mut self, f: F, seek: SeekFrom) -> EncodingResult<R>
    where
        F: FnOnce(&mut Encoder<T>) -> EncodingResult<R>,
    {
        // Track the current position
        let prev = self.stream_position()? as isize;

        // Magic fn!
        let ret = f(self);

        // Seek to the desired location, and track the location now
        let cur = self.stream.seek(seek)? as isize;
        // Find the difference
        let diff = prev - cur;

        // Now we can seek even on streams that don't support seeking from the Start or End
        self.stream.seek(SeekFrom::Current(diff))?;
        ret
    }
}

/// A binary data structure specification which can be **encoded** into its binary representation.
///
/// Implementations that need to **seek** should implement for `W: Write + Seek`.
///
/// You should keep your implementation as general as possible and avoid
/// implementing for a `R = ConcreteType` if possible
pub trait Encode<W: Write> {
    /// Encodes `self` into its binary format.
    ///
    /// Calling `encode` multiple times on the same value without
    /// changing the encoder settings or the value itself in-between calls should produce
    /// the same output.
    ///
    /// If the result is Ok,
    /// implementations should guarantee that the state of the encoder
    /// is preserved. If the result is Err,
    /// no guarantees are made about the state of the encoder,
    /// and users should reset it before reuse.
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()>;
}

/// A binary data structure specification which can be **decoded** from its binary representation.
///
/// Implementations that need to **seek** should implement for `R: Read + Seek`,
/// while those that need to **borrow** should implement for `R: BorrowRead<'data>`.
///
/// If you need both, use `R: BorrowRead<'data> + Seek`
///
/// You should keep your implementation as general as possible and avoid
/// implementing for a `R = ConcreteType` if possible
///
/// # Note about lifetimes
///
/// An implementation of this trait where your type uses the same lifetime as the decoder
/// (the `'data` lifetime) will greatly limit the possible usages of the implementation.
///
/// Instead, prefer giving your type a different lifetime and make the `'data` lifetime depend on it.
///
/// ### Correct:
/// ```ignore
/// impl<'data: 'a + 'b + ..., 'a, 'b, ...> Decode<BorrowRead<'data>> for Thing<'a, 'b, ...> { ... }
/// ```
///
/// ### Misleading:
/// ```ignore
/// impl<'data> Decode<BorrowRead<'data>> for Thing<'data, 'data, ...> { ... }
/// ```
pub trait Decode<R: Read>: Sized {
    /// Decodes `Self` from its binary format.
    ///
    /// Calling `decode` multiple times without changing the
    /// encoder settings or the underlying binary data in-between calls should produce
    /// the same output.
    ///
    /// If the result is Ok,
    /// implementations should guarantee that the state of the encoder
    /// is preserved. If the result is Err,
    /// no guarantees are made about the state of the encoder,
    /// and users should reset it before reuse.
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self>;
}