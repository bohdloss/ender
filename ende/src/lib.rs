#![cfg_attr(doc_cfg, feature(doc_cfg))]
#![cfg_attr(feature = "unstable", feature(never_type))]
#![cfg_attr(feature = "unstable", feature(error_in_core))]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;
extern crate core;

use core::any::Any;
use core::fmt::Debug;
use core::hash::Hash;
use core::mem::replace;

use parse_display::Display;

#[doc = include_str!("../derive.md")]
#[cfg(feature = "derive")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "derive")))]
pub use ende_derive::{BorrowDecode, Decode, Encode};
pub use error::*;

use crate::io::{BorrowRead, Read, SizeLimit, SizeTrack, Write, Zero};

#[cfg(test)]
mod test;

mod error;
pub mod facade;
mod impls;
pub mod io;
#[cfg(feature = "serde")]
mod serde;

/// Encodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the given context.
pub fn encode_with<T: Write, V: Encode>(
    writer: T,
    context: Context,
    value: V,
) -> EncodingResult<()> {
    let mut stream = Encoder::new(writer, context);
    value.encode(&mut stream)
}

/// Decodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the given context.
pub fn decode_with<T: Read, V: Decode>(reader: T, context: Context) -> EncodingResult<V> {
    let mut stream = Encoder::new(reader, context);
    V::decode(&mut stream)
}

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
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
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
    pub const fn native() -> BitWidth {
        #[cfg(target_pointer_width = "128")]
        {
            BitWidth::Bit128
        }
        #[cfg(target_pointer_width = "64")]
        {
            BitWidth::Bit64
        }
        #[cfg(target_pointer_width = "32")]
        {
            BitWidth::Bit32
        }
        #[cfg(target_pointer_width = "16")]
        {
            BitWidth::Bit16
        }
        #[cfg(target_pointer_width = "8")]
        {
            BitWidth::Bit8
        }
    }

    /// Returns the number of bits represented by this bit-width.
    ///
    /// # Example
    ///
    /// ```rust
    /// use ende::BitWidth;
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
    /// use ende::BitWidth;
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

/// The encoding method used for strings and chars.
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

impl StrEncoding {
    /// Returns the number of bytes of each **code unit** for this encoding.
    #[inline]
    pub const fn bytes(&self) -> usize {
        match self {
            StrEncoding::Ascii => 1,
            StrEncoding::Utf8 => 1,
            StrEncoding::Utf16 => 2,
            StrEncoding::Utf32 => 4,
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
#[display("str_encoding = {str_encoding}, endianness = {endianness}")]
pub struct StringRepr {
    pub str_encoding: StrEncoding,
    pub endianness: Endianness,
}

impl StringRepr {
    /// Returns the default string representation: utf-8, length-prefixed, little_endian
    #[inline]
    pub const fn new() -> Self {
        Self {
            str_encoding: StrEncoding::Utf8,
            endianness: Endianness::LittleEndian,
        }
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

    /// Just like [`Self::new`] but uses the given settings instead of the default
    pub fn with_user_data(settings: BinSettings, data: &'a dyn Any) -> Self {
        Self {
            user: Some(data),
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
pub struct Encoder<'a, T> {
    /// The underlying stream
    pub stream: T,
    /// The state
    pub ctxt: Context<'a>,
}

impl<'a, T> Encoder<'a, T> {
    /// Wraps the given stream and state.
    pub fn new(stream: T, ctxt: Context<'a>) -> Self {
        Self { stream, ctxt }
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
    pub fn encode_value<V: Encode>(&mut self, value: V) -> EncodingResult<()> {
        value.encode(self)
    }
}

impl<T: Read> Encoder<'_, T> {
    /// Method for convenience.<br>
    /// Decodes a value using `self` as the decoder.<br>
    /// This method is not magic - it is literally defined as `V::decode(self)`
    pub fn decode_value<V: Decode>(&mut self) -> EncodingResult<V> {
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
	    fn $uleb128_encode(&mut self, value: $uty) -> EncodingResult<()> {
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
		        self.stream.write(&byte)?;
			}
		    Ok(())
	    }

	    #[doc = "Encodes a `"]
	    #[doc = stringify!($uty)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $u_write(&mut self, value: $uty) -> EncodingResult<()> {
		    self.$u_write_direct(value, self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    fn $u_write_size(&mut self, value: $uty) -> EncodingResult<()> {
		    self.$u_write_direct(value, self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }

	    fn $u_write_variant(&mut self, value: $uty) -> EncodingResult<()> {
		    self.$u_write_direct(value, self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }

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
			        self.stream.write(&byte)?;
				}
		        Ok(())
	        }

	    #[doc = "Encodes a `"]
	    #[doc = stringify!($ity)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $i_write(&mut self, value: $ity) -> EncodingResult<()> {
		    self.$i_write_direct(value, self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    fn $i_write_size(&mut self, value: $ity) -> EncodingResult<()> {
		    self.$i_write_direct(value, self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }

	    fn $i_write_variant(&mut self, value: $ity) -> EncodingResult<()> {
		    self.$i_write_direct(value, self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }

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
            pub u_write_direct: write_u8_direct,
            priv u_write_size: write_u8_size,
            priv u_write_variant: write_u8_variant,
            priv uleb128_encode: uleb128_encode_u8,
        },
        type i8 {
            pub i_write: write_i8,
            pub i_write_direct: write_i8_direct,
            priv i_write_size: write_i8_size,
            priv i_write_variant: write_i8_variant,
            priv leb128_encode: leb128_encode_i8,
        },
    }
    make_write_fns! {
        type u16 {
            pub u_write: write_u16,
            pub u_write_direct: write_u16_direct,
            priv u_write_size: write_u16_size,
            priv u_write_variant: write_u16_variant,
            priv uleb128_encode: uleb128_encode_u16,
        },
        type i16 {
            pub i_write: write_i16,
            pub i_write_direct: write_i16_direct,
            priv i_write_size: write_i16_size,
            priv i_write_variant: write_i16_variant,
            priv leb128_encode: leb128_encode_i16,
        },
    }
    make_write_fns! {
        type u32 {
            pub u_write: write_u32,
            pub u_write_direct: write_u32_direct,
            priv u_write_size: write_u32_size,
            priv u_write_variant: write_u32_variant,
            priv uleb128_encode: uleb128_encode_u32,
        },
        type i32 {
            pub i_write: write_i32,
            pub i_write_direct: write_i32_direct,
            priv i_write_size: write_i32_size,
            priv i_write_variant: write_i32_variant,
            priv leb128_encode: leb128_encode_i32,
        },
    }
    make_write_fns! {
        type u64 {
            pub u_write: write_u64,
            pub u_write_direct: write_u64_direct,
            priv u_write_size: write_u64_size,
            priv u_write_variant: write_u64_variant,
            priv uleb128_encode: uleb128_encode_u64,
        },
        type i64 {
            pub i_write: write_i64,
            pub i_write_direct: write_i64_direct,
            priv i_write_size: write_i64_size,
            priv i_write_variant: write_i64_variant,
            priv leb128_encode: leb128_encode_i64,
        },
    }
    make_write_fns! {
        type u128 {
            pub u_write: write_u128,
            pub u_write_direct: write_u128_direct,
            priv u_write_size: write_u128_size,
            priv u_write_variant: write_u128_variant,
            priv uleb128_encode: uleb128_encode_u128,
        },
        type i128 {
            pub i_write: write_i128,
            pub i_write_direct: write_i128_direct,
            priv i_write_size: write_i128_size,
            priv i_write_variant: write_i128_variant,
            priv leb128_encode: leb128_encode_i128,
        },
    }

    /// Encodes a length. If the flatten attribute is set to Some, this function checks
    /// if the value matches but then returns immediately without writing,
    /// otherwise it will behave identically to [`Self::write_usize`], with an additional
    /// check that the length does not exceed the max size defined in the encoder's state.
    pub fn write_length(&mut self, value: usize) -> EncodingResult<()> {
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
                    requested: value,
                });
            }
            self.write_usize(value)
        }
    }

    /// Encodes a `usize` to the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn write_usize(&mut self, value: usize) -> EncodingResult<()> {
        match self.ctxt.settings.size_repr.width {
            BitWidth::Bit8 => self.write_u8_size(value as _),
            BitWidth::Bit16 => self.write_u16_size(value as _),
            BitWidth::Bit32 => self.write_u32_size(value as _),
            BitWidth::Bit64 => self.write_u64_size(value as _),
            BitWidth::Bit128 => self.write_u128_size(value as _),
        }
    }

    /// Encodes a `isize` to the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn write_isize(&mut self, value: isize) -> EncodingResult<()> {
        match self.ctxt.settings.size_repr.width {
            BitWidth::Bit8 => self.write_i8_size(value as _),
            BitWidth::Bit16 => self.write_i16_size(value as _),
            BitWidth::Bit32 => self.write_i32_size(value as _),
            BitWidth::Bit64 => self.write_i64_size(value as _),
            BitWidth::Bit128 => self.write_i128_size(value as _),
        }
    }

    /// Encodes an unsigned enum variant to the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn write_uvariant(&mut self, value: u128) -> EncodingResult<()> {
        match self.ctxt.settings.variant_repr.width {
            BitWidth::Bit8 => self.write_u8_variant(value as _),
            BitWidth::Bit16 => self.write_u16_variant(value as _),
            BitWidth::Bit32 => self.write_u32_variant(value as _),
            BitWidth::Bit64 => self.write_u64_variant(value as _),
            BitWidth::Bit128 => self.write_u128_variant(value as _),
        }
    }

    /// Encodes a signed enum variant to the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn write_ivariant(&mut self, value: i128) -> EncodingResult<()> {
        match self.ctxt.settings.variant_repr.width {
            BitWidth::Bit8 => self.write_i8_variant(value as _),
            BitWidth::Bit16 => self.write_i16_variant(value as _),
            BitWidth::Bit32 => self.write_i32_variant(value as _),
            BitWidth::Bit64 => self.write_i64_variant(value as _),
            BitWidth::Bit128 => self.write_i128_variant(value as _),
        }
    }

    /// Encodes the boolean state of a value.
    /// If the flatten attribute is set to Some, this function checks
    /// if the value matches but then returns immediately without writing,
    /// otherwise it will behave identically to [`Self::write_bool`].
    pub fn write_state(&mut self, value: bool) -> EncodingResult<()> {
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
    pub fn write_bool(&mut self, value: bool) -> EncodingResult<()> {
        self.write_u8_direct(value as u8, NumEncoding::Fixed, Endianness::LittleEndian)
    }

    /// Encodes a `char` to the underlying stream, according to the endianness and string encoding
    /// in the encoder's state.
    pub fn write_char(&mut self, value: char) -> EncodingResult<()> {
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

                self.write_bytes(&buf[..len])
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
    pub fn write_f32(&mut self, value: f32) -> EncodingResult<()> {
        self.write_u32_direct(
            value.to_bits(),
            NumEncoding::Fixed,
            self.ctxt.settings.num_repr.endianness,
        )
    }

    /// Encodes a `f64` to the underlying stream, ignoring the numeric encoding but respecting
    /// the endianness. Equivalent of `Self::write_u64(value.to_bits())` with the numeric
    /// encoding set to Fixed
    pub fn write_f64(&mut self, value: f64) -> EncodingResult<()> {
        self.write_u64_direct(
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
    /// use ende::{Context, Encoder};
    /// use ende::io::Zero;
    ///
    /// let mut encoder = Encoder::new(Zero, Context::new());
    /// encoder.write_str("Hello, world!".chars()).unwrap();
    /// ```
    pub fn write_str<S, I>(&mut self, string: S) -> EncodingResult<()>
    where
        S: IntoIterator<Item = char, IntoIter = I>,
        I: Iterator<Item = char> + Clone,
    {
        let chars = string.into_iter();
        // We don't know the length of the string in advance

        // Create a fake encoder that simply keeps track of the length
        let mut sz_encoder = Encoder::new(SizeTrack::new(Zero), self.ctxt);
        for ch in chars.clone() {
            sz_encoder.write_char(ch)?;
        }
        let size = sz_encoder.finish().0.size_written();

        // Now encode the length and the string data
        self.write_length(size)?;
        for ch in chars {
            self.write_char(ch)?;
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
	    fn $uleb128_decode(&mut self) -> EncodingResult<$uty> {
			    let mut result: $uty = 0;
		        let mut byte = [0u8; 1];
		        let mut shift: u8 = 0;
		        loop {
			        if shift >= <$uty>::BITS as u8 {
				        return Err(EncodingError::VarIntError);
			        }

		            self.stream.read(&mut byte)?;
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
	    pub fn $u_read(&mut self) -> EncodingResult<$uty> {
		    self.$u_read_direct(self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    fn $u_read_size(&mut self) -> EncodingResult<$uty> {
		    self.$u_read_direct(self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }

	    fn $u_read_variant(&mut self) -> EncodingResult<$uty> {
		    self.$u_read_direct(self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }

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
		        let mut byte = [0u8; 1];
		        let mut shift: u8 = 0;
		        loop {
			        if shift >= <$ity>::BITS as u8 {
				        return Err(EncodingError::VarIntError);
			        }

		            self.stream.read(&mut byte)?;
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
	    pub fn $i_read(&mut self) -> EncodingResult<$ity> {
		    self.$i_read_direct(self.ctxt.settings.num_repr.num_encoding, self.ctxt.settings.num_repr.endianness)
	    }

	    fn $i_read_size(&mut self) -> EncodingResult<$ity> {
		    self.$i_read_direct(self.ctxt.settings.size_repr.num_encoding, self.ctxt.settings.size_repr.endianness)
	    }

	    fn $i_read_variant(&mut self) -> EncodingResult<$ity> {
		    self.$i_read_direct(self.ctxt.settings.variant_repr.num_encoding, self.ctxt.settings.variant_repr.endianness)
	    }

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
            pub u_read_direct: read_u8_direct,
            priv u_read_size: read_u8_size,
            priv u_read_variant: read_u8_variant,
            priv uleb128_decode: uleb128_decode_u8,
        },
        type i8 {
            pub i_read: read_i8,
            pub i_read_direct: read_i8_direct,
            priv i_read_size: read_i8_size,
            priv i_read_variant: read_i8_variant,
            priv leb128_decode: leb128_decode_i8,
        },
    }
    make_read_fns! {
        type u16 {
            pub u_read: read_u16,
            pub u_read_direct: read_u16_direct,
            priv u_read_size: read_u16_size,
            priv u_read_variant: read_u16_variant,
            priv uleb128_decode: uleb128_decode_u16,
        },
        type i16 {
            pub i_read: read_i16,
            pub i_read_direct: read_i16_direct,
            priv i_read_size: read_i16_size,
            priv i_read_variant: read_i16_variant,
            priv leb128_decode: leb128_decode_i16,
        },
    }
    make_read_fns! {
        type u32 {
            pub u_read: read_u32,
            pub u_read_direct: read_u32_direct,
            priv u_read_size: read_u32_size,
            priv u_read_variant: read_u32_variant,
            priv uleb128_decode: uleb128_decode_u32,
        },
        type i32 {
            pub i_read: read_i32,
            pub i_read_direct: read_i32_direct,
            priv i_read_size: read_i32_size,
            priv i_read_variant: read_i32_variant,
            priv leb128_decode: leb128_decode_i32,
        },
    }
    make_read_fns! {
        type u64 {
            pub u_read: read_u64,
            pub u_read_direct: read_u64_direct,
            priv u_read_size: read_u64_size,
            priv u_read_variant: read_u64_variant,
            priv uleb128_decode: uleb128_decode_u64,
        },
        type i64 {
            pub i_read: read_i64,
            pub i_read_direct: read_i64_direct,
            priv i_read_size: read_i64_size,
            priv i_read_variant: read_i64_variant,
            priv leb128_decode: leb128_decode_i64,
        },
    }
    make_read_fns! {
        type u128 {
            pub u_read: read_u128,
            pub u_read_direct: read_u128_direct,
            priv u_read_size: read_u128_size,
            priv u_read_variant: read_u128_variant,
            priv uleb128_decode: uleb128_decode_u128,
        },
        type i128 {
            pub i_read: read_i128,
            pub i_read_direct: read_i128_direct,
            priv i_read_size: read_i128_size,
            priv i_read_variant: read_i128_variant,
            priv leb128_decode: leb128_decode_i128,
        },
    }

    /// Decodes a length. If the flatten attribute is set to Some, this function
    /// will return its value without reading, otherwise it will behave identically to
    /// [`Self::read_usize`], with an additional check that the length does not exceed the
    /// max size defined in the encoder's state.
    pub fn read_length(&mut self) -> EncodingResult<usize> {
        if let Some(length) = self.ctxt.flatten() {
            Ok(length)
        } else {
            let value = self.read_usize()?;
            if value > self.ctxt.settings.size_repr.max_size {
                return Err(EncodingError::MaxLengthExceeded {
                    max: self.ctxt.settings.size_repr.max_size,
                    requested: value,
                });
            }
            Ok(value)
        }
    }

    /// Decodes a `usize` from the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn read_usize(&mut self) -> EncodingResult<usize> {
        Ok(match self.ctxt.settings.size_repr.width {
            BitWidth::Bit8 => self.read_u8_size()? as usize,
            BitWidth::Bit16 => self.read_u16_size()? as usize,
            BitWidth::Bit32 => self.read_u32_size()? as usize,
            BitWidth::Bit64 => self.read_u64_size()? as usize,
            BitWidth::Bit128 => self.read_u128_size()? as usize,
        })
    }

    /// Decodes a `isize` from the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn read_isize(&mut self) -> EncodingResult<isize> {
        Ok(match self.ctxt.settings.size_repr.width {
            BitWidth::Bit8 => self.read_i8_size()? as isize,
            BitWidth::Bit16 => self.read_i16_size()? as isize,
            BitWidth::Bit32 => self.read_i32_size()? as isize,
            BitWidth::Bit64 => self.read_i64_size()? as isize,
            BitWidth::Bit128 => self.read_i128_size()? as isize,
        })
    }

    /// Decodes an unsigned enum variant from the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn read_uvariant(&mut self) -> EncodingResult<u128> {
        Ok(match self.ctxt.settings.variant_repr.width {
            BitWidth::Bit8 => self.read_u8_variant()? as _,
            BitWidth::Bit16 => self.read_u16_variant()? as _,
            BitWidth::Bit32 => self.read_u32_variant()? as _,
            BitWidth::Bit64 => self.read_u64_variant()? as _,
            BitWidth::Bit128 => self.read_u128_variant()? as _,
        })
    }

    /// Decodes a signed enum variant from the underlying stream, according to the endianness,
    /// numerical encoding and bit-width in the encoder's state
    pub fn read_ivariant(&mut self) -> EncodingResult<i128> {
        Ok(match self.ctxt.settings.variant_repr.width {
            BitWidth::Bit8 => self.read_i8_variant()? as _,
            BitWidth::Bit16 => self.read_i16_variant()? as _,
            BitWidth::Bit32 => self.read_i32_variant()? as _,
            BitWidth::Bit64 => self.read_i64_variant()? as _,
            BitWidth::Bit128 => self.read_i128_variant()? as _,
        })
    }

    /// Decodes the boolean state of a value. If the flatten attribute is set to Some,
    /// this function will return its value (converted to a bool) without reading from the
    /// underlying stream, otherwise it will behave identically to [`Self::read_bool`].
    pub fn read_state(&mut self) -> EncodingResult<bool> {
        if let Some(length) = self.ctxt.flatten() {
            match length {
                0 => Ok(false),
                1 => Ok(true),
                _ => Err(EncodingError::FlattenError(FlattenError::InvalidBool)),
            }
        } else {
            self.read_bool()
        }
    }

    /// Decodes a `bool` from the underlying stream, ignoring any encoding option.
    /// It is guaranteed that, one u8 is read from the underlying stream and, if
    /// it's equal to `1`, `true` is returned, if it's equal to `0`, `false` is returned,
    /// if it's equal to any other value, `InvalidBool` error will be returned
    pub fn read_bool(&mut self) -> EncodingResult<bool> {
        match self.read_u8_direct(NumEncoding::Fixed, Endianness::LittleEndian)? {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(EncodingError::InvalidBool),
        }
    }

    /// Decodes a `char` from the underlying stream, according to the endianness and string encoding
    /// in the encoder's state.
    pub fn read_char(&mut self) -> EncodingResult<char> {
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
    pub fn read_f32(&mut self) -> EncodingResult<f32> {
        Ok(f32::from_bits(self.read_u32_direct(
            NumEncoding::Fixed,
            self.ctxt.settings.num_repr.endianness,
        )?))
    }

    /// Decodes a `f64` from the underlying stream, ignoring the numeric encoding but respecting
    /// the endianness. Equivalent of `f64::from_bits(self.read_u64())` with the numeric
    /// encoding set to Fixed
    pub fn read_f64(&mut self) -> EncodingResult<f64> {
        Ok(f64::from_bits(self.read_u64_direct(
            NumEncoding::Fixed,
            self.ctxt.settings.num_repr.endianness,
        )?))
    }

    /// Decodes a String from the underlying stream, according to the endianness,
    /// string encoding and string-length encoding in the encoder's state
    pub fn read_string<S>(&mut self) -> EncodingResult<S>
    where
        S: FromIterator<char>,
    {
        let _encoding = self.ctxt.settings.string_repr.str_encoding;

        let length = self.read_length()?;

        struct CharIter<'iter, 'user, T: Read> {
            encoder: Encoder<'user, SizeLimit<&'iter mut T>>,
        }

        impl<'iter, 'user, T: Read> Iterator for CharIter<'iter, 'user, T> {
            type Item = EncodingResult<char>;
            fn next(&mut self) -> Option<Self::Item> {
                match self.encoder.read_char() {
                    err @ Err(EncodingError::UnexpectedEnd) => {
                        if self.encoder.stream.remaining_readable() != 0 {
                            // We reached the limit in the middle of a char
                            // This is a hard error
                            Some(err)
                        } else {
                            // We reached the limit on a char boundary
                            // this means we can end the iterator by returning None
                            None
                        }
                    }
                    any @ _ => Some(any),
                }
            }
        }

        let iter = CharIter {
            encoder: Encoder::new(SizeLimit::new(&mut self.stream, 0, length), self.ctxt),
        };

        iter.collect()
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
    /// use ende::{Context, Encoder};
    /// use ende::io::Slice;
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
        Ok(conv)
    }

    /// Borrows a `isize` slice of length `length` from the encoder.
    ///
    /// Checks that the [`Endianness`] and [`BitWidth`] match those of the target system,
    /// and that the [`NumEncoding`] is [`borrowable`][`NumEncoding::borrowable`]
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
    fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()>;
}

/// A binary data structure specification which can be **decoded** from its binary representation
/// into an owned type.
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
    fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self>;
}

/// A binary data structure specification which can be **decoded** from its binary representation
/// by borrowing the data.
pub trait BorrowDecode<'data>: Sized {
    /// Decodes `Self` from a binary format.<br>
    /// As a baseline, calling `decode` multiple times without changing the
    /// encoder settings or the underlying binary data in-between calls must produce
    /// the same output.<br>
    /// If the result is Ok,
    /// implementations should guarantee that the state of the encoder
    /// is preserved. If the result is Err,
    /// no guarantees are made about the state of the encoder,
    /// and users should reset it before reuse.<br>
    fn borrow_decode<Reader: BorrowRead<'data>>(
        decoder: &mut Encoder<Reader>,
    ) -> EncodingResult<Self>;
}
