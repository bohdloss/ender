#[cfg(test)]
mod test;

#[cfg(feature = "encryption")]
pub mod encryption;
#[cfg(feature = "compression")]
pub mod compression;
#[cfg(feature = "serde")]
pub mod serde;

use std::collections::HashMap;
use std::ffi::{CStr, CString, FromVecWithNulError};
use std::hash::Hash;
use std::io;
use std::io::{Read, Write};
use std::marker::PhantomData;
use std::string::FromUtf8Error;
use array_init::array_init;
use thiserror::Error;

#[cfg(feature = "derive")]
pub use ende_derive::{Encode, Decode};
use parse_display::Display;

/// Encodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the given options.
/// If you have the need of preserving resources, it is recommended to keep an instance
/// of [`BinStream`] instead of continuously calling this method.
pub fn encode_with<T: Write, V: Encode>(writer: &mut T, options: BinOptions, value: V) -> EncodingResult<()> {
	let mut stream = BinStream::new(writer, options);
	value.encode(&mut stream)
}

/// Encodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the given options and cryptostate.
/// If you have the need of preserving resources, it is recommended to keep an instance
/// of [`BinStream`] instead of continuously calling this method.
#[cfg(feature = "encryption")]
pub fn encode_with_crypto_params<T: Write, V: Encode>(writer: &mut T, options: BinOptions, crypto: encryption::CryptoState, value: V) -> EncodingResult<()> {
	let mut stream = BinStream::with_crypto_params(writer, options, crypto);
	value.encode(&mut stream)
}

/// Decodes the given type by constructing an encoder on the fly and using it to wrap the reader,
/// with the given options.
/// If you have the need of preserving resources, it is recommended to keep an instance
/// of [`BinStream`] instead of continuously calling this method.
pub fn decode_with<T: Read, V: Decode>(reader: &mut T, options: BinOptions) -> EncodingResult<V> {
	let mut stream = BinStream::new(reader, options);
	V::decode(&mut stream)
}

/// Decodes the given type by constructing an encoder on the fly and using it to wrap the reader,
/// with the given options and cryptostate.
/// If you have the need of preserving resources, it is recommended to keep an instance
/// of [`BinStream`] instead of continuously calling this method.
#[cfg(feature = "encryption")]
pub fn decode_with_crypto_params<T: Read, V: Decode>(reader: &mut T, options: BinOptions, crypto: encryption::CryptoState) -> EncodingResult<V> {
	let mut stream = BinStream::with_crypto_params(reader, options, crypto);
	V::decode(&mut stream)
}

/// Controls the endianness of a numerical value. Endianness is just
/// the order in which the value's bytes are written.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(u8)]
pub enum Endianness {
	/// Least significant bytes first
	LittleEndian,
	/// Most significant bytes first
	BigEndian
}

/// Controls the encoding of a numerical value
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(u8)]
pub enum NumEncoding {
	/// Its bits are encoded as-is according to endianness
	Fixed,
	/// Its bits are encoded according to the LEB128 (Little Endian Base 128) standard
	/// if unsigned, or ULEB128 standard if signed
	Leb128
}

/// How many bits a size or enum variant will occupy in the binary format. If the value
/// contains more bits, they will be trimmed (lost), so change this value with care
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(u8)]
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

/// The options (and state) of the [`BinStream`]. It is an aggregation of the [`NumRepr`],
/// the [`SizeRepr`], the [`VariantRepr`], and a flatten state variable used by `Vec`, `HashMap`
/// and other data structs with a length to omit writing/reading the length when the user requests
/// it.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("num_repr = ({num_repr}), size_repr = ({size_repr}), variant_repr = ({variant_repr})")]
pub struct BinOptions {
	pub num_repr: NumRepr,
	pub size_repr: SizeRepr,
	pub variant_repr: VariantRepr,
	/// The flatten state variable. When present, for `Option` it indicates in Encode mode
	/// not to write whether the optional is present, and in Decode mode that it is present (without
	/// checking), for `Vec`, `HashMap` and other data structures with a length it indicates in
	/// Encode mode not to write said length, and in Decode mode it indicates the length itself.
	pub flatten: Option<usize>,
}

impl BinOptions {
	/// Returns the default options, with flatten set to None and the default representations.
	/// See: [`NumRepr::new`], [`SizeRepr::new`], [`VariantRepr::new`]
	pub const fn new() -> Self {
		Self {
			num_repr: NumRepr::new(),
			size_repr: SizeRepr::new(),
			variant_repr: VariantRepr::new(),
			flatten: None
		}
	}

	/// Returns the state of the `flatten` variable, consuming it
	pub fn flatten(&mut self) -> Option<usize> {
		let old = self.flatten;
		self.flatten = None;
		old
	}
}

impl Default for BinOptions {
	fn default() -> Self {
		Self::new()
	}
}

/// A helper trait used to indicate that a type (usually a steam) can unwrap to its inner type
/// and perform some form of cleanup. This trait is implemented for Encryptors and Compressors
/// for example to pad the inner stream to the next full block size
pub trait Finish {
	type Output;
	fn finish(self) -> EncodingResult<Self::Output>;
}

/// The base type for encoding/decoding. Contains a stream, encoding options (and state)
/// and if the `encryption` feature is enabled, a [`encryption::CryptoState`].<br>
/// It's recommended to use wrap the stream in a [`std::io::BufReader`] or [`std::io::BufWriter`],
/// because many small write and read calls will be made
pub struct BinStream<T>{
	/// The underlying stream
	pub stream: T,
	/// The options/state
	pub options: BinOptions,
	#[cfg(feature = "encryption")]
	/// The cryptographic state. See [`encryption::CryptoState`]
	pub crypto: encryption::CryptoState
}

impl<T> BinStream<T> {
	/// Wraps the given stream and uses the given options.
	/// If the `encryption` feature is enabled, the cryptostate will
	/// be initialized to default
	pub fn new(stream: T, options: BinOptions) -> Self {
		Self {
			stream,
			options,
			#[cfg(feature = "encryption")]
			crypto: encryption::CryptoState::new()
		}
	}

	/// Wraps the given stream and uses the given options and cryptostate.
	#[cfg(feature = "encryption")]
	pub fn with_crypto_params(stream: T, options: BinOptions, crypto: encryption::CryptoState) -> Self {
		Self {
			stream,
			options,
			crypto
		}
	}

	/// Wraps the given stream and uses the default options.
	/// If the `encryption` feature is enabled, the cryptostate will
	/// be initialized to default
	pub fn new_default(stream: T) -> Self {
		Self {
			stream,
			options: Default::default(),
			#[cfg(feature = "encryption")]
			crypto: encryption::CryptoState::new()
		}
	}
}

impl<T> Finish for BinStream<T> {
	type Output = T;
	fn finish(self) -> EncodingResult<Self::Output> {
		Ok(self.stream)
	}
}

impl<T: Write> BinStream<T> {
	/// Returns a BinStream with the same options and crypto state,
	/// but wraps the underlying stream in an [`encryption::Encrypt`].
	/// When either the key or the iv are None, this function will try to fetch them
	/// from the cryptostate
	#[cfg(feature = "encryption")]
	pub fn add_encryption(&mut self, encryption: encryption::Encryption, key: Option<&[u8]>, iv: Option<&[u8]>) -> EncodingResult<BinStream<encryption::Encrypt<&mut T>>> {
		let options = self.options;
		let crypto = self.crypto.clone();
		let key = if let Some(key) = key {
			Some(key)
		} else {
			self.crypto.symm.get_key()
		};
		let iv = if let Some(iv) = iv {
			Some(iv)
		} else {
			self.crypto.symm.get_iv()
		};
		Ok(BinStream::with_crypto_params(encryption.encrypt(&mut self.stream, key, iv)?, options, crypto))
	}

	/// Returns a BinStream with the same options (and crypto state if the `encryption` feature is enabled),
	/// but wraps the underlying stream in an [`compression::Compress`]
	#[cfg(feature = "compression")]
	pub fn add_compression(&mut self, compression: compression::Compression) -> EncodingResult<BinStream<compression::Compress<&mut T>>> {
		let options = self.options;
		#[cfg(feature = "encryption")]
		let crypto = self.crypto.clone();

		#[cfg(not(feature = "encryption"))]
		return Ok(BinStream::new(compression.compress(&mut self.stream)?, options));
		#[cfg(feature = "encryption")]
		return Ok(BinStream::with_crypto_params(compression.compress(&mut self.stream)?, options, crypto));
	}
}

impl<T: Read> BinStream<T> {
	/// Returns a BinStream with the same options and crypto state,
	/// but wraps the underlying stream in an [`encryption::Decrypt`]
	/// When either the key or the iv are None, this function will try to fetch them
	/// from the cryptostate
	#[cfg(feature = "encryption")]
	pub fn add_decryption(&mut self, encryption: encryption::Encryption, key: Option<&[u8]>, iv: Option<&[u8]>) -> EncodingResult<BinStream<encryption::Decrypt<&mut T>>> {
		let options = self.options;
		let key = if let Some(key) = key {
			Some(key)
		} else {
			self.crypto.symm.get_key()
		};
		let iv = if let Some(iv) = iv {
			Some(iv)
		} else {
			self.crypto.symm.get_iv()
		};
		Ok(BinStream::new(encryption.decrypt(&mut self.stream, key, iv)?, options))
	}

	/// Returns a BinStream with the same options (and crypto state if the `encryption` feature is enabled),
	/// but wraps the underlying stream in an [`compression::Decompress`]
	#[cfg(feature = "compression")]
	pub fn add_decompression(&mut self, compression: compression::Compression) -> EncodingResult<BinStream<compression::Decompress<&mut T>>> {
		let options = self.options;
		#[cfg(feature = "encryption")]
			let crypto = self.crypto.clone();

		#[cfg(not(feature = "encryption"))]
		return Ok(BinStream::new(compression.decompress(&mut self.stream)?, options));
		#[cfg(feature = "encryption")]
		return Ok(BinStream::with_crypto_params(compression.decompress(&mut self.stream)?, options, crypto));
	}
}

macro_rules! make_unsigned_write_fn {
    ($write_internal:ident => $write_size:ident => $write_variant:ident => $write:ident => $ty:ty) => {
	    #[doc = "Encodes a `"]
	    #[doc = stringify!($ty)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $write(&mut self, value: $ty) -> EncodingResult<()> {
		    self.$write_internal(value, self.options.num_repr.num_encoding, self.options.num_repr.endianness)
	    }
	    
	    fn $write_size(&mut self, value: $ty) -> EncodingResult<()> {
		    self.$write_internal(value, self.options.size_repr.num_encoding, self.options.size_repr.endianness)
	    }
	    
	    fn $write_variant(&mut self, value: $ty) -> EncodingResult<()> {
		    self.$write_internal(value, self.options.variant_repr.num_encoding, self.options.variant_repr.endianness)
	    }

        fn $write_internal(&mut self, value: $ty, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<()> {
	        match num_encoding {
		        NumEncoding::Fixed => {
			        let bytes: [u8; std::mem::size_of::<$ty>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write_all(&bytes)?;
		        },
		        NumEncoding::Leb128 => {
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
		        }
	        }
            Ok(())
        }
    };
}

macro_rules! make_signed_write_fn {
    ($write_internal:ident => $write_size:ident => $write_variant:ident => $write:ident => $ty:ty) => {
	    #[doc = "Encodes a `"]
	    #[doc = stringify!($ty)]
	    #[doc = "` to the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $write(&mut self, value: $ty) -> EncodingResult<()> {
		    self.$write_internal(value, self.options.num_repr.num_encoding, self.options.num_repr.endianness)
	    }
	    
	    fn $write_size(&mut self, value: $ty) -> EncodingResult<()> {
		    self.$write_internal(value, self.options.size_repr.num_encoding, self.options.size_repr.endianness)
	    }
	    
	    fn $write_variant(&mut self, value: $ty) -> EncodingResult<()> {
		    self.$write_internal(value, self.options.variant_repr.num_encoding, self.options.variant_repr.endianness)
	    }

        fn $write_internal(&mut self, value: $ty, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<()> {
	        match num_encoding {
		        NumEncoding::Fixed => {
			        let bytes: [u8; std::mem::size_of::<$ty>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write_all(&bytes)?;
		        },
		        NumEncoding::Leb128 => {
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
		        }
	        }
            Ok(())
        }
    };
}

macro_rules! make_unsigned_read_fn {
    ($read_internal:ident => $read_size:ident => $read_variant:ident => $read:ident => $ty:ty) => {
	    #[doc = "Decodes a `"]
	    #[doc = stringify!($ty)]
	    #[doc = "` from the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $read(&mut self) -> EncodingResult<$ty> {
		    self.$read_internal(self.options.num_repr.num_encoding, self.options.num_repr.endianness)
	    }
	    
	    fn $read_size(&mut self) -> EncodingResult<$ty> {
		    self.$read_internal(self.options.size_repr.num_encoding, self.options.size_repr.endianness)
	    }
	    
	    fn $read_variant(&mut self) -> EncodingResult<$ty> {
		    self.$read_internal(self.options.variant_repr.num_encoding, self.options.variant_repr.endianness)
	    }
	    
        fn $read_internal(&mut self, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<$ty> {
	        Ok(match num_encoding {
		        NumEncoding::Fixed => {
			        let mut bytes: [u8; std::mem::size_of::<$ty>()] = [0u8; std::mem::size_of::<$ty>()];
		            self.stream.read_exact(&mut bytes)?;

		            match endianness {
			            Endianness::BigEndian => <$ty>::from_be_bytes(bytes),
			            Endianness::LittleEndian => <$ty>::from_le_bytes(bytes)
		            }
		        }
		        NumEncoding::Leb128 => {
			        let mut result: $ty = 0;
			        let mut byte = [0u8; 1];
			        let mut shift: u8 = 0;
			        loop {
				        if shift >= <$ty>::BITS as u8 {
					        return Err(EncodingError::VarIntError);
				        }
				        
			            self.stream.read_exact(&mut byte)?;
				        result |= (byte[0] & 0b0111_1111) as $ty << shift;
				        shift += 7;
				        
				        if (byte[0] & 0b1000_0000) == 0 {
					        break;
				        }
					}
			        result
		        }
	        })
        }
    };
}

macro_rules! make_signed_read_fn {
    ($read_internal:ident => $read_size:ident => $read_variant:ident => $read:ident => $ty:ty) => {
	    #[doc = "Decodes a `"]
	    #[doc = stringify!($ty)]
	    #[doc = "` from the underlying stream, according to the endianness and numerical encoding in the encoder's state"]
	    pub fn $read(&mut self) -> EncodingResult<$ty> {
		    self.$read_internal(self.options.num_repr.num_encoding, self.options.num_repr.endianness)
	    }
	    
	    fn $read_size(&mut self) -> EncodingResult<$ty> {
		    self.$read_internal(self.options.size_repr.num_encoding, self.options.size_repr.endianness)
	    }
	    
	    fn $read_variant(&mut self) -> EncodingResult<$ty> {
		    self.$read_internal(self.options.variant_repr.num_encoding, self.options.variant_repr.endianness)
	    }
	    
        fn $read_internal(&mut self, num_encoding: NumEncoding, endianness: Endianness) -> EncodingResult<$ty> {
	        Ok(match num_encoding {
		        NumEncoding::Fixed => {
			        let mut bytes: [u8; std::mem::size_of::<$ty>()] = [0u8; std::mem::size_of::<$ty>()];
		            self.stream.read_exact(&mut bytes)?;

		            match endianness {
			            Endianness::BigEndian => <$ty>::from_be_bytes(bytes),
			            Endianness::LittleEndian => <$ty>::from_le_bytes(bytes)
		            }
		        }
		        NumEncoding::Leb128 => {
			        let mut result: $ty = 0;
			        let mut byte = [0u8; 1];
			        let mut shift: u8 = 0;
			        loop {
				        if shift >= <$ty>::BITS as u8 {
					        return Err(EncodingError::VarIntError);
				        }
				        
			            self.stream.read_exact(&mut byte)?;
				        result |= (byte[0] & 0b0111_1111) as $ty << shift;
				        shift += 7;
				        
				        if (byte[0] & 0b1000_0000) == 0 {
					        break;
				        }
					}
			        
			        if shift < <$ty>::BITS as u8 && (byte[0] & 0b0100_0000) != 0 {
				        result |= (!0 << shift);
			        }
			        
			        result
		        }
	        })
        }
    };
}

impl<T: Write> BinStream<T> {
	make_unsigned_write_fn!(_write_u8 => _write_u8_size => _write_u8_variant => write_u8 => u8);
	make_unsigned_write_fn!(_write_u16 => _write_u16_size => _write_u16_variant => write_u16 => u16);
	make_unsigned_write_fn!(_write_u32 => _write_u32_size => _write_u32_variant => write_u32 => u32);
	make_unsigned_write_fn!(_write_u64 => _write_u64_size => _write_u64_variant => write_u64 => u64);
	make_unsigned_write_fn!(_write_u128 => _write_u128_size => _write_u128_variant => write_u128 => u128);
	make_signed_write_fn!(_write_i8 => _write_i8_size => _write_i8_variant => write_i8 => i8);
	make_signed_write_fn!(_write_i16 => _write_i16_size => _write_i16_variant => write_i16 => i16);
	make_signed_write_fn!(_write_i32 => _write_i32_size => _write_i32_variant => write_i32 => i32);
	make_signed_write_fn!(_write_i64 => _write_i64_size => _write_i64_variant => write_i64 => i64);
	make_signed_write_fn!(_write_i128 => _write_i128_size => _write_i128_variant => write_i128 => i128);

	/// Encodes a length. If the flatten attribute is set to Some, this function is a no-op,
	/// otherwise it will behave identically to [`Self::write_usize`].
	pub fn write_length(&mut self, value: usize) -> EncodingResult<()> {
		if self.options.flatten().is_none() {
			self.write_usize(value)?;
		}
		Ok(())
	}

	/// Encodes a `usize` to the underlying stream, according to the endianness,
	/// numerical encoding, bit-width and max size in the encoder's state
	pub fn write_usize(&mut self, value: usize) -> EncodingResult<()> {
		if value > self.options.size_repr.max_size {
			return Err(EncodingError::MaxLengthExceeded {
				max: self.options.size_repr.max_size,
				requested: value
			})
		}
		match self.options.size_repr.width {
			BitWidth::Bit8 => self._write_u8_size(value as _),
			BitWidth::Bit16 => self._write_u16_size(value as _),
			BitWidth::Bit32 => self._write_u32_size(value as _),
			BitWidth::Bit64 => self._write_u64_size(value as _),
			BitWidth::Bit128 => self._write_u128_size(value as _),
		}
	}

	/// Encodes a `isize` to the underlying stream, according to the endianness,
	/// numerical encoding, bit-width and max size in the encoder's state
	pub fn write_isize(&mut self, value: isize) -> EncodingResult<()> {
		if value >= 0 && value as usize > self.options.size_repr.max_size {
			return Err(EncodingError::MaxLengthExceeded {
				max: self.options.size_repr.max_size,
				requested: value as usize
			})
		}
		match self.options.size_repr.width {
			BitWidth::Bit8 => self._write_i8_size(value as _),
			BitWidth::Bit16 => self._write_i16_size(value as _),
			BitWidth::Bit32 => self._write_i32_size(value as _),
			BitWidth::Bit64 => self._write_i64_size(value as _),
			BitWidth::Bit128 => self._write_i128_size(value as _),
		}
	}

	/// Encodes an unsigned enum variant to the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn write_uvariant(&mut self, value: u128) -> EncodingResult<()> {
		match self.options.variant_repr.width {
			BitWidth::Bit8 => self._write_u8_variant(value as _),
			BitWidth::Bit16 => self._write_u16_variant(value as _),
			BitWidth::Bit32 => self._write_u32_variant(value as _),
			BitWidth::Bit64 => self._write_u64_variant(value as _),
			BitWidth::Bit128 => self._write_u128_variant(value as _),
		}
	}

	/// Encodes a signed enum variant to the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn write_ivariant(&mut self, value: i128) -> EncodingResult<()> {
		match self.options.variant_repr.width {
			BitWidth::Bit8 => self._write_i8_variant(value as _),
			BitWidth::Bit16 => self._write_i16_variant(value as _),
			BitWidth::Bit32 => self._write_i32_variant(value as _),
			BitWidth::Bit64 => self._write_i64_variant(value as _),
			BitWidth::Bit128 => self._write_i128_variant(value as _),
		}
	}

	/// Encodes a `bool` to the underlying stream, ignoring any encoding option.
	/// It is guaranteed that, if `value` is `true`, a single u8 will be written to the
	/// underlying stream with the value `1`, and if `value` is `false`, with a value of `0`
	pub fn write_bool(&mut self, value: bool) -> EncodingResult<()> {
		self._write_u8(value as u8, NumEncoding::Fixed, Endianness::LittleEndian)
	}

	/// FIXME Decide how chars should be encoded
	pub fn write_char(&mut self, value: char) -> EncodingResult<()> {
		self.write_u32(value as u32)
	}

	/// Encodes a `f32` to the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `Self::write_u32(value.to_bits())` with the numeric
	/// encoding set to Fixed
	pub fn write_f32(&mut self, value: f32) -> EncodingResult<()> {
		self._write_u32(value.to_bits(), NumEncoding::Fixed, self.options.num_repr.endianness)
	}

	/// Encodes a `f64` to the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `Self::write_u64(value.to_bits())` with the numeric
	/// encoding set to Fixed
	pub fn write_f64(&mut self, value: f64) -> EncodingResult<()> {
		self._write_u64(value.to_bits(), NumEncoding::Fixed, self.options.num_repr.endianness)
	}

	/// Writes the given slice to the underlying stream as-is.
	pub fn write_raw_bytes(&mut self, bytes: &[u8]) -> EncodingResult<()> {
		Ok(self.stream.write_all(bytes)?)
	}
}

impl<T: Read> BinStream<T> {
	make_unsigned_read_fn!(_read_u8 => _read_u8_size => _read_u8_variant => read_u8 => u8);
	make_unsigned_read_fn!(_read_u16 => _read_u16_size => _read_u16_variant => read_u16 => u16);
	make_unsigned_read_fn!(_read_u32 => _read_u32_size => _read_u32_variant => read_u32 => u32);
	make_unsigned_read_fn!(_read_u64 => _read_u64_size => _read_u64_variant => read_u64 => u64);
	make_unsigned_read_fn!(_read_u128 => _read_u128_size => _read_u128_variant => read_u128 => u128);
	make_signed_read_fn!(_read_i8 => _read_i8_size => _read_i8_variant => read_i8 => i8);
	make_signed_read_fn!(_read_i16 => _read_i16_size => _read_i16_variant => read_i16 => i16);
	make_signed_read_fn!(_read_i32 => _read_i32_size => _read_i32_variant => read_i32 => i32);
	make_signed_read_fn!(_read_i64 => _read_i64_size => _read_i64_variant => read_i64 => i64);
	make_signed_read_fn!(_read_i128 => _read_i128_size => _read_i128_variant => read_i128 => i128);

	/// Decodes a length. If the flatten attribute is set to Some, this function
	/// will return its value, otherwise it will behave identically to [`Self::read_usize`].
	pub fn read_length(&mut self) -> EncodingResult<usize> {
		if let Some(length) = self.options.flatten() {
			Ok(length)
		} else {
			self.read_usize()
		}
	}

	/// Decodes a `usize` from the underlying stream, according to the endianness,
	/// numerical encoding, bit-width and max size in the encoder's state
	pub fn read_usize(&mut self) -> EncodingResult<usize> {
		let value = match self.options.size_repr.width {
			BitWidth::Bit8 => self._read_u8_size()? as usize,
			BitWidth::Bit16 => self._read_u16_size()? as usize,
			BitWidth::Bit32 => self._read_u32_size()? as usize,
			BitWidth::Bit64 => self._read_u64_size()? as usize,
			BitWidth::Bit128 => self._read_u128_size()? as usize,
		};
		if value > self.options.size_repr.max_size {
			return Err(EncodingError::MaxLengthExceeded {
				max: self.options.size_repr.max_size,
				requested: value
			})
		}
		Ok(value)
	}

	/// Decodes a `isize` from the underlying stream, according to the endianness,
	/// numerical encoding, bit-width and max size in the encoder's state
	pub fn read_isize(&mut self) -> EncodingResult<isize> {
		let value = match self.options.size_repr.width {
			BitWidth::Bit8 => self._read_i8_size()? as isize,
			BitWidth::Bit16 => self._read_i16_size()? as isize,
			BitWidth::Bit32 => self._read_i32_size()? as isize,
			BitWidth::Bit64 => self._read_i64_size()? as isize,
			BitWidth::Bit128 => self._read_i128_size()? as isize,
		};
		if value >= 0 && value as usize > self.options.size_repr.max_size {
			return Err(EncodingError::MaxLengthExceeded {
				max: self.options.size_repr.max_size,
				requested: value as usize
			})
		}
		Ok(value)
	}

	/// Decodes an unsigned enum variant from the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn read_uvariant(&mut self) -> EncodingResult<u128> {
		Ok(match self.options.variant_repr.width {
			BitWidth::Bit8 => self._read_u8_variant()? as _,
			BitWidth::Bit16 => self._read_u16_variant()? as _,
			BitWidth::Bit32 => self._read_u32_variant()? as _,
			BitWidth::Bit64 => self._read_u64_variant()? as _,
			BitWidth::Bit128 => self._read_u128_variant()? as _,
		})
	}

	/// Decodes a signed enum variant from the underlying stream, according to the endianness,
	/// numerical encoding and bit-width in the encoder's state
	pub fn read_ivariant(&mut self) -> EncodingResult<i128> {
		Ok(match self.options.variant_repr.width {
			BitWidth::Bit8 => self._read_i8_variant()? as _,
			BitWidth::Bit16 => self._read_i16_variant()? as _,
			BitWidth::Bit32 => self._read_i32_variant()? as _,
			BitWidth::Bit64 => self._read_i64_variant()? as _,
			BitWidth::Bit128 => self._read_i128_variant()? as _,
		})
	}

	/// Decodes a `bool` from the underlying stream, ignoring any encoding option.
	/// It is guaranteed that, one u8 is read from the underlying stream and, if
	/// it's equal to `1`, `true` is returned, if it's equal to `0`, `false` is returned,
	/// if it's equal to any other value, `InvalidBool` error will be returned
	pub fn read_bool(&mut self) -> EncodingResult<bool> {
		match self._read_u8(NumEncoding::Fixed, Endianness::LittleEndian)? {
			0 => Ok(false),
			1 => Ok(true),
			_ => Err(EncodingError::InvalidBool)
		}
	}

	/// FIXME Decide how chars should be decoded
	pub fn read_char(&mut self) -> EncodingResult<char> {
		char::from_u32(self.read_u32()?).ok_or(EncodingError::InvalidChar)
	}

	/// Decodes a `f32` from the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `f32::from_bits(self.read_u32())` with the numeric
	/// encoding set to Fixed
	pub fn read_f32(&mut self) -> EncodingResult<f32> {
		Ok(f32::from_bits(self._read_u32(NumEncoding::Fixed, self.options.num_repr.endianness)?))
	}

	/// Decodes a `f64` from the underlying stream, ignoring the numeric encoding but respecting
	/// the endianness. Equivalent of `f64::from_bits(self.read_u64())` with the numeric
	/// encoding set to Fixed
	pub fn read_f64(&mut self) -> EncodingResult<f64> {
		Ok(f64::from_bits(self._read_u64(NumEncoding::Fixed, self.options.num_repr.endianness)?))
	}

	/// Reads `buf.len()` bytes from the stream to the buffer as-is.
	pub fn read_raw_bytes(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		Ok(self.stream.read_exact(buf)?)
	}
}

/// Represents any kind of error that can happen during encoding and decoding
#[derive(Debug, Error)]
pub enum EncodingError {
	/// Generic IO error
	#[error("IO Error occurred: {0}")]
	IOError(
		#[source]
		#[from]
		io::Error
	),
	/// A var-int was malformed and could not be decoded
	#[error("Malformed var-int encoding")]
	VarIntError,
	/// An invalid character value was read
	#[error("Invalid char value")]
	InvalidChar,
	/// A value other than `1` or `0` was read while decoding a `bool`
	#[error("Invalid bool value")]
	InvalidBool,
	/// Tried to write or read a length greater than the max
	#[error("A length of {requested} exceeded the max allowed value of {max}")]
	MaxLengthExceeded {
		max: usize,
		requested: usize
	},
	/// A string contained invalid UTF8 bytes and couldn't be decoded
	#[error("Invalid string value: {0}")]
	InvalidString(
		#[source]
		#[from]
		FromUtf8Error
	),
	/// A c-like string wasn't null terminated
	#[error("Invalid c-string value: {0}")]
	InvalidCString(
		#[source]
		#[from]
		FromVecWithNulError
	),
	/// Tried to decode an unrecognized enum variant
	#[error("Unrecognized enum variant")]
	InvalidVariant,
	/// A `#[ende(validate = ...)]` check failed
	#[error("Validation error: {0}")]
	ValidationError(String),
	/// A generic serde error occurred
	#[cfg(feature = "serde")]
	#[error("Serde error occurred: {0}")]
	SerdeError(String),
	/// A cryptographic error occurred
	#[cfg(feature = "encryption")]
	#[error("Cryptographic error: {0}")]
	EncryptionError(
		#[source]
		#[from]
		encryption::CryptoError
	),
	/// A compression error occurred
	#[cfg(feature = "compression")]
	#[error("Compression error: {0}")]
	CompressionError(
		#[source]
		#[from]
		compression::CompressionError
	)
}

/// A convenience alias to `Result<T, EncodingError>`
pub type EncodingResult<T> = Result<T, EncodingError>;

/// The base trait for anything that can be Encoded.
/// Indicates that a type can be converted into a sequence of bytes
pub trait Encode {
	/// Encodes `self` into a binary format.<br>
	/// If the result is Ok,
	/// implementations should guarantee that the state of the encoder
	/// is the same as before calling this function. If the result is Err,
	/// no guarantees should be made about the state of the encoder,
	/// and users should reset it before reuse.<br>
	/// Implementation are discouraged from writing `encode` implementations
	/// that modify `self` through interior mutability
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()>;
}

/// The base trait for anything that can be Decoded.
/// Indicates that a sequence of bytes can be converted back into a type
pub trait Decode {
	/// Decodes `Self` from a binary format.<br>
	/// If the result is Ok,
	/// implementations should guarantee that the state of the encoder
	/// is the same as before calling this function. If the result is Err,
	/// no guarantees should be made about the state of the encoder,
	/// and users should reset it before reuse.<br>
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized;
}

// Primitives

macro_rules! impl_primitives {
    ($($ty:ty => $write:ident => $read:ident);* $(;)? ) => {
	    $(
	    impl Encode for $ty {
		    fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		        encoder.$write(*self)
		    }
	    }
	    impl Decode for $ty {
		    fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		        decoder.$read()
		    }
	    }
	    )*
    };
}

impl_primitives!{
	u8 => write_u8 => read_u8;
	u16 => write_u16 => read_u16;
	u32 => write_u32 => read_u32;
	u64 => write_u64 => read_u64;
	u128 => write_u128 => read_u128;
	i8 => write_i8 => read_i8;
	i16 => write_i16 => read_i16;
	i32 => write_i32 => read_i32;
	i64 => write_i64 => read_i64;
	i128 => write_i128 => read_i128;
	bool => write_bool => read_bool;
	char => write_char => read_char;
	f32 => write_f32 => read_f32;
	f64 => write_f64 => read_f64;
	usize => write_usize => read_usize;
	isize => write_isize => read_isize;
}

// STRINGS

impl Encode for String {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		encoder.write_raw_bytes(self.as_bytes())
	}
}

impl Decode for String {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		let len = decoder.read_length()?;
		let mut buffer = vec![0u8; len];
		decoder.read_raw_bytes(&mut buffer)?;
		Ok(String::from_utf8(buffer)?)
	}
}

impl Encode for &str {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		encoder.write_raw_bytes(self.as_bytes())
	}
}

// CSTRING

impl Encode for CString {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		if encoder.options.flatten().is_some() {
			encoder.write_raw_bytes(self.as_bytes())
		} else {
			encoder.write_raw_bytes(self.as_bytes_with_nul())
		}
	}
}

impl Decode for CString {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		if let Some(length) = decoder.options.flatten() {
			let mut buffer = vec![0; length + 1];
			decoder.read_raw_bytes(&mut buffer[..length])?;
			Ok(CString::from_vec_with_nul(buffer)?)
		} else {
			let mut last_byte: u8;
			let mut buffer = Vec::new();
			while { last_byte = decoder.read_u8()?; last_byte != 0 } {
				buffer.push(last_byte);
			}
			buffer.push(0u8);
			Ok(CString::from_vec_with_nul(buffer)?)
		}
	}
}

impl Encode for CStr {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		if encoder.options.flatten().is_some() {
			encoder.write_raw_bytes(self.to_bytes())
		} else {
			encoder.write_raw_bytes(self.to_bytes_with_nul())
		}
	}
}

// Option

impl<T: Encode> Encode for Option<T> {
	fn encode<G: Write>(&self, encoder: &mut BinStream<G>) -> EncodingResult<()> {
		if encoder.options.flatten().is_some() {
			match self {
				None => Ok(()),
				Some(x) => {
					x.encode(encoder)
				}
			}
		} else {
			match self {
				None => encoder.write_bool(false),
				Some(value) => {
					encoder.write_bool(true)?;
					value.encode(encoder)
				}
			}
		}
	}
}

impl<T: Decode> Decode for Option<T> {
	fn decode<G: Read>(decoder: &mut BinStream<G>) -> EncodingResult<Self> where Self: Sized {
		if decoder.options.flatten().is_some() {
			Ok(Some(T::decode(decoder)?))
		} else {
			Ok(match decoder.read_bool()? {
				true => Some(T::decode(decoder)?),
				false => None
			})
		}
	}
}

// Slice

impl<T: Encode> Encode for &[T] {
	fn encode<G: Write>(&self, encoder: &mut BinStream<G>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for i in 0..self.len() {
			self[i].encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Encode, const SIZE: usize> Encode for [T; SIZE] {
	fn encode<G: Write>(&self, encoder: &mut BinStream<G>) -> EncodingResult<()> {
		for i in 0..SIZE {
			self[i].encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Decode + Default, const SIZE: usize> Decode for [T; SIZE] {
	fn decode<G: Read>(decoder: &mut BinStream<G>) -> EncodingResult<Self> where Self: Sized {
		let mut uninit = array_init(|_| T::decode(decoder).unwrap());
		for i in 0..SIZE {
			uninit[i] = T::decode(decoder)?;
		}
		Ok(uninit)
	}
}

// Vec

impl<T: Encode> Encode for Vec<T> {
	fn encode<G: Write>(&self, encoder: &mut BinStream<G>) -> EncodingResult<()> {
		Encode::encode(&self.as_slice(), encoder)
	}
}

impl<T: Decode> Decode for Vec<T> {
	fn decode<G: Read>(decoder: &mut BinStream<G>) -> EncodingResult<Self> where Self: Sized {
		let size = decoder.read_length()?;
		let mut vec = Vec::with_capacity(size);
		for _ in 0..size {
			vec.push(Decode::decode(decoder)?);
		}
		Ok(vec)
	}
}

// Maps

impl<K: Encode, V: Encode> Encode for HashMap<K, V> {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for (k, v) in self.iter() {
			k.encode(encoder)?;
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl<K: Decode + Eq + Hash, V: Decode> Decode for HashMap<K, V> {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		let size = decoder.read_length()?;
		let mut map = HashMap::with_capacity(size);
		for _ in 0..size {
			map.insert(K::decode(decoder)?, V::decode(decoder)?);
		}
		Ok(map)
	}
}

// Phantom data

impl<T> Encode for PhantomData<T> {
	fn encode<G: Write>(&self, _encoder: &mut BinStream<G>) -> EncodingResult<()> {
		Ok(())
	}
}

impl<T> Decode for PhantomData<T> {
	fn decode<G: Read>(_decoder: &mut BinStream<G>) -> EncodingResult<Self> where Self: Sized {
		Ok(Self)
	}
}

// Unit

impl Encode for () {
	fn encode<T: Write>(&self, _encoder: &mut BinStream<T>) -> EncodingResult<()> {
		Ok(())
	}
}

impl Decode for () {
	fn decode<T: Read>(_decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		Ok(())
	}
}

// Some tuples

macro_rules! consume {
    ($x:tt, $expr:expr) => {
	    $expr
    };
}

macro_rules! tuple_impl {
    ($($name:ident)+) => {
	    #[automatically_derived]
	    #[allow(non_snake_case)]
	    impl<$($name: $crate::Encode),+> $crate::Encode for ($($name),+) {
		    fn encode<__T: Write>(&self, encoder: &mut BinStream<__T>) -> EncodingResult<()> {
		        let ($($name),*) = self;
			    $(
			        $crate::Encode::encode($name, encoder)?;
			    )+
			    Ok(())
		    }
	    }
	    
	    #[automatically_derived]
	    #[allow(non_snake_case)]
	    impl<$($name: $crate::Decode),+> $crate::Decode for ($($name),+) {
		    fn decode<__T: Read>(decoder: &mut BinStream<__T>) -> EncodingResult<Self> where Self: Sized {
			    Ok(($(
		            consume!($name, $crate::Decode::decode(decoder)?),
		        )+))
		    }
	    }
    };
}

tuple_impl! { A B }
tuple_impl! { A B C }
tuple_impl! { A B C D }
tuple_impl! { A B C D E }
tuple_impl! { A B C D E F }
tuple_impl! { A B C D E F G }
tuple_impl! { A B C D E F G H }
tuple_impl! { A B C D E F G H I }
tuple_impl! { A B C D E F G H I J }
tuple_impl! { A B C D E F G H I J K }
tuple_impl! { A B C D E F G H I J K L }
tuple_impl! { A B C D E F G H I J K L M }
tuple_impl! { A B C D E F G H I J K L M N }
tuple_impl! { A B C D E F G H I J K L M N O }
tuple_impl! { A B C D E F G H I J K L M N O P } // Up to 16