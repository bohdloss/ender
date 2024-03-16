#[cfg(test)]
mod test;

#[cfg(feature = "encryption")]
pub mod encryption;
#[cfg(feature = "compression")]
pub mod compression;

use std::ffi::{CStr, CString, FromVecWithNulError};
use std::io;
use std::io::{Read, Write};
use std::marker::PhantomData;
use std::string::FromUtf8Error;
use array_init::array_init;
use thiserror::Error;

pub use ende_derive::{Encode, Decode};
use parse_display::Display;

pub fn encode_with<T: Write, V: Encode>(writer: T, options: BinOptions, value: V) -> EncodingResult<()> {
	let mut stream = BinStream::new(writer, options);
	value.encode(&mut stream)
}

pub fn decode_with<T: Read, V: Decode>(reader: T, options: BinOptions) -> EncodingResult<V> {
	let mut stream = BinStream::new(reader, options);
	V::decode(&mut stream)
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(u8)]
pub enum Endianness {
	LittleEndian,
	BigEndian
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(u8)]
pub enum NumEncoding {
	FixedInt,
	Leb128Int
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(u8)]
pub enum BitWidth {
	Bit8,
	Bit16,
	Bit32,
	Bit64,
	Bit128
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("endianness = {endianness}, encoding = {num_encoding}")]
pub struct NumRepr {
	pub endianness: Endianness,
	pub num_encoding: NumEncoding,
}

impl NumRepr {
	pub const fn new() -> Self {
		Self {
			endianness: Endianness::LittleEndian,
			num_encoding: NumEncoding::FixedInt
		}
	}
}

impl Default for NumRepr {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("endianness = {endianness} , encoding = {num_encoding}, bit_width = {width}, max_size = {max_size}")]
pub struct SizeRepr {
	pub endianness: Endianness,
	pub num_encoding: NumEncoding,
	pub width: BitWidth,
	pub max_size: usize
}

impl SizeRepr {
	pub const fn new() -> Self {
		Self {
			endianness: Endianness::LittleEndian,
			num_encoding: NumEncoding::FixedInt,
			width: BitWidth::Bit64,
			max_size: u32::MAX as usize
		}
	}
}

impl Default for SizeRepr {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("endianness = {endianness} , encoding = {num_encoding}, bit_width = {width}")]
pub struct VariantRepr {
	pub endianness: Endianness,
	pub num_encoding: NumEncoding,
	pub width: BitWidth
}

impl VariantRepr {
	pub const fn new() -> Self {
		Self {
			endianness: Endianness::LittleEndian,
			num_encoding: NumEncoding::FixedInt,
			width: BitWidth::Bit32
		}
	}
}

impl Default for VariantRepr {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("num_repr = ({num_repr}), size_repr = ({size_repr}), variant_repr = ({variant_repr})")]
pub struct BinOptions {
	pub num_repr: NumRepr,
	pub size_repr: SizeRepr,
	pub variant_repr: VariantRepr,
	pub flatten: u64,
}

impl BinOptions {
	pub const fn new() -> Self {
		Self {
			num_repr: NumRepr::new(),
			size_repr: SizeRepr::new(),
			variant_repr: VariantRepr::new(),
			flatten: 0
		}
	}

	pub fn flatten(&mut self) -> bool {
		if self.flatten == 0 {
			false
		} else {
			self.flatten -= 1;
			true
		}
	}
}

impl Default for BinOptions {
	fn default() -> Self {
		Self::new()
	}
}

pub struct BinStream<T>{
	pub stream: T,
	pub options: BinOptions
}

impl<T> BinStream<T> {
	pub fn new(stream: T, options: BinOptions) -> Self {
		Self {
			stream,
			options
		}
	}
	
	pub fn new_default(stream: T) -> Self {
		Self {
			stream,
			options: Default::default()
		}
	}

	pub fn finish(self) -> T {
		self.stream
	}
}

macro_rules! make_unsigned_write_fn {
    ($write_internal:ident => $write_size:ident => $write_variant:ident => $write:ident => $ty:ty) => {
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
		        NumEncoding::FixedInt => {
			        let bytes: [u8; std::mem::size_of::<$ty>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write_all(&bytes)?;
		        },
		        NumEncoding::Leb128Int => {
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
		        NumEncoding::FixedInt => {
			        let bytes: [u8; std::mem::size_of::<$ty>()] = match endianness {
			            Endianness::BigEndian => value.to_be_bytes(),
			            Endianness::LittleEndian => value.to_le_bytes()
		            };
		            self.stream.write_all(&bytes)?;
		        },
		        NumEncoding::Leb128Int => {
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
		        NumEncoding::FixedInt => {
			        let mut bytes: [u8; std::mem::size_of::<$ty>()] = [0u8; std::mem::size_of::<$ty>()];
		            self.stream.read_exact(&mut bytes)?;

		            match endianness {
			            Endianness::BigEndian => <$ty>::from_be_bytes(bytes),
			            Endianness::LittleEndian => <$ty>::from_le_bytes(bytes)
		            }
		        }
		        NumEncoding::Leb128Int => {
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
		        NumEncoding::FixedInt => {
			        let mut bytes: [u8; std::mem::size_of::<$ty>()] = [0u8; std::mem::size_of::<$ty>()];
		            self.stream.read_exact(&mut bytes)?;

		            match endianness {
			            Endianness::BigEndian => <$ty>::from_be_bytes(bytes),
			            Endianness::LittleEndian => <$ty>::from_le_bytes(bytes)
		            }
		        }
		        NumEncoding::Leb128Int => {
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
	pub fn write_uvariant(&mut self, value: u128) -> EncodingResult<()> {
		match self.options.variant_repr.width {
			BitWidth::Bit8 => self._write_u8_variant(value as _),
			BitWidth::Bit16 => self._write_u16_variant(value as _),
			BitWidth::Bit32 => self._write_u32_variant(value as _),
			BitWidth::Bit64 => self._write_u64_variant(value as _),
			BitWidth::Bit128 => self._write_u128_variant(value as _),
		}
	}
	pub fn write_ivariant(&mut self, value: i128) -> EncodingResult<()> {
		match self.options.variant_repr.width {
			BitWidth::Bit8 => self._write_i8_variant(value as _),
			BitWidth::Bit16 => self._write_i16_variant(value as _),
			BitWidth::Bit32 => self._write_i32_variant(value as _),
			BitWidth::Bit64 => self._write_i64_variant(value as _),
			BitWidth::Bit128 => self._write_i128_variant(value as _),
		}
	}
	pub fn write_bool(&mut self, value: bool) -> EncodingResult<()> {
		self._write_u8(value as u8, NumEncoding::FixedInt, Endianness::LittleEndian)
	}
	pub fn write_char(&mut self, value: char) -> EncodingResult<()> {
		self.write_u32(value as u32)
	}
	pub fn write_f32(&mut self, value: f32) -> EncodingResult<()> {
		self._write_u32(value.to_bits(), NumEncoding::FixedInt, self.options.num_repr.endianness)
	}
	pub fn write_f64(&mut self, value: f64) -> EncodingResult<()> {
		self._write_u64(value.to_bits(), NumEncoding::FixedInt, self.options.num_repr.endianness)
	}
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
	pub fn read_uvariant(&mut self) -> EncodingResult<u128> {
		Ok(match self.options.variant_repr.width {
			BitWidth::Bit8 => self._read_u8_variant()? as _,
			BitWidth::Bit16 => self._read_u16_variant()? as _,
			BitWidth::Bit32 => self._read_u32_variant()? as _,
			BitWidth::Bit64 => self._read_u64_variant()? as _,
			BitWidth::Bit128 => self._read_u128_variant()? as _,
		})
	}
	pub fn read_ivariant(&mut self) -> EncodingResult<i128> {
		Ok(match self.options.variant_repr.width {
			BitWidth::Bit8 => self._read_i8_variant()? as _,
			BitWidth::Bit16 => self._read_i16_variant()? as _,
			BitWidth::Bit32 => self._read_i32_variant()? as _,
			BitWidth::Bit64 => self._read_i64_variant()? as _,
			BitWidth::Bit128 => self._read_i128_variant()? as _,
		})
	}
	pub fn read_bool(&mut self) -> EncodingResult<bool> {
		match self._read_u8(NumEncoding::FixedInt, Endianness::LittleEndian)? {
			0 => Ok(false),
			1 => Ok(true),
			_ => Err(EncodingError::InvalidBool)
		}
	}
	pub fn read_char(&mut self) -> EncodingResult<char> {
		char::from_u32(self.read_u32()?).ok_or(EncodingError::InvalidChar)
	}
	pub fn read_f32(&mut self) -> EncodingResult<f32> {
		Ok(f32::from_bits(self._read_u32(NumEncoding::FixedInt, self.options.num_repr.endianness)?))
	}
	pub fn read_f64(&mut self) -> EncodingResult<f64> {
		Ok(f64::from_bits(self._read_u64(NumEncoding::FixedInt, self.options.num_repr.endianness)?))
	}
	pub fn read_raw_bytes(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		Ok(self.stream.read_exact(buf)?)
	}
}

#[derive(Debug, Error)]
pub enum EncodingError {
	#[error("IO Error occurred: {0}")]
	IOError(
		#[source]
		#[from]
		io::Error
	),
	#[error("Malformed var-int encoding")]
	VarIntError,
	#[error("Invalid char value")]
	InvalidChar,
	#[error("Invalid bool value")]
	InvalidBool,
	#[error("A length (like a string length or array length) of {requested} exceeded the max allowed value of {max}")]
	MaxLengthExceeded {
		max: usize,
		requested: usize
	},
	#[error("Invalid string value: {0}")]
	InvalidString(
		#[source]
		#[from]
		FromUtf8Error
	),
	#[error("Invalid c-string value: {0}")]
	InvalidCString(
		#[source]
		#[from]
		FromVecWithNulError
	),
	#[error("Unrecognized enum variant")]
	InvalidVariant,
	#[cfg(feature = "serde")]
	#[error("Serde error occurred: {0}")]
	SerdeError(String),
	#[cfg(feature = "encryption")]
	#[error("Cryptographic error: {0}")]
	EncryptionError(
		#[source]
		#[from]
		encryption::CryptoError
	)
}

pub type EncodingResult<T> = Result<T, EncodingError>;

pub trait Encode {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()>;
}

pub trait Decode {
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
		encoder.write_usize(self.len())?;
		encoder.write_raw_bytes(self.as_bytes())
	}
}

impl Decode for String {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		let len = decoder.read_usize()?;
		let mut buffer = vec![0u8; len];
		decoder.read_raw_bytes(&mut buffer)?;
		Ok(String::from_utf8(buffer)?)
	}
}

impl Encode for &str {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		encoder.write_usize(self.len())?;
		encoder.write_raw_bytes(self.as_bytes())
	}
}

// CSTRING

impl Encode for CString {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		encoder.write_raw_bytes(self.as_bytes_with_nul())
	}
}

impl Decode for CString {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		let mut last_byte: u8;
		let mut buffer = Vec::new();
		while { last_byte = decoder.read_u8()?; last_byte != 0 } {
			buffer.push(last_byte);
		}
		buffer.push(0u8);
		Ok(CString::from_vec_with_nul(buffer)?)
	}
}

impl Encode for CStr {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		encoder.write_raw_bytes(self.to_bytes_with_nul())
	}
}

// Option

impl<T: Encode> Encode for Option<T> {
	fn encode<G: Write>(&self, encoder: &mut BinStream<G>) -> EncodingResult<()> {
		if encoder.options.flatten() {
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
		if decoder.options.flatten() {
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
		encoder.write_usize(self.len())?;
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
		let size = decoder.read_usize()?;
		let mut vec = Vec::with_capacity(size);
		for _ in 0..size {
			vec.push(Decode::decode(decoder)?);
		}
		Ok(vec)
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

// Optional serde implementation

#[cfg(feature = "serde")]
mod serde {
	use std::fmt::Display;
	use std::io::{Read, Write};
	use serde::{de, Deserializer, ser, Serialize, Serializer};
	use serde::ser::{SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple, SerializeTupleStruct, SerializeTupleVariant};
	use serde::de::{DeserializeSeed, EnumAccess, MapAccess, SeqAccess, VariantAccess, Visitor};
	use crate::{BinStream, Decode, Encode, EncodingError};

	impl ser::Error for EncodingError {
		fn custom<T>(msg: T) -> Self where T: Display {
			Self::SerdeError(msg.to_string())
		}
	}

	impl de::Error for EncodingError {
		fn custom<T>(msg: T) -> Self where T: Display {
			Self::SerdeError(msg.to_string())
		}
	}

	impl<T: Write> Serializer for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;
		type SerializeSeq = Self;
		type SerializeTuple = Self;
		type SerializeTupleStruct = Self;
		type SerializeTupleVariant = Self;
		type SerializeMap = Self;
		type SerializeStruct = Self;
		type SerializeStructVariant = Self;

		fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
			self.write_bool(v)
		}

		fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
			self.write_i8(v)
		}

		fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
			self.write_i16(v)
		}

		fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
			self.write_i32(v)
		}

		fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
			self.write_i64(v)
		}

		fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
			self.write_i128(v)
		}

		fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
			self.write_u8(v)
		}

		fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
			self.write_u16(v)
		}

		fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
			self.write_u32(v)
		}

		fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
			self.write_u64(v)
		}

		fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
			self.write_u128(v)
		}

		fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
			self.write_f32(v)
		}

		fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
			self.write_f64(v)
		}

		fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
			self.write_char(v)
		}

		fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
			v.encode(self)
		}

		fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
			self.write_raw_bytes(v)
		}

		fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
			self.write_bool(false)
		}

		fn serialize_some<G: ?Sized>(self, value: &G) -> Result<Self::Ok, Self::Error> where G: Serialize {
			self.write_bool(true)?;
			value.serialize(self)
		}

		fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}

		fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}

		fn serialize_unit_variant(self, _name: &'static str, variant_index: u32, _variant: &'static str) -> Result<Self::Ok, Self::Error> {
			self.write_uvariant(variant_index as u128)
		}

		fn serialize_newtype_struct<G: ?Sized>(self, _name: &'static str, value: &G) -> Result<Self::Ok, Self::Error> where G: Serialize {
			value.serialize(self)
		}

		fn serialize_newtype_variant<G: ?Sized>(self, _name: &'static str, variant_index: u32, _variant: &'static str, value: &G) -> Result<Self::Ok, Self::Error> where G: Serialize {
			self.write_uvariant(variant_index as u128)?;
			value.serialize(self)
		}

		fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
			let len = len.ok_or(EncodingError::SerdeError("Length must be known upfront".to_string()))?;
			self.write_usize(len)?;
			Ok(self)
		}

		fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
			Ok(self)
		}

		fn serialize_tuple_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeTupleStruct, Self::Error> {
			Ok(self)
		}

		fn serialize_tuple_variant(self, _name: &'static str, variant_index: u32, _variant: &'static str, _len: usize) -> Result<Self::SerializeTupleVariant, Self::Error> {
			self.write_uvariant(variant_index as u128)?;
			Ok(self)
		}

		fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
			let len = len.ok_or(EncodingError::SerdeError("Length must be known upfront".to_string()))?;
			self.write_usize(len)?;
			Ok(self)
		}

		fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct, Self::Error> {
			Ok(self)
		}

		fn serialize_struct_variant(self, _name: &'static str, variant_index: u32, _variant: &'static str, _len: usize) -> Result<Self::SerializeStructVariant, Self::Error> {
			self.write_uvariant(variant_index as u128)?;
			Ok(self)
		}

		fn is_human_readable(&self) -> bool {
			false
		}
	}

	impl<T: Write> SerializeSeq for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;

		fn serialize_element<G: ?Sized>(&mut self, value: &G) -> Result<(), Self::Error> where G: Serialize {
			value.serialize(&mut **self)
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	}

	impl<T: Write> SerializeTuple for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;

		fn serialize_element<G: ?Sized>(&mut self, value: &G) -> Result<(), Self::Error> where G: Serialize {
			value.serialize(&mut **self)
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	}

	impl<T: Write> SerializeTupleStruct for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;

		fn serialize_field<G: ?Sized>(&mut self, value: &G) -> Result<(), Self::Error> where G: Serialize {
			value.serialize(&mut **self)
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	}

	impl<T: Write> SerializeTupleVariant for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;

		fn serialize_field<G: ?Sized>(&mut self, value: &G) -> Result<(), Self::Error> where G: Serialize {
			value.serialize(&mut **self)
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	}

	impl<T: Write> SerializeMap for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;

		fn serialize_key<G: ?Sized>(&mut self, key: &G) -> Result<(), Self::Error> where G: Serialize {
			key.serialize(&mut **self)
		}

		fn serialize_value<G: ?Sized>(&mut self, value: &G) -> Result<(), Self::Error> where G: Serialize {
			value.serialize(&mut **self)
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	}

	impl<T: Write> SerializeStruct for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;

		fn serialize_field<G: ?Sized>(&mut self, _key: &'static str, value: &G) -> Result<(), Self::Error> where G: Serialize {
			value.serialize(&mut **self)
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	}

	impl<T: Write> SerializeStructVariant for &mut BinStream<T> {
		type Ok = ();
		type Error = EncodingError;

		fn serialize_field<G: ?Sized>(&mut self, _key: &'static str, value: &G) -> Result<(), Self::Error> where G: Serialize {
			value.serialize(&mut **self)
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	}

	impl<'de, T: Read> Deserializer<'de> for &mut BinStream<T> {
		type Error = EncodingError;

		fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			Err(EncodingError::SerdeError("deserialize_any: This data format is non-describing".to_string()))
		}

		fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_bool(self.read_bool()?)
		}

		fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_i8(self.read_i8()?)
		}

		fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_i16(self.read_i16()?)
		}

		fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_i32(self.read_i32()?)
		}

		fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_i64(self.read_i64()?)
		}

		fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_i128(self.read_i128()?)
		}

		fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_u8(self.read_u8()?)
		}

		fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_u16(self.read_u16()?)
		}

		fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_u32(self.read_u32()?)
		}

		fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_u64(self.read_u64()?)
		}

		fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_u128(self.read_u128()?)
		}

		fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_f32(self.read_f32()?)
		}

		fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_f64(self.read_f64()?)
		}

		fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_char(self.read_char()?)
		}

		fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_str(&String::decode(self)?)
		}

		fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_string(String::decode(self)?)
		}

		fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_bytes(Vec::decode(self)?.as_slice())
		}

		fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_byte_buf(Vec::decode(self)?)
		}

		fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			if self.read_bool()? {
				visitor.visit_some(self)
			} else {
				visitor.visit_none()
			}
		}

		fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_unit()
		}

		fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_unit()
		}

		fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_newtype_struct(self)
		}

		fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			let len = self.read_usize()?;
			visitor.visit_seq(BinSeqDeserializer {
				stream: self,
				len,
			})
		}

		fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_seq(BinSeqDeserializer {
				stream: self,
				len,
			})
		}

		fn deserialize_tuple_struct<V>(self, _name: &'static str, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_seq(BinSeqDeserializer {
				stream: self,
				len,
			})
		}

		fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			let len = self.read_usize()?;
			visitor.visit_map(BinMapDeserializer {
				stream: self,
				len
			})
		}

		fn deserialize_struct<V>(self, _name: &'static str, fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_seq(BinSeqDeserializer {
				stream: self,
				len: fields.len(),
			})
		}

		fn deserialize_enum<V>(self, _name: &'static str, _variants: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_enum(self)
		}

		fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			visitor.visit_u32(self.read_uvariant()? as u32)
		}

		fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			Err(EncodingError::SerdeError("deserialize_ignored_any: This data format is non-describing".to_string()))
		}

		fn is_human_readable(&self) -> bool {
			false
		}
	}

	struct BinSeqDeserializer<'a, T: Read> {
		stream: &'a mut BinStream<T>,
		len: usize
	}

	impl<'de, 'a, T: Read> SeqAccess<'de> for BinSeqDeserializer<'a, T> {
		type Error = EncodingError;

		fn next_element_seed<G>(&mut self, seed: G) -> Result<Option<G::Value>, Self::Error> where G: DeserializeSeed<'de> {
			if self.len != 0 {
				seed.deserialize(&mut *self.stream).map(Some)
			} else {
				Ok(None)
			}
		}

		fn size_hint(&self) -> Option<usize> {
			Some(self.len)
		}
	}

	struct BinMapDeserializer<'a, T: Read> {
		stream: &'a mut BinStream<T>,
		len: usize
	}

	impl<'de, 'a, T: Read> MapAccess<'de> for BinMapDeserializer<'a, T> {
		type Error = EncodingError;

		fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error> where K: DeserializeSeed<'de> {
			if self.len != 0 {
				self.len -= 1;
				seed.deserialize(&mut *self.stream).map(Some)
			} else {
				Ok(None)
			}
		}

		fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error> where V: DeserializeSeed<'de> {
			seed.deserialize(&mut *self.stream)
		}

		fn size_hint(&self) -> Option<usize> {
			Some(self.len)
		}
	}

	impl<'de, T: Read> EnumAccess<'de> for &mut BinStream<T> {
		type Error = EncodingError;
		type Variant = Self;

		fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error> where V: DeserializeSeed<'de> {
			let val = seed.deserialize(&mut *self)?;
			Ok((val, self))
		}
	}

	impl<'de, T: Read> VariantAccess<'de> for &mut BinStream<T> {
		type Error = EncodingError;

		fn unit_variant(self) -> Result<(), Self::Error> {
			Ok(())
		}

		fn newtype_variant_seed<G>(self, seed: G) -> Result<G::Value, Self::Error> where G: DeserializeSeed<'de> {
			seed.deserialize(self)
		}

		fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			self.deserialize_tuple(len, visitor)
		}

		fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where V: Visitor<'de> {
			self.deserialize_struct("", fields, visitor)
		}
	}
}