mod stream;

use std::io;
use std::io::{Read, Write};
use parse_display::Display;
use thiserror::Error;
use crate::{Decode, Encode, Encoder, EncodingError, EncodingResult, Finish};

pub use stream::*;

/// Function for convenience.<br>
/// It calls [`Encoder::add_compression`] on the encoder with the given compression parameter,
/// calls the closure with the transformed encoder, then finalizes the compressor before returning
pub fn encode_with_compression<T, F>(
	encoder: &mut Encoder<T>,
	compression: Option<Compression>,
	f: F
) -> EncodingResult<()>
	where T: Write,
	      F: FnOnce(&mut Encoder<Compress<&mut T>>) -> EncodingResult<()>
{
	let mut encoder = encoder.add_compression(compression)?;
	let v = f(&mut encoder);
	encoder.finish()?.0.finish()?;
	v
}

/// Function for convenience.<br>
/// It calls [`Encoder::add_decompression`] on the decoder with the given compression parameter,
/// calls the closure with the transformed decoder, then finalizes the decompressor before returning
pub fn decode_with_compression<T, F, V>(
	decoder: &mut Encoder<T>,
	compression: Option<Compression>,
	f: F
) -> EncodingResult<V>
	where T: Read,
	      F: FnOnce(&mut Encoder<Decompress<&mut T>>) -> EncodingResult<V>,
	      V: crate::Decode
{
	let mut decoder = decoder.add_decompression(compression)?;
	let v = f(&mut decoder);
	decoder.finish()?.0.finish()?;
	v
}

/// Contains compression parameters known at a higher level than
/// the encoding/decoding step. Consists of a [`Compression`] parameter.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Display)]
#[display("compression = ({compression})")]
pub struct CompressionState {
	/// The compression parameter. This will be used to infer the compression mode when
	/// it is not known.
	pub compression: Compression
}

impl CompressionState {
	/// Constructs a new compression state, with the compression parameter set to None
	pub const fn new() -> Self {
		Self {
			compression: Compression::None,
		}
	}
}

/// ZStandard compression level
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display)]
#[repr(u8)]
pub enum ZStdLevel {
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
	#[display("10")]
	L10 = 10,
	#[display("11")]
	L11 = 11,
	#[display("12")]
	L12 = 12,
	#[display("13")]
	L13 = 13,
	#[display("14")]
	L14 = 14,
	#[display("15")]
	L15 = 15,
	#[display("16")]
	L16 = 16,
	#[display("17")]
	L17 = 17,
	#[display("18")]
	L18 = 18,
	#[display("19")]
	L19 = 19,
	#[display("20")]
	L20 = 20,
	#[display("21")]
	L21 = 21,
	#[display("22")]
	L22 = 22,
}

impl Encode for ZStdLevel {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		encoder.write_u8(*self as u8)
	}
}

impl Decode for ZStdLevel {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			1 => ZStdLevel::L1,
			2 => ZStdLevel::L2,
			3 => ZStdLevel::L3,
			4 => ZStdLevel::L4,
			5 => ZStdLevel::L5,
			6 => ZStdLevel::L6,
			7 => ZStdLevel::L7,
			8 => ZStdLevel::L8,
			9 => ZStdLevel::L9,
			10 => ZStdLevel::L10,
			11 => ZStdLevel::L11,
			12 => ZStdLevel::L12,
			13 => ZStdLevel::L13,
			14 => ZStdLevel::L14,
			15 => ZStdLevel::L15,
			16 => ZStdLevel::L16,
			17 => ZStdLevel::L17,
			18 => ZStdLevel::L18,
			19 => ZStdLevel::L19,
			20 => ZStdLevel::L20,
			21 => ZStdLevel::L21,
			22 => ZStdLevel::L22,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

/// ZLib compression level
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display)]
#[repr(u8)]
pub enum ZLibLevel {
	#[display("0")]
	L0 = 0,
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
}

impl Encode for ZLibLevel {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		encoder.write_u8(*self as u8)
	}
}

impl Decode for ZLibLevel {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			0 => ZLibLevel::L0,
			1 => ZLibLevel::L1,
			2 => ZLibLevel::L2,
			3 => ZLibLevel::L3,
			4 => ZLibLevel::L4,
			5 => ZLibLevel::L5,
			6 => ZLibLevel::L6,
			7 => ZLibLevel::L7,
			8 => ZLibLevel::L8,
			9 => ZLibLevel::L9,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

/// Deflate compression level
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display)]
#[repr(u8)]
pub enum DeflateLevel {
	#[display("0")]
	L0 = 0,
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
}

impl Encode for DeflateLevel {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		encoder.write_u8(*self as u8)
	}
}

impl Decode for DeflateLevel {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			0 => DeflateLevel::L0,
			1 => DeflateLevel::L1,
			2 => DeflateLevel::L2,
			3 => DeflateLevel::L3,
			4 => DeflateLevel::L4,
			5 => DeflateLevel::L5,
			6 => DeflateLevel::L6,
			7 => DeflateLevel::L7,
			8 => DeflateLevel::L8,
			9 => DeflateLevel::L9,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

/// GZip compression level
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display)]
#[repr(u8)]
pub enum GZipLevel {
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
}

impl Encode for GZipLevel {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		encoder.write_u8(*self as u8)
	}
}

impl Decode for GZipLevel {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			1 => GZipLevel::L1,
			2 => GZipLevel::L2,
			3 => GZipLevel::L3,
			4 => GZipLevel::L4,
			5 => GZipLevel::L5,
			6 => GZipLevel::L6,
			7 => GZipLevel::L7,
			8 => GZipLevel::L8,
			9 => GZipLevel::L9,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

/// Compression algorithm and level, or None to indicate absence of compression.
/// Can be used to wrap a type implementing Write/Read in order to provide Compression/Decompression
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(u8)]
pub enum Compression {
	#[display("no compression")]
	None,
	#[display("level {0} ZStd compression")]
	ZStd(ZStdLevel),
	#[display("level {0} ZLib compression")]
	ZLib(ZLibLevel),
	#[display("level {0} Deflate compression")]
	Deflate(DeflateLevel),
	#[display("level {0} GZip compression")]
	GZip(GZipLevel),
}

impl Encode for Compression {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		match self {
			Compression::None => encoder.write_u8(0)?,
			Compression::ZStd(m0) => {
				encoder.write_u8(1)?;
				m0.encode(encoder)?;
			}
			Compression::ZLib(m0) => {
				encoder.write_u8(2)?;
				m0.encode(encoder)?;
			}
			Compression::Deflate(m0) => {
				encoder.write_u8(3)?;
				m0.encode(encoder)?;
			}
			Compression::GZip(m0) => {
				encoder.write_u8(4)?;
				m0.encode(encoder)?;
			}
		}
		Ok(())
	}
}

impl Decode for Compression {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			0 => Compression::None,
			1 => Compression::ZStd(
				Decode::decode(decoder)?
			),
			2 => Compression::ZLib(
				Decode::decode(decoder)?
			),
			3 => Compression::Deflate(
				Decode::decode(decoder)?
			),
			4 => Compression::GZip(
				Decode::decode(decoder)?
			),
			_ => return Err(EncodingError::InvalidVariant)
		})
	}
}

impl Compression {
	/// Returns true if the `self` is None
	pub fn is_none(&self) -> bool {
		match self {
			Compression::None => true,
			_ => false
		}
	}

	/// Returns true if the `self` is ZStd
	pub fn is_zstd(&self) -> bool {
		match self {
			Compression::ZStd(..) => true,
			_ => false
		}
	}

	/// Returns true if the `self` is ZLib
	pub fn is_zlib(&self) -> bool {
		match self {
			Compression::ZLib(..) => true,
			_ => false
		}
	}

	/// Returns true if the `self` is Deflate
	pub fn is_deflate(&self) -> bool {
		match self {
			Compression::Deflate(..) => true,
			_ => false
		}
	}

	/// Returns true if the `self` is GZip
	pub fn is_gzip(&self) -> bool {
		match self {
			Compression::GZip(..) => true,
			_ => false
		}
	}
}

/// A generic error for anything that might go wrong during Compression/Decompression<br>
/// FIXME This is still subject to change
#[derive(Debug, Error)]
pub enum CompressionError {
	/// Generic IO Error
	#[error("IO Error occurred: {0}")]
	IOError(
		#[source]
		#[from]
		io::Error
	)
}