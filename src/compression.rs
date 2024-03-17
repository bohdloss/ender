use std::io;
use std::io::{BufReader, Read, Write};
use ende_derive::{Decode, Encode};
use parse_display::Display;
use thiserror::Error;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
pub enum Compression {
	#[display("no compression")]
	None,
	#[display("level {0} ZStandard compression")]
	ZStd(ZStdLevel)
}

impl Compression {
	pub fn is_none(&self) -> bool {
		match self {
			Compression::None => true,
			_ => false
		}
	}

	pub fn is_zstd(&self) -> bool {
		match self {
			Compression::ZStd(..) => true,
			_ => false
		}
	}
}

impl Compression {
	pub fn compress<T: Write>(&self, input: T) -> Result<Compress<T>, CompressionError> {
		match self {
			Compression::None => {
				Ok(Compress(CompressInner::None(input)))
			}
			Compression::ZStd(level) => {
				Ok(Compress(CompressInner::ZStd(zstd::stream::write::Encoder::new(input, *level as _)?)))
			}
		}
	}

	pub fn decompress<T: Read>(&self, input: T) -> Result<Decompress<T>, CompressionError> {
		match self {
			Compression::None => {
				Ok(Decompress(DecompressInner::None(input)))
			}
			Compression::ZStd(..) => {
				Ok(Decompress(DecompressInner::ZStd(zstd::stream::read::Decoder::new(input)?)))
			}
		}
	}
}

enum CompressInner<'a, T: Write> {
	None(T),
	ZStd(zstd::stream::write::Encoder<'a, T>)
}

impl<T: Write> CompressInner<'_, T> {
	#[inline]
	fn finish(self) -> Result<T, CompressionError> {
		match self {
			CompressInner::None(x) => Ok(x),
			CompressInner::ZStd(x) => Ok(x.finish()?),
		}
	}
}

impl<T: Write> Write for CompressInner<'_, T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		match self {
			CompressInner::None(x) => x.write(buf),
			CompressInner::ZStd(x) => x.write(buf),
		}
	}
	#[inline]
	fn flush(&mut self) -> io::Result<()> {
		match self {
			CompressInner::None(x) => x.flush(),
			CompressInner::ZStd(x) => x.flush(),
		}
	}
}

#[repr(transparent)]
pub struct Compress<'a, T: Write>(CompressInner<'a, T>);

impl<T: Write> Compress<'_, T> {
	#[inline]
	pub fn finish(self) -> Result<T, CompressionError> {
		self.0.finish()
	}
}

impl<T: Write> Write for Compress<'_, T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		self.0.write(buf)
	}
	#[inline]
	fn flush(&mut self) -> io::Result<()> {
		self.0.flush()
	}
}

enum DecompressInner<'a, T: Read> {
	None(T),
	ZStd(zstd::stream::read::Decoder<'a, BufReader<T>>)
}

impl<T: Read> DecompressInner<'_ ,T> {
	#[inline]
	fn finish(self) -> Result<T, CompressionError> {
		match self {
			DecompressInner::None(x) => Ok(x),
			DecompressInner::ZStd(x) => Ok(x.finish().into_inner()),
		}
	}
}

impl<T: Read> Read for DecompressInner<'_, T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		match self {
			DecompressInner::None(x) => x.read(buf),
			DecompressInner::ZStd(x) => x.read(buf),
		}
	}
}

#[repr(transparent)]
pub struct Decompress<'a, T: Read>(DecompressInner<'a, T>);

impl<T: Read> Decompress<'_, T> {
	#[inline]
	pub fn finish(self) -> Result<T, CompressionError> {
		self.0.finish()
	}
}

impl<T: Read> Read for Decompress<'_ ,T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		self.0.read(buf)
	}
}

#[derive(Debug, Error)]
pub enum CompressionError {
	#[error("IO Error occurred: {0}")]
	IOError(
		#[source]
		#[from]
		io::Error
	)
}