use std::io;
use std::io::{BufReader, Read, Write};
use crate::compression::{Compression, CompressionError};
use crate::{EncodingResult, Finish};

impl Compression {
	/// Wraps a type implementing [`std::io::Write`] in a [`Compress`] using `self` as the parameters.
	pub fn compress<T: Write>(&self, input: T) -> Result<Compress<T>, CompressionError> {
		match self {
			Compression::None => {
				Ok(Compress(CompressInner::None(input)))
			}
			Compression::ZStd(level) => {
				Ok(Compress(CompressInner::ZStd(zstd::stream::write::Encoder::new(input, *level as _)?)))
			}
			Compression::ZLib(level) => {
				Ok(Compress(CompressInner::ZLib(flate2::write::ZlibEncoder::new(input, flate2::Compression::new(*level as _)))))
			}
			Compression::Deflate(level) => {
				Ok(Compress(CompressInner::Deflate(flate2::write::DeflateEncoder::new(input, flate2::Compression::new(*level as _)))))
			}
			Compression::GZip(level) => {
				Ok(Compress(CompressInner::GZip(flate2::write::GzEncoder::new(input, flate2::Compression::new(*level as _)))))
			}
		}
	}

	/// Wraps a type implementing [`std::io::Read`] in a [`Decompress`] using `self` as the parameters.
	pub fn decompress<T: Read>(&self, input: T) -> Result<Decompress<T>, CompressionError> {
		match self {
			Compression::None => {
				Ok(Decompress(DecompressInner::None(input)))
			}
			Compression::ZStd(..) => {
				Ok(Decompress(DecompressInner::ZStd(zstd::stream::read::Decoder::new(input)?)))
			}
			Compression::ZLib(..) => {
				Ok(Decompress(DecompressInner::ZLib(flate2::read::ZlibDecoder::new(input))))
			}
			Compression::Deflate(..) => {
				Ok(Decompress(DecompressInner::Deflate(flate2::read::DeflateDecoder::new(input))))
			}
			Compression::GZip(..) => {
				Ok(Decompress(DecompressInner::GZip(flate2::read::GzDecoder::new(input))))
			}
		}
	}
}

enum CompressInner<T: Write> {
	None(T),
	ZStd(zstd::stream::write::Encoder<'static, T>),
	ZLib(flate2::write::ZlibEncoder<T>),
	Deflate(flate2::write::DeflateEncoder<T>),
	GZip(flate2::write::GzEncoder<T>),
}

impl<T: Write> Finish for CompressInner<T> {
	type Output = T;
	#[inline]
	fn finish(self) -> EncodingResult<Self::Output> {
		match self {
			CompressInner::None(x) => Ok(x),
			CompressInner::ZStd(x) => Ok(x.finish()?),
			CompressInner::ZLib(x) => Ok(x.finish()?),
			CompressInner::Deflate(x) => Ok(x.finish()?),
			CompressInner::GZip(x) => Ok(x.finish()?),
		}
	}
}

impl<T: Write> Write for CompressInner<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		match self {
			CompressInner::None(x) => x.write(buf),
			CompressInner::ZStd(x) => x.write(buf),
			CompressInner::ZLib(x) => x.write(buf),
			CompressInner::Deflate(x) => x.write(buf),
			CompressInner::GZip(x) => x.write(buf),
		}
	}
	#[inline]
	fn flush(&mut self) -> io::Result<()> {
		match self {
			CompressInner::None(x) => x.flush(),
			CompressInner::ZStd(x) => x.flush(),
			CompressInner::ZLib(x) => x.flush(),
			CompressInner::Deflate(x) => x.flush(),
			CompressInner::GZip(x) => x.flush(),
		}
	}
}

/// A writer that compresses the data written to it before
/// forwarding it to the underlying stream.<br>
/// This value can be constructed by calling [`Compression::compress`]
/// with a type implementing [`std::io::Write`]
#[repr(transparent)]
pub struct Compress<T: Write>(CompressInner<T>);

impl<T: Write> Finish for Compress<T> {
	type Output = T;

	/// Flushes all the data yet to be compressed and potentially pads it to the nearest full
	/// block before returning the inner stream
	#[inline]
	fn finish(self) -> EncodingResult<Self::Output> {
		self.0.finish()
	}
}

impl<T: Write> Write for Compress<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		self.0.write(buf)
	}
	#[inline]
	fn flush(&mut self) -> io::Result<()> {
		self.0.flush()
	}
}

enum DecompressInner<T: Read> {
	None(T),
	ZStd(zstd::stream::read::Decoder<'static, BufReader<T>>),
	ZLib(flate2::read::ZlibDecoder<T>),
	Deflate(flate2::read::DeflateDecoder<T>),
	GZip(flate2::read::GzDecoder<T>),
}

impl<T: Read> Finish for DecompressInner<T> {
	type Output = T;
	#[inline]
	fn finish(self) -> EncodingResult<Self::Output> {
		match self {
			DecompressInner::None(x) => Ok(x),
			DecompressInner::ZStd(x) => Ok(x.finish().into_inner()),
			DecompressInner::ZLib(x) => Ok(x.into_inner()),
			DecompressInner::Deflate(x) => Ok(x.into_inner()),
			DecompressInner::GZip(x) => Ok(x.into_inner()),
		}
	}
}

impl<T: Read> Read for DecompressInner<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		match self {
			DecompressInner::None(x) => x.read(buf),
			DecompressInner::ZStd(x) => x.read(buf),
			DecompressInner::ZLib(x) => x.read(buf),
			DecompressInner::Deflate(x) => x.read(buf),
			DecompressInner::GZip(x) => x.read(buf),
		}
	}
}

/// A reader that decompresses the data read from the underlying
/// stream before returning it.<br>
/// This value can be constructed by calling [`Compression::decompress`]
/// with a type implementing [`std::io::Read`]
#[repr(transparent)]
pub struct Decompress<T: Read>(DecompressInner<T>);

impl<T: Read> Finish for Decompress<T> {
	type Output = T;

	/// Potentially reads the remaining bytes needed for padding up to a
	/// full block, then returns the inner stream
	#[inline]
	fn finish(self) -> EncodingResult<Self::Output> {
		self.0.finish()
	}
}

impl<T: Read> Read for Decompress<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		self.0.read(buf)
	}
}