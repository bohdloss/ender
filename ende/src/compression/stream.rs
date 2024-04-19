use embedded_io::{Error, ErrorType};

use crate::{Read, Write};
use crate::compression::{Compression, CompressionError};

fn io_err<T: Error>(value: T) -> CompressionError<T> {
	CompressionError::IOError(value)
}

#[cfg(feature = "std")]
fn initialize_err<T: Error>(value: std::io::Error) -> CompressionError<T> {
	if let std::io::ErrorKind::UnexpectedEof = value.kind() {
		CompressionError::UnexpectedEOF
	} else {
		CompressionError::Initialize
	}
}

#[cfg(feature = "std")]
fn internal_err<T: Error>(value: std::io::Error) -> CompressionError<T> {
	if let std::io::ErrorKind::UnexpectedEof = value.kind() {
		CompressionError::UnexpectedEOF
	} else {
		CompressionError::Internal
	}
}

#[cfg(feature = "std")]
fn finalize_err<T: Error>(value: std::io::Error) -> CompressionError<T> {
	if let std::io::ErrorKind::UnexpectedEof = value.kind() {
		CompressionError::UnexpectedEOF
	} else {
		CompressionError::Finalize
	}
}

impl Compression {
	/// Wraps a type implementing [Write][`embedded_io::Write`] in a [`Compress`] using `self` as the parameters.
	pub fn compress<T: Write>(&self, input: T) -> Result<Compress<T>, CompressionError<T::Error>> {
		match self {
			Compression::None => {
				Ok(Compress(CompressInner::None(input)))
			}
			#[cfg(feature = "std")]
			Compression::ZStd(level) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let compressor = zstd::stream::write::Encoder::new(input, *level as _).map_err(initialize_err)?;
				Ok(Compress(CompressInner::ZStd(compressor)))
			}
			#[cfg(feature = "std")]
			Compression::ZLib(level) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let compressor = flate2::write::ZlibEncoder::new(input, flate2::Compression::new(*level as _));
				Ok(Compress(CompressInner::ZLib(compressor)))
			}
			#[cfg(feature = "std")]
			Compression::Deflate(level) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let compressor = flate2::write::DeflateEncoder::new(input, flate2::Compression::new(*level as _));
				Ok(Compress(CompressInner::Deflate(compressor)))
			}
			#[cfg(feature = "std")]
			Compression::GZip(level) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let compressor = flate2::write::GzEncoder::new(input, flate2::Compression::new(*level as _));
				Ok(Compress(CompressInner::GZip(compressor)))
			}
		}
	}

	/// Wraps a type implementing [Read][`embedded_io::Read`] in a [`Decompress`] using `self` as the parameters.
	pub fn decompress<T: Read>(&self, input: T) -> Result<Decompress<T>, CompressionError<T::Error>> {
		match self {
			Compression::None => {
				Ok(Decompress(DecompressInner::None(input)))
			}
			#[cfg(feature = "std")]
			Compression::ZStd(..) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let decompressor = zstd::stream::read::Decoder::new(input).map_err(initialize_err)?;
				Ok(Decompress(DecompressInner::ZStd(decompressor)))
			}
			#[cfg(feature = "std")]
			Compression::ZLib(..) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let decompressor = flate2::read::ZlibDecoder::new(input);
				Ok(Decompress(DecompressInner::ZLib(decompressor)))
			}
			#[cfg(feature = "std")]
			Compression::Deflate(..) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let decompressor = flate2::read::DeflateDecoder::new(input);
				Ok(Decompress(DecompressInner::Deflate(decompressor)))
			}
			#[cfg(feature = "std")]
			Compression::GZip(..) => {
				let input = embedded_io_adapters::std::ToStd::new(input);
				let decompressor = flate2::read::GzDecoder::new(input);
				Ok(Decompress(DecompressInner::GZip(decompressor)))
			}
		}
	}
}

enum CompressInner<T: Write> {
	None(T),
	#[cfg(feature = "std")]
	ZStd(zstd::stream::write::Encoder<'static, embedded_io_adapters::std::ToStd<T>>),
	#[cfg(feature = "std")]
	ZLib(flate2::write::ZlibEncoder<embedded_io_adapters::std::ToStd<T>>),
	#[cfg(feature = "std")]
	Deflate(flate2::write::DeflateEncoder<embedded_io_adapters::std::ToStd<T>>),
	#[cfg(feature = "std")]
	GZip(flate2::write::GzEncoder<embedded_io_adapters::std::ToStd<T>>),
}

impl<T: Write> CompressInner<T> {
	#[inline]
	pub fn finish(self) -> Result<T, CompressionError<T::Error>> {
		match self {
			CompressInner::None(x) => Ok(x),
			#[cfg(feature = "std")]
			CompressInner::ZStd(x) => Ok(x.finish().map_err(finalize_err)?.into_inner()),
			#[cfg(feature = "std")]
			CompressInner::ZLib(x) => Ok(x.finish().map_err(finalize_err)?.into_inner()),
			#[cfg(feature = "std")]
			CompressInner::Deflate(x) => Ok(x.finish().map_err(finalize_err)?.into_inner()),
			#[cfg(feature = "std")]
			CompressInner::GZip(x) => Ok(x.finish().map_err(finalize_err)?.into_inner()),
		}
	}
}

impl<T: Write> ErrorType for CompressInner<T> {
	type Error = CompressionError<T::Error>;
}

impl<T: Write> Write for CompressInner<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
		match self {
			CompressInner::None(x) => x.write(buf).map_err(io_err),
			#[cfg(feature = "std")]
			CompressInner::ZStd(x) => std::io::Write::write(x, buf).map_err(internal_err),
			#[cfg(feature = "std")]
			CompressInner::ZLib(x) => std::io::Write::write(x, buf).map_err(internal_err),
			#[cfg(feature = "std")]
			CompressInner::Deflate(x) => std::io::Write::write(x, buf).map_err(internal_err),
			#[cfg(feature = "std")]
			CompressInner::GZip(x) => std::io::Write::write(x, buf).map_err(internal_err),
		}
	}
	#[inline]
	fn flush(&mut self) -> Result<(), Self::Error> {
		match self {
			CompressInner::None(x) => x.flush().map_err(io_err),
			#[cfg(feature = "std")]
			CompressInner::ZStd(x) => std::io::Write::flush(x).map_err(internal_err),
			#[cfg(feature = "std")]
			CompressInner::ZLib(x) => std::io::Write::flush(x).map_err(internal_err),
			#[cfg(feature = "std")]
			CompressInner::Deflate(x) => std::io::Write::flush(x).map_err(internal_err),
			#[cfg(feature = "std")]
			CompressInner::GZip(x) => std::io::Write::flush(x).map_err(internal_err),
		}
	}
}

/// A writer that compresses the data written to it before
/// forwarding it to the underlying stream.<br>
/// This value can be constructed by calling [`Compression::compress`]
/// with a type implementing [`std::io::Write`]
#[repr(transparent)]
pub struct Compress<T: Write>(CompressInner<T>);

impl<T: Write> Compress<T> {
	/// Flushes all the data yet to be compressed and potentially pads it to the nearest full
	/// block before returning the inner stream
	#[inline]
	pub fn finish(self) -> Result<T, CompressionError<T::Error>> {
		self.0.finish()
	}
}

impl<T: Write> ErrorType for Compress<T> {
	type Error = CompressionError<T::Error>;
}

impl<T: Write> Write for Compress<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
		self.0.write(buf)
	}
	#[inline]
	fn flush(&mut self) -> Result<(), Self::Error> {
		self.0.flush()
	}
}

enum DecompressInner<T: Read> {
	None(T),
	#[cfg(feature = "std")]
	ZStd(zstd::stream::read::Decoder<'static, std::io::BufReader<embedded_io_adapters::std::ToStd<T>>>),
	#[cfg(feature = "std")]
	ZLib(flate2::read::ZlibDecoder<embedded_io_adapters::std::ToStd<T>>),
	#[cfg(feature = "std")]
	Deflate(flate2::read::DeflateDecoder<embedded_io_adapters::std::ToStd<T>>),
	#[cfg(feature = "std")]
	GZip(flate2::read::GzDecoder<embedded_io_adapters::std::ToStd<T>>),
}

impl<T: Read> DecompressInner<T> {
	#[inline]
	fn finish(self) -> Result<T, CompressionError<T::Error>> {
		match self {
			DecompressInner::None(x) => Ok(x),
			#[cfg(feature = "std")]
			DecompressInner::ZStd(x) => Ok(x.finish().into_inner().into_inner()),
			#[cfg(feature = "std")]
			DecompressInner::ZLib(x) => Ok(x.into_inner().into_inner()),
			#[cfg(feature = "std")]
			DecompressInner::Deflate(x) => Ok(x.into_inner().into_inner()),
			#[cfg(feature = "std")]
			DecompressInner::GZip(x) => Ok(x.into_inner().into_inner()),
		}
	}
}

impl<T: Read> ErrorType for DecompressInner<T> {
	type Error = CompressionError<T::Error>;
}

impl<T: Read> Read for DecompressInner<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
		match self {
			DecompressInner::None(x) => x.read(buf).map_err(io_err),
			#[cfg(feature = "std")]
			DecompressInner::ZStd(x) => std::io::Read::read(x, buf).map_err(internal_err),
			#[cfg(feature = "std")]
			DecompressInner::ZLib(x) => std::io::Read::read(x, buf).map_err(internal_err),
			#[cfg(feature = "std")]
			DecompressInner::Deflate(x) => std::io::Read::read(x, buf).map_err(internal_err),
			#[cfg(feature = "std")]
			DecompressInner::GZip(x) => std::io::Read::read(x, buf).map_err(internal_err),
		}
	}
}

/// A reader that decompresses the data read from the underlying
/// stream before returning it.<br>
/// This value can be constructed by calling [`Compression::decompress`]
/// with a type implementing [`std::io::Read`]
#[repr(transparent)]
pub struct Decompress<T: Read>(DecompressInner<T>);

impl<T: Read> Decompress<T> {
	/// Potentially reads the remaining bytes needed for padding up to a
	/// full block, then returns the inner stream
	#[inline]
	pub fn finish(self) -> Result<T, CompressionError<T::Error>> {
		self.0.finish()
	}
}

impl<T: Read> ErrorType for Decompress<T> {
	type Error = CompressionError<T::Error>;
}

impl<T: Read> Read for Decompress<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
		self.0.read(buf)
	}
}