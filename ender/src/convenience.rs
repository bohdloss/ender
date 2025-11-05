use crate::{Context, Decode, Encode, Encoder, EncodingResult};
use crate::io::{Read, Slice, SliceMut, Write};

/// Encodes the given value by constructing an encoder on the fly backed by a
/// [VecStream][`crate::io::VecStream`], then returning the wrapped vector of bytes.
/// 
/// Uses the default [`Context`]
#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
#[inline]
pub fn encode_bytes<V: Encode<crate::io::VecStream>>(value: V) -> EncodingResult<alloc::vec::Vec<u8>> {
	let stream = crate::io::VecStream::new(alloc::vec::Vec::new(), 0);
	let mut encoder = Encoder::new(stream, Context::default());
	value.encode(&mut encoder)?;
	Ok(encoder.stream.into_inner())
}

/// Decodes the given value by constructing an encoder on the fly and using it to wrap a byte
/// slice.
/// 
/// Uses the default [`Context`]
#[inline]
pub fn decode_bytes<'a, R: AsRef<[u8]>, V: Decode<'a, Slice<'a>>>(bytes: &'a R) -> EncodingResult<V> {
	let mut decoder = Encoder::new(Slice::new(bytes.as_ref()), Context::default());
	V::decode(&mut decoder)
}

/// Encodes the given value by constructing an encoder on the fly backed by a
/// [VecStream][`crate::io::VecStream`], then returning the wrapped vector of bytes
/// 
/// Uses the given [`Context`]
#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
#[inline]
pub fn encode_bytes_with<V: Encode<crate::io::VecStream>>(value: V, context: Context) -> EncodingResult<alloc::vec::Vec<u8>> {
	let stream = crate::io::VecStream::new(alloc::vec::Vec::new(), 0);
	let mut encoder = Encoder::new(stream, context);
	value.encode(&mut encoder)?;
	Ok(encoder.stream.into_inner())
}

/// Decodes the given value by constructing an encoder on the fly and using it to wrap a byte
/// slice.
/// 
/// Uses the given [`Context`]
#[inline]
pub fn decode_bytes_with<'a, R: AsRef<[u8]>, V: Decode<'a, Slice<'a>>>(bytes: &'a R, context: Context) -> EncodingResult<V> {
	let mut decoder = Encoder::new(Slice::new(bytes.as_ref()), context);
	V::decode(&mut decoder)
}

/// Encodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the default context.
#[inline]
pub fn encode<W: IntoWrite, V: Encode<W::Write>>(
	writer: W,
	value: V,
) -> EncodingResult<()> {
	let mut encoder = Encoder::new(writer.into_write(), Context::default());
	value.encode(&mut encoder)
}

/// Decodes the given value by constructing an encoder on the fly and using it to wrap the reader,
/// with the default context.
#[inline]
pub fn decode<'de, R: IntoRead<'de>, V: Decode<'de, R::Read>>(reader: R) -> EncodingResult<V> {
	let mut decoder = Encoder::new(reader.into_read(), Context::default());
	V::decode(&mut decoder)
}

/// Encodes the given value by constructing an encoder on the fly and using it to wrap the writer,
/// with the given context.
#[inline]
pub fn encode_with<W: IntoWrite, V: Encode<W::Write>>(
	writer: W,
	context: Context,
	value: V,
) -> EncodingResult<()> {
	let mut encoder = Encoder::new(writer.into_write(), context);
	value.encode(&mut encoder)
}

/// Decodes the given value by constructing an encoder on the fly and using it to wrap the reader,
/// with the given context.
#[inline]
pub fn decode_with<'de, R: IntoRead<'de>, V: Decode<'de, R::Read>>(reader: R, context: Context) -> EncodingResult<V> {
	let mut decoder = Encoder::new(reader.into_read(), context);
	V::decode(&mut decoder)
}

/// Something that can be turned into a writer compatible with [Encoder][`crate::Encoder`]
///
/// This is automatically implemented for some standard types, like byte slices and vectors of bytes.
pub trait IntoWrite {
	type Write: Write;
	/// Transforms `self` into a type implementing [Write][`crate::io::Write`]
	fn into_write(self) -> Self::Write;
}

impl<T: Write> IntoWrite for T {
	type Write = T;
	#[inline(always)]
	fn into_write(self) -> Self::Write {
		self
	}
}

impl<'a> IntoWrite for &'a mut [u8] {
	type Write = SliceMut<'a>;
	#[inline]
	fn into_write(self) -> Self::Write {
		SliceMut::new(self)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl IntoWrite for alloc::vec::Vec<u8> {
	type Write = crate::io::VecStream;
	#[inline]
	fn into_write(self) -> Self::Write {
		crate::io::VecStream::new(self, 0)
	}
}

/// Something that can be turned into a reader compatible with [Encoder][`crate::Encoder`]
///
/// This is automatically implemented for some standard types, like byte slices and vectors of bytes.
pub trait IntoRead<'de> {
	type Read: Read<'de>;
	/// Transforms `self` into a type implementing [Read][`crate::io::Read`]
	fn into_read(self) -> Self::Read;
}

impl<'de, T: Read<'de>> IntoRead<'de> for T {
	type Read = T;
	#[inline(always)]
	fn into_read(self) -> Self::Read {
		self
	}
}

impl<'de> IntoRead<'de> for &'de [u8] {
	type Read = Slice<'de>;
	#[inline]
	fn into_read(self) -> Self::Read {
		Slice::new(self)
	}
}

impl<'de> IntoRead<'de> for &'de mut [u8] {
	type Read = Slice<'de>;
	#[inline]
	fn into_read(self) -> Self::Read {
		Slice::new(self)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl IntoRead<'_> for alloc::vec::Vec<u8> {
	type Read = crate::io::VecStream;
	#[inline]
	fn into_read(self) -> Self::Read {
		crate::io::VecStream::new(self, 0)
	}
}