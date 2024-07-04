use crate::io::{Read, Slice, SliceMut, VecStream, Write};

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
	type Write = VecStream;
	#[inline]
	fn into_write(self) -> Self::Write {
		VecStream::new(self, 0)
	}
}

/// Something that can be turned into a reader compatible with [Encoder][`crate::Encoder`]
///
/// This is automatically implemented for some standard types, like byte slices and vectors of bytes.
pub trait IntoRead {
	type Read: Read;
	/// Transforms `self` into a type implementing [Read][`crate::io::Read`]
	fn into_read(self) -> Self::Read;
}

impl<T: Read> IntoRead for T {
	type Read = T;
	#[inline(always)]
	fn into_read(self) -> Self::Read {
		self
	}
}

impl<'a> IntoRead for &'a [u8] {
	type Read = Slice<'a>;
	#[inline]
	fn into_read(self) -> Self::Read {
		Slice::new(self)
	}
}

impl<'a> IntoRead for &'a mut [u8] {
	type Read = Slice<'a>;
	#[inline]
	fn into_read(self) -> Self::Read {
		Slice::new(self)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl IntoRead for alloc::vec::Vec<u8> {
	type Read = VecStream;
	#[inline]
	fn into_read(self) -> Self::Read {
		VecStream::new(self, 0)
	}
}