//! An abstraction over the underlying IO implementation, removing
//! unneeded elements and functions, while improving the interoperability
//! with the library.<br>
//! Custom [`Write`], [`Read`], [`BorrowRead`] traits are provided, as well
//! as a compatibility layer with `std::io` (see [`Std`])

use core::mem::take;
use crate::{EncodingError, EncodingResult};

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct Std<T>(T);

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T> Std<T> {
	#[inline]
	pub fn new(stream: T) -> Self {
		Self(stream)
	}
	#[inline]
	pub fn inner(&self) -> &T {
		&self.0
	}
	#[inline]
	pub fn inner_mut(&mut self) -> &mut T {
		&mut self.0
	}
	#[inline]
	pub fn into_inner(self) -> T {
		self.0
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: std::io::Write> Write for Std<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
		<T as std::io::Write>::write_all(&mut self.0, buf).map_err(|x| x.into())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: std::io::Read> Read for Std<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		<T as std::io::Read>::read_exact(&mut self.0, buf).map_err(|x| x.into())
	}
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct Embedded<T>(T);

impl<T> Embedded<T> {
	#[inline]
	pub fn new(stream: T) -> Self {
		Self(stream)
	}
	#[inline]
	pub fn inner(&self) -> &T {
		&self.0
	}
	#[inline]
	pub fn inner_mut(&mut self) -> &mut T {
		&mut self.0
	}
	#[inline]
	pub fn into_inner(self) -> T {
		self.0
	}
}

impl<T: embedded_io::Write> Write for Embedded<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
		use embedded_io::Error;
		<T as embedded_io::Write>::write_all(&mut self.0, buf).map_err(|x| x.kind().into())
	}
}

impl<T: embedded_io::Read> Read for Embedded<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		<T as embedded_io::Read>::read_exact(&mut self.0, buf).map_err(|x| x.into())
	}
}

impl Write for &mut [u8] {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
		if buf.len() > self.len() {
			return Err(EncodingError::UnexpectedEnd);
		}
		
		let (first, second) = take(self).split_at_mut(buf.len());
		first.copy_from_slice(buf);
		*self = second;
		Ok(())
	}
}

impl Read for &mut [u8] {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		if buf.len() > self.len() {
			return Err(EncodingError::UnexpectedEnd);
		}

		let (first, second) = take(self).split_at_mut(buf.len());
		buf.copy_from_slice(first);
		*self = second;
		Ok(())
	}
}

impl Read for &[u8] {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		if buf.len() > self.len() {
			return Err(EncodingError::UnexpectedEnd);
		}
		
		let (first, second) = take(self).split_at(buf.len());
		buf.copy_from_slice(first);
		*self = second;
		Ok(())
	}
}

impl<'data> BorrowRead<'data> for &'data [u8] {
	#[inline]
	fn peek(&self, len: usize) -> EncodingResult<&'data [u8]> {
		if len > self.len() {
			return Err(EncodingError::UnexpectedEnd);
		}
		
		Ok(&self[..len])
	}
	#[inline]
	fn borrow_read(&mut self, len: usize) -> EncodingResult<&'data [u8]> {
		if len > self.len() {
			return Err(EncodingError::UnexpectedEnd);
		}

		let (first, second) = take(self).split_at(len);
		*self = second;
		Ok(first)
	}
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct SizeWrite(usize);

impl SizeWrite {
	#[inline]
	pub fn new() -> Self {
		Self(0)
	}
	#[inline]
	pub fn size(&self) -> usize {
		self.0
	}
}

impl Write for SizeWrite {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
		self.0 += buf.len();
		Ok(())
	}
}

#[derive(Clone, Debug)]
pub struct SizeTrack<T>{
	stream: T,
	wsize: usize,
	rsize: usize,
}

impl<T> SizeTrack<T> {
	#[inline]
	pub fn new(stream: T) -> Self {
		Self {
			stream,
			wsize: 0,
			rsize: 0,
		}
	}
	#[inline]
	pub fn size_written(&self) -> usize {
		self.wsize
	}

	#[inline]
	pub fn size_read(&self) -> usize {
		self.rsize
	}

	#[inline]
	pub fn inner(&self) -> &T {
		&self.stream
	}

	#[inline]
	pub fn inner_mut(&mut self) -> &mut T {
		&mut self.stream
	}

	#[inline]
	pub fn into_inner(self) -> T {
		self.stream
	}
}

impl<T: Write> Write for SizeTrack<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
		let ok = self.stream.write(buf)?;
		self.wsize += buf.len();
		Ok(ok)
	}
}

impl<T: Read> Read for SizeTrack<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		let ok = self.stream.read(buf)?;
		self.rsize += buf.len();
		Ok(ok)
	}
}

impl<'data, T: BorrowRead<'data>> BorrowRead<'data> for SizeTrack<T> {
	#[inline]
	fn peek(&self, len: usize) -> EncodingResult<&'data [u8]> {
		self.stream.peek(len)
	}
	#[inline]
	fn borrow_read(&mut self, len: usize) -> EncodingResult<&'data [u8]> {
		let ok = self.stream.borrow_read(len)?;
		self.rsize += len;
		Ok(ok)
	}
}

#[derive(Clone, Debug)]
pub struct SizeLimit<T>{
	stream: T,
	wsize: usize,
	rsize: usize,
}

impl<T> SizeLimit<T> {
	#[inline]
	pub fn new(stream: T, write_limit: usize, read_limit: usize) -> Self {
		Self {
			stream,
			wsize: write_limit,
			rsize: read_limit,
		}
	}
	#[inline]
	pub fn remaining_writable(&self) -> usize {
		self.wsize
	}

	#[inline]
	pub fn remaining_readable(&self) -> usize {
		self.rsize
	}

	#[inline]
	pub fn inner(&self) -> &T {
		&self.stream
	}

	#[inline]
	pub fn inner_mut(&mut self) -> &mut T {
		&mut self.stream
	}

	#[inline]
	pub fn into_inner(self) -> T {
		self.stream
	}
}

impl<T: Write> Write for SizeLimit<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
		if buf.len() > self.wsize {
			return Err(EncodingError::UnexpectedEnd);
		}
		let ok = self.stream.write(buf)?;
		self.wsize -= buf.len();
		Ok(ok)
	}
}

impl<T: Read> Read for SizeLimit<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		if buf.len() > self.rsize {
			return Err(EncodingError::UnexpectedEnd);
		}
		let ok = self.stream.read(buf)?;
		self.rsize -= buf.len();
		Ok(ok)
	}
}

impl<'data, T: BorrowRead<'data>> BorrowRead<'data> for SizeLimit<T> {
	#[inline]
	fn peek(&self, len: usize) -> EncodingResult<&'data [u8]> {
		if len > self.rsize {
			return Err(EncodingError::UnexpectedEnd);
		}
		self.stream.peek(len)
	}
	#[inline]
	fn borrow_read(&mut self, len: usize) -> EncodingResult<&'data [u8]> {
		if len > self.rsize {
			return Err(EncodingError::UnexpectedEnd);
		}
		let ok = self.stream.borrow_read(len)?;
		self.rsize -= len;
		Ok(ok)
	}
}

impl Write for () {
	#[inline]
	fn write(&mut self, _buf: &[u8]) -> EncodingResult<()> {
		Ok(())
	}
}

impl Read for () {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		for x in buf {
			*x = 0;
		}
		Ok(())
	}
}

pub trait Write {
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()>;
}

pub trait Read {
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()>;
}

pub trait BorrowRead<'data>: Read {
	fn peek(&self, len: usize) -> EncodingResult<&'data [u8]>;
	fn borrow_read(&mut self, len: usize) -> EncodingResult<&'data [u8]>;
}

impl<T: Write> Write for &mut T {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
		<T as Write>::write(self, buf)
	}
}

impl<T: Read> Read for &mut T {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
		<T as Read>::read(self, buf)
	}
}

impl<'data, T: BorrowRead<'data>> BorrowRead<'data> for &mut T {
	#[inline]
	fn peek(&self, len: usize) -> EncodingResult<&'data [u8]> {
		<T as BorrowRead<'data>>::peek(self, len)
	}
	#[inline]
	fn borrow_read(&mut self, len: usize) -> EncodingResult<&'data [u8]> {
		<T as BorrowRead<'data>>::borrow_read(self, len)
	}
}