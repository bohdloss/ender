//! An abstraction over the underlying IO implementation, removing
//! unneeded elements and functions, while improving the interoperability
//! with the library.<br>
//! Custom [`Write`], [`Read`], [`BorrowRead`] traits are provided, as well
//! as a compatibility layer with `std::io` (see [`Std`])

use crate::{EncodingError, EncodingResult};
use core::mem::take;

/// A compatibility layer between this crate's I/O traits and `std::io` traits.
/// 
/// If `T` implements [`std::io::Write`] or [`std::io::Read`] or both, `Std<T>` will implement
/// [`Write`] or [`Read`] or both, and vice versa.
/// 
/// The memory layout is always guaranteed to be that of `T`.
#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Std<T>(T);

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T> Std<T> {
    /// Wraps a `T`.
    #[inline]
    pub fn new(stream: T) -> Self {
        Self(stream)
    }
    /// Read-only reference to `T`.
    #[inline]
    pub fn inner(&self) -> &T {
        &self.0
    }
    /// Mutable reference to `T`.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.0
    }
    /// Unwraps `T` and returns it.
    #[inline]
    pub fn into_inner(self) -> T {
        self.0
    }
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Write> std::io::Write for Std<T> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        <T as Write>::write(&mut self.0, buf).map_err(|x| match x {
            EncodingError::IOError(error) => {
                std::io::Error::from(<std::io::ErrorKind as From<_>>::from(error))
            }
            EncodingError::UnexpectedEnd => std::io::Error::from(std::io::ErrorKind::UnexpectedEof),
            error => std::io::Error::new(std::io::ErrorKind::Other, error),
        })?;
        Ok(buf.len())
    }
    #[inline]
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Read> std::io::Read for Std<T> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        <T as Read>::read(&mut self.0, buf).map_err(|x| match x {
            EncodingError::IOError(error) => {
                std::io::Error::from(<std::io::ErrorKind as From<_>>::from(error))
            }
            EncodingError::UnexpectedEnd => std::io::Error::from(std::io::ErrorKind::UnexpectedEof),
            error => std::io::Error::new(std::io::ErrorKind::Other, error),
        })?;
        Ok(buf.len())
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

/// Wraps a mutable `u8` slice providing [`Write`] and [`Read`] implementations.
#[derive(Eq, PartialEq, Debug)]
pub struct SliceMut<'data>(&'data mut [u8]);

impl<'data> SliceMut<'data> {
    /// Wraps a mutable slice in an I/O object.
    #[inline]
    pub fn new(slice: &'data mut [u8]) -> Self {
        Self(slice)
    }
    /// Read-only reference to the slice.
    #[inline]
    pub fn inner(&self) -> &[u8] {
        &self.0
    }
    /// Mutable reference to the slice.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
    /// Unwraps the slice, returning it.
    #[inline]
    pub fn into_inner(self) -> &'data mut [u8] {
        self.0
    }
}

impl Write for SliceMut<'_> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        if buf.len() > self.0.len() {
            return Err(EncodingError::UnexpectedEnd);
        }

        let (first, second) = take(&mut self.0).split_at_mut(buf.len());
        first.copy_from_slice(buf);
        self.0 = second;
        Ok(())
    }
}

impl Read for SliceMut<'_> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        if buf.len() > self.0.len() {
            return Err(EncodingError::UnexpectedEnd);
        }

        let (first, second) = take(&mut self.0).split_at_mut(buf.len());
        buf.copy_from_slice(first);
        self.0 = second;
        Ok(())
    }
}

/// Wraps an immutable `u8` slice providing [`Read`] and [`BorrowRead`] implementations.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Slice<'data>(&'data [u8]);

impl<'data> Slice<'data> {
    /// Wraps an immutable slice in an I/O object.
    #[inline]
    pub fn new(slice: &'data [u8]) -> Self {
        Self(slice)
    }
    /// Reference to the slice.
    #[inline]
    pub fn inner(&self) -> &[u8] {
        &self.0
    }
    /// Unwraps the slice, returning it.
    #[inline]
    pub fn into_inner(self) -> &'data [u8] {
        self.0
    }
}

impl Read for Slice<'_> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        if buf.len() > self.0.len() {
            return Err(EncodingError::UnexpectedEnd);
        }

        let (first, second) = take(&mut self.0).split_at(buf.len());
        buf.copy_from_slice(first);
        self.0 = second;
        Ok(())
    }
}

impl<'data> BorrowRead<'data> for Slice<'data> {
    #[inline]
    fn peek(&self, len: usize) -> EncodingResult<&'data [u8]> {
        if len > self.0.len() {
            return Err(EncodingError::UnexpectedEnd);
        }

        Ok(&self.0[..len])
    }
    #[inline]
    fn borrow_read(&mut self, len: usize) -> EncodingResult<&'data [u8]> {
        if len > self.0.len() {
            return Err(EncodingError::UnexpectedEnd);
        }

        let (first, second) = take(&mut self.0).split_at(len);
        self.0 = second;
        Ok(first)
    }
}

/// Wraps a `Vec` providing [`Write`] and [`Read`] implementations.
/// 
/// The advantage of this over a [`SliceMut`] is that when the end of the
/// vector is reached, the backing memory is simply extended and writing can continue.
#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct VecWrite(alloc::vec::Vec<u8>);

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl VecWrite {
    #[inline]
    pub fn new(vec: Vec<u8>) -> Self {
        Self(vec)
    }
    #[inline]
    pub fn inner(&self) -> &Vec<u8> {
        &self.0
    }
    #[inline]
    pub fn inner_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
    #[inline]
    pub fn into_inner(self) -> Vec<u8> {
        self.0
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Write for VecWrite {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        self.0.reserve(buf.len());
        self.0.extend_from_slice(buf);
        Ok(())
    }
}

/// Wraps any type that implements [`Write`] or [`Read`] and keeps track of how many
/// bytes are written and read, separately.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SizeTrack<T> {
    stream: T,
    wsize: usize,
    rsize: usize,
}

impl<T> SizeTrack<T> {
    /// Creates a new tracker for `T`.
    #[inline]
    pub fn new(stream: T) -> Self {
        Self {
            stream,
            wsize: 0,
            rsize: 0,
        }
    }
    
    /// Returns the number of bytes written so far.
    /// 
    /// Note that if a write call fails, the value returned by
    /// this function is left unchanged.
    #[inline]
    pub fn size_written(&self) -> usize {
        self.wsize
    }

    /// Returns the number of bytes read so far.
    ///
    /// Note that if a read call fails, the value returned by
    /// this function is left unchanged.
    #[inline]
    pub fn size_read(&self) -> usize {
        self.rsize
    }

    /// Read-only reference to `T`.
    #[inline]
    pub fn inner(&self) -> &T {
        &self.stream
    }

    /// Mutable reference to `T`.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.stream
    }

    /// Unwraps `T`, returning it.
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

/// Wraps any type that implements [`Write`] or [`Read`] and limits how many
/// bytes can be written and read, separately.
#[derive(Clone, Debug)]
pub struct SizeLimit<T> {
    stream: T,
    wsize: usize,
    rsize: usize,
}

impl<T> SizeLimit<T> {
    /// Creates a new limiter for `T`.
    #[inline]
    pub fn new(stream: T, write_limit: usize, read_limit: usize) -> Self {
        Self {
            stream,
            wsize: write_limit,
            rsize: read_limit,
        }
    }

    /// Returns the number of bytes that can still be written.
    ///
    /// Note that if a write call fails, the value returned by
    /// this function is left unchanged.
    #[inline]
    pub fn remaining_writable(&self) -> usize {
        self.wsize
    }

    /// Returns the number of bytes that can still be read.
    ///
    /// Note that if a read call fails, the value returned by
    /// this function is left unchanged.
    #[inline]
    pub fn remaining_readable(&self) -> usize {
        self.rsize
    }

    /// Read-only reference to `T`.
    #[inline]
    pub fn inner(&self) -> &T {
        &self.stream
    }

    /// Mutable reference to `T`.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.stream
    }

    /// Unwraps `T`, returning it.
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

/// A NOP stream, that ignores write calls, and responds to read calls with infinite zeroes.
#[derive(Clone)]
pub struct Zero;

impl Write for Zero {
    #[inline]
    fn write(&mut self, _buf: &[u8]) -> EncodingResult<()> {
        Ok(())
    }
}

impl Read for Zero {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        for x in buf {
            *x = 0;
        }
        Ok(())
    }
}

/// Everything, be it a stream or buffer, which can be **encoded into**.
pub trait Write {
    /// Writes the entire contents of `buf`.
    /// 
    /// No guarantees are made about flushing.
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()>;
}

/// Everything, be it a stream or buffer, which can be **decoded from**.
pub trait Read {
    /// Reads `buf.len()` bytes into `buf`.
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()>;
}

/// A buffer that is capable of lending data, in order to perform **zero copy decoding**.
pub trait BorrowRead<'data>: Read {
    /// Exactly the same as [`borrow_read`][`Self::borrow_read`], except the stream position
    /// is not advanced. E.G. multiple subsequent calls to this function are guaranteed to produce
    /// the same output.
    fn peek(&self, len: usize) -> EncodingResult<&'data [u8]>;
    
    /// Borrows a slice of bytes from the buffer, incrementing the buffer position.
    /// The slice's lifetime is bound to the buffer's lifetime.
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
