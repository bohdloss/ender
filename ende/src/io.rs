//! An abstraction over the underlying IO implementation, removing
//! unneeded elements and functions, while improving the interoperability
//! with the library.<br>
//! Custom [`Write`], [`Read`], [`BorrowRead`] traits are provided, as well
//! as a compatibility layer with `std::io` (see [`Std`])

use crate::{EncodingError, EncodingResult, SeekError};

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
fn io_err(err: EncodingError) -> std::io::Error {
    match err {
        EncodingError::IOError(error) => {
            std::io::Error::from(<std::io::ErrorKind as From<_>>::from(error))
        }
        EncodingError::UnexpectedEnd => std::io::Error::from(std::io::ErrorKind::UnexpectedEof),
        error => std::io::Error::new(std::io::ErrorKind::Other, error),
    }
}

/// A compatibility layer between this crate's I/O traits and `std::io` traits.
///
/// If `T` implements either [`std::io::Write`], [`std::io::Read`], [`std::io::Seek`]
/// or any combination of those, `Std<T>` will implement either [`Write`], [`Read`], [`Seek`]
/// or any combination of those, and vice versa.
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
        <T as Write>::write(&mut self.0, buf).map_err(io_err)?;
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
        <T as Read>::read(&mut self.0, buf).map_err(io_err)?;
        Ok(buf.len())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Seek> std::io::Seek for Std<T> {
    #[inline]
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        let pos = match pos {
            std::io::SeekFrom::Start(off) => SeekFrom::Start(off),
            std::io::SeekFrom::End(off) => SeekFrom::End(off),
            std::io::SeekFrom::Current(off) => SeekFrom::Current(off),
        };
        <T as Seek>::seek(&mut self.0, pos).map_err(io_err)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: std::io::Write> Write for Std<T> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        <T as std::io::Write>::write_all(&mut self.0, buf).map_err(Into::into)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: std::io::Read> Read for Std<T> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        <T as std::io::Read>::read_exact(&mut self.0, buf).map_err(Into::into)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: std::io::Seek> Seek for Std<T> {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<u64> {
        let seek = match seek {
            SeekFrom::Start(off) => std::io::SeekFrom::Start(off),
            SeekFrom::End(off) => std::io::SeekFrom::End(off),
            SeekFrom::Current(off) => std::io::SeekFrom::Current(off),
        };
        <T as std::io::Seek>::seek(&mut self.0, seek).map_err(Into::into)
    }
}

/// Wraps a mutable `u8` slice providing [`Write`], [`Read`] and [`Seek`] implementations.
#[derive(Eq, PartialEq, Debug)]
pub struct SliceMut<'data> {
    slice: &'data mut [u8],
    pos: usize,
}

impl<'data> SliceMut<'data> {
    /// Wraps a mutable slice in an I/O object.
    #[inline]
    pub fn new(slice: &'data mut [u8]) -> Self {
        Self {
            slice,
            pos: 0,
        }
    }
    /// Read-only reference to the slice.
    #[inline]
    pub fn inner(&self) -> &[u8] {
        &self.slice
    }
    /// Mutable reference to the slice.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut [u8] {
        &mut self.slice
    }
    /// Unwraps the slice, returning it.
    #[inline]
    pub fn into_inner(self) -> &'data mut [u8] {
        self.slice
    }
}

impl Write for SliceMut<'_> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        let rem = self.slice.len() - self.pos;
        if buf.len() > rem {
            return Err(EncodingError::UnexpectedEnd);
        }
        let sub = &mut self.slice[self.pos..(self.pos + buf.len())];
        sub.copy_from_slice(buf);
        self.pos += buf.len();
        Ok(())
    }
}

impl Read for SliceMut<'_> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        let rem = self.slice.len() - self.pos;
        if buf.len() > rem {
            return Err(EncodingError::UnexpectedEnd);
        }
        let sub = &self.slice[self.pos..(self.pos + buf.len())];
        buf.copy_from_slice(sub);
        self.pos += buf.len();
        Ok(())
    }
}

impl Seek for SliceMut<'_> {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<u64> {
        // `as` conversion never fails, unless we are on a 128-bit system I guess?
        let offset = seek.as_buf_offset(self.pos as _, self.slice.len() as _)?;
        self.pos = offset;
        Ok(offset as u64)
    }
}

/// Wraps an immutable `u8` slice providing [`Read`], [`BorrowRead`] and [`Seek`] implementations.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Slice<'data> {
    slice: &'data [u8],
    pos: usize,
}

impl<'data> Slice<'data> {
    /// Wraps an immutable slice in an I/O object.
    #[inline]
    pub fn new(slice: &'data [u8]) -> Self {
        Self {
            slice,
            pos: 0,
        }
    }
    /// Reference to the slice.
    #[inline]
    pub fn inner(&self) -> &[u8] {
        &self.slice
    }
    /// Unwraps the slice, returning it.
    #[inline]
    pub fn into_inner(self) -> &'data [u8] {
        self.slice
    }
}

impl Read for Slice<'_> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        let rem = self.slice.len() - self.pos;
        if buf.len() > rem {
            return Err(EncodingError::UnexpectedEnd);
        }
        let sub = &self.slice[self.pos..(self.pos + buf.len())];
        buf.copy_from_slice(sub);
        self.pos += buf.len();
        Ok(())
    }
}

impl<'data> BorrowRead<'data> for Slice<'data> {
    #[inline]
    fn peek(&self, len: usize) -> EncodingResult<&'data [u8]> {
        let rem = self.slice.len() - self.pos;
        if len > rem {
            return Err(EncodingError::UnexpectedEnd);
        }
        let sub = &self.slice[self.pos..(self.pos + len)];
        Ok(sub)
    }
    #[inline]
    fn borrow_read(&mut self, len: usize) -> EncodingResult<&'data [u8]> {
        let rem = self.slice.len() - self.pos;
        if len > rem {
            return Err(EncodingError::UnexpectedEnd);
        }
        let sub = &self.slice[self.pos..(self.pos + len)];
        self.pos += len;
        Ok(sub)
    }
}

impl Seek for Slice<'_> {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<u64> {
        let offset = seek.as_buf_offset(self.pos as _, self.slice.len() as _)?;
        self.pos = offset;
        Ok(offset as u64)
    }
}

/// Wraps a `Vec` providing [`Write`], [`Read`] and [`Seek`] implementations.
///
/// The advantage of this over a [`SliceMut`] is that when the end of the
/// vector's capacity is reached, the backing memory is simply extended and writing can continue.
#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct VecStream {
    vec: alloc::vec::Vec<u8>,
    pos: usize,
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl VecStream {
    /// Wraps a `Vec` in a type implementing "infinite" read, write and seek.
    /// 
    /// The `start` parameter is used to determine the initial value of the stream pointer.
    #[inline]
    pub fn new(vec: alloc::vec::Vec<u8>, start: usize) -> Self {
        let mut this = Self {
            vec,
            pos: start,
        };
        this.ensure_capacity(this.pos);
        this
    }
    /// Read-only window into the contents of the vector written so far.
    #[inline]
    pub fn inner(&self) -> &[u8] {
        &self.vec[..self.pos]
    }
    /// Returns the vector, truncated to the length of the stream pointer at the moment
    /// of calling this function.
    #[inline]
    pub fn into_inner(mut self) -> alloc::vec::Vec<u8> {
        self.vec.truncate(self.pos);
        self.vec
    }

    // Ensure the capacity is at least `at_least`, if it isn't
    // then reserve the missing number of elements to reach that capacity,
    // then pad with zeros
    fn ensure_capacity(&mut self, at_least: usize) {
        if at_least > self.vec.capacity() {
            self.vec.reserve(at_least - self.vec.len());
            for _ in self.vec.len()..self.vec.capacity() {
                self.vec.push(0);
            }
        }
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Write for VecStream {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        self.ensure_capacity(self.pos + buf.len());
        let sub = &mut self.vec[self.pos..(self.pos + buf.len())];
        sub.copy_from_slice(buf);
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Read for VecStream {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        self.ensure_capacity(self.pos + buf.len());
        let sub = &self.vec[self.pos..(self.pos + buf.len())];
        buf.copy_from_slice(sub);
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Seek for VecStream {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<u64> {
        if let SeekFrom::End(_) = seek {
            return Err(SeekError::UnknownRange.into());
        }
        
        let offset = seek.as_buf_offset(self.pos, 0)?;
        self.ensure_capacity(offset);
        self.pos = offset;
        Ok(offset as u64)
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

/// A NOP stream, that ignores write and seek calls,and responds to read calls
/// with infinite zeroes.
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

impl Seek for Zero {
    #[inline]
    fn seek(&mut self, _seek: SeekFrom) -> EncodingResult<u64> {
        Ok(0)
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

/// The argument to a call to [`Seek::seek`].
///
/// Supports seeking relative to the current position, the beginning or the end
/// of a stream or buffer.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SeekFrom {
    /// Offsets `n` bytes from the beginning of the stream or buffer.
    ///
    /// If `n` is beyond the end, a [`SeekError`][`crate::SeekError`]
    /// is returned.
    Start(u64),
    /// Offsets `n` bytes from the end of the stream or buffer.
    ///
    /// If `n` is before the beginning of the stream, or if it is beyond the end of the stream,
    /// a [`SeekError`][`crate::SeekError`] is returned.
    End(i64),
    /// Offsets `n` bytes from the current position in the stream or buffer.
    ///
    /// If `n` is before the beginning of the stream, or if it is beyond the end of the stream,
    /// a [`SeekError`][`crate::SeekError`] is returned.
    Current(i64),
}

impl SeekFrom {
    /// Calculates the new stream position in the case a seek operation is applied
    /// to a buffer, given the current position and the length of the buffer.
    ///
    /// Returns the offset, or a [`SeekError`] if the resulting offset would
    /// be outside of bounds.
    pub const fn as_buf_offset(&self, pos: usize, len: usize) -> Result<usize, SeekError> {
        let offset = match *self {
            SeekFrom::Start(off) => {
                off as i64
            }
            SeekFrom::End(off) => {
                len as i64 + off
            }
            SeekFrom::Current(off) => {
                pos as i64 + off
            }
        };

        if offset > len as i64 {
            Err(SeekError::AfterEnd(offset as u64))
        } else if offset < 0 {
            Err(SeekError::BeforeBeginning(offset))
        } else {
            Ok(offset as usize)
        }
    }
    
    /// Returns a [`SeekFrom`] such that when passed to [`Seek::seek`],
    /// the position is left unchanged and the return value of the call
    /// will contain the current position.
    /// 
    /// This function is not magic: it is literally defined as `SeekFrom::Current(0)`
    pub const fn get_pos() -> Self {
        Self::Current(0)
    }
}

/// Any stream or buffer that supports moving the stream position forwards and backwards,
/// as well as returning the current position.
pub trait Seek {
    /// Offsets the stream position using the given [`SeekFrom`] argument.
    ///
    /// In general, a seek beyond the end or to a negative offset
    /// will yield a [`SeekError`][`crate::SeekError`].
    ///
    /// Returns the new position, as an offset from the start of the stream.
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<u64>;
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

impl<T: Seek> Seek for &mut T {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<u64> {
        <T as Seek>::seek(self, seek)
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
