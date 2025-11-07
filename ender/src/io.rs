//! An abstraction over the underlying IO implementation, removing
//! unneeded elements and functions, while improving the interoperability
//! with the library.<br>
//! Custom [`Write`], [`Read`], [`BorrowRead`] traits are provided, as well
//! as a compatibility layer with `std::io` (see [`Std`])

use crate::{EncodingError, EncodingResult, SeekError};

#[allow(unused)]
fn usize_to_u64(val: usize) -> u64 {
    // PANIC SAFETY
    // `as` conversion never fails, unless we are on a >64-bit system I guess?
    val as _
}

#[allow(unused)]
fn isize_to_i64(val: isize) -> i64 {
    // PANIC SAFETY
    // Same as above
    val as _
}

#[allow(unused)]
fn u64_to_usize(val: u64) -> usize {
    assert!(val <= isize::MAX as _);

    val as _
}

#[allow(unused)]
fn i64_to_isize(val: i64) -> isize {
    assert!(val <= isize::MAX as _);

    val as _
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
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
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Std<T>(T);

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
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
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
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
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<'de, T: Read<'de>> std::io::Read for Std<T> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        <T as Read>::read(&mut self.0, buf).map_err(io_err)?;
        Ok(buf.len())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<T: Seek> std::io::Seek for Std<T> {
    #[inline]
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        let pos = match pos {
            std::io::SeekFrom::Start(off) => SeekFrom::Start(u64_to_usize(off)),
            std::io::SeekFrom::End(off) => SeekFrom::End(i64_to_isize(off)),
            std::io::SeekFrom::Current(off) => SeekFrom::Current(i64_to_isize(off)),
        };
        match <T as Seek>::seek(&mut self.0, pos) {
            Ok(off) => Ok(usize_to_u64(off)),
            Err(err) => Err(io_err(err)),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<T: std::io::Write> Write for Std<T> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        <T as std::io::Write>::write_all(&mut self.0, buf).map_err(Into::into)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<'de, T: std::io::Read> Read<'de> for Std<T> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        <T as std::io::Read>::read_exact(&mut self.0, buf).map_err(Into::into)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<T: std::io::Seek> Seek for Std<T> {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<usize> {
        // PANIC SAFETY
        // `as` conversion never fails, unless we are on a >64-bit system I guess?
        let seek = match seek {
            SeekFrom::Start(off) => std::io::SeekFrom::Start(usize_to_u64(off)),
            SeekFrom::End(off) => std::io::SeekFrom::End(isize_to_i64(off)),
            SeekFrom::Current(off) => std::io::SeekFrom::Current(isize_to_i64(off)),
        };
        match <T as std::io::Seek>::seek(&mut self.0, seek) {
            Ok(off) => Ok(u64_to_usize(off)),
            Err(x) => Err(x.into()),
        }
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
        Self { slice, pos: 0 }
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

impl<'de> Read<'de> for SliceMut<'_> {
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
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<usize> {
        let offset = seek.as_buf_offset(self.pos, self.slice.len())?;
        self.pos = offset;
        Ok(offset)
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
        Self { slice, pos: 0 }
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

impl<'de> Read<'de> for Slice<'_> {
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
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<usize> {
        let offset = seek.as_buf_offset(self.pos, self.slice.len())?;
        self.pos = offset;
        Ok(offset)
    }
}

/// Wraps a `Vec` providing [`Write`], [`Read`] and [`Seek`] implementations.
///
/// The advantage of this over a [`SliceMut`] is that when the end of the
/// vector's capacity is reached, the backing memory is simply extended and writing can continue.
#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct VecStream {
    vec: alloc::vec::Vec<u8>,
    pos: usize,
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl VecStream {
    /// Wraps a `Vec` in a type implementing "infinite" read, write and seek.
    ///
    /// The `start` parameter is used to determine the initial value of the stream pointer.
    #[inline]
    pub fn new(vec: alloc::vec::Vec<u8>, start: usize) -> Self {
        let mut this = Self { vec, pos: start };
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
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl Write for VecStream {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        self.ensure_capacity(self.pos + buf.len());
        let sub = &mut self.vec[self.pos..(self.pos + buf.len())];
        sub.copy_from_slice(buf);
        self.pos += buf.len();
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl Read<'_> for VecStream {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        self.ensure_capacity(self.pos + buf.len());
        let sub = &self.vec[self.pos..(self.pos + buf.len())];
        buf.copy_from_slice(sub);
        self.pos += buf.len();
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl Seek for VecStream {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<usize> {
        if let SeekFrom::End(_) = seek {
            return Err(SeekError::UnknownRange.into());
        }

        let offset = seek.as_buf_offset(self.pos, 0)?;
        self.ensure_capacity(offset);
        self.pos = offset;
        Ok(offset)
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

impl<'de, T: Read<'de>> Read<'de> for SizeTrack<T> {
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

impl<'de, T: Read<'de>> Read<'de> for SizeLimit<T> {
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

/// A NOP stream, that ignores write and seek calls, and responds to read calls
/// with infinite zeroes.
#[derive(Clone)]
pub struct Zero;

impl Write for Zero {
    #[inline]
    fn write(&mut self, _buf: &[u8]) -> EncodingResult<()> {
        Ok(())
    }
}

impl<'de> Read<'de> for Zero {
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
    fn seek(&mut self, _seek: SeekFrom) -> EncodingResult<usize> {
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
pub trait Read<'de> {
    /// Reads `buf.len()` bytes into `buf`.
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()>;
}

pub trait ReadOwned: for<'de> Read<'de> {}
impl<R> ReadOwned for R where R: for<'de> Read<'de> {}

/// A buffer that is capable of lending data, in order to perform **zero copy decoding**.
pub trait BorrowRead<'de>: Read<'de> {
    /// Exactly the same as [`borrow_read`][`Self::borrow_read`], except the stream position
    /// is not advanced. E.G. multiple subsequent calls to this function are guaranteed to produce
    /// the same output.
    fn peek(&self, len: usize) -> EncodingResult<&'de [u8]>;

    /// Borrows a slice of bytes from the buffer, incrementing the buffer position.
    /// The slice's lifetime is bound to the buffer's lifetime.
    fn borrow_read(&mut self, len: usize) -> EncodingResult<&'de [u8]>;
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
    Start(usize),
    /// Offsets `n` bytes from the end of the stream or buffer.
    ///
    /// If `n` is before the beginning of the stream, or if it is beyond the end of the stream,
    /// a [`SeekError`][`crate::SeekError`] is returned.
    End(isize),
    /// Offsets `n` bytes from the current position in the stream or buffer.
    ///
    /// If `n` is before the beginning of the stream, or if it is beyond the end of the stream,
    /// a [`SeekError`][`crate::SeekError`] is returned.
    Current(isize),
}

impl SeekFrom {
    /// A [`SeekFrom`] such that when passed to [`Seek::seek`],
    /// the position is left unchanged and the return value of the call
    /// will contain the current position.
    pub const POSITION: Self = Self::Current(0);

    /// Calculates the new stream position in the case a seek operation is applied
    /// to a buffer, given the current position and the length of the buffer.
    ///
    /// Returns the offset, or a [`SeekError`] if the resulting offset would
    /// be outside of bounds.
    #[inline]
    pub const fn as_buf_offset(&self, pos: usize, len: usize) -> Result<usize, SeekError> {
        assert!(pos <= isize::MAX as _);
        assert!(len <= isize::MAX as _);

        let offset = match *self {
            SeekFrom::Start(off) => off as isize,
            SeekFrom::End(off) => len as isize + off,
            SeekFrom::Current(off) => pos as isize + off,
        };

        if offset > len as isize {
            Err(SeekError::AfterEnd(offset as usize))
        } else if offset < 0 {
            Err(SeekError::BeforeBeginning(offset))
        } else {
            Ok(offset as usize)
        }
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
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<usize>;
}

impl<T: Write> Write for &mut T {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> EncodingResult<()> {
        <T as Write>::write(self, buf)
    }
}

impl<'de, T: Read<'de>> Read<'de> for &mut T {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> EncodingResult<()> {
        <T as Read>::read(self, buf)
    }
}

impl<T: Seek> Seek for &mut T {
    #[inline]
    fn seek(&mut self, seek: SeekFrom) -> EncodingResult<usize> {
        <T as Seek>::seek(self, seek)
    }
}

impl<'de, T: BorrowRead<'de>> BorrowRead<'de> for &mut T {
    #[inline]
    fn peek(&self, len: usize) -> EncodingResult<&'de [u8]> {
        <T as BorrowRead<'de>>::peek(self, len)
    }
    #[inline]
    fn borrow_read(&mut self, len: usize) -> EncodingResult<&'de [u8]> {
        <T as BorrowRead<'de>>::borrow_read(self, len)
    }
}
