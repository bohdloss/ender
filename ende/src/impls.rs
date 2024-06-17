use core::cell::{Cell, RefCell};
use core::ffi::CStr;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ops::{Bound, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo};
use core::time::Duration;

use crate::io::{BorrowRead, Read, Seek, Write};
use crate::{
    BorrowError, Decode, Encode, Encoder, EncodingError, EncodingResult, NumEncoding,
    StrEncoding, StringError,
};

// Primitives

macro_rules! impl_encode {
    ($($ty:ty => $write:ident);* $(;)? ) => {
	    $(
	    impl<W: $crate::io::Write> $crate::Encode<W> for $ty {
		    #[inline]
            fn encode(&self, encoder: &mut $crate::Encoder<W>) -> $crate::EncodingResult<()> {
		        encoder.$write(*self)
		    }
	    }
	    )*
    };
}

impl_encode! {
    u8 => write_u8;
    u16 => write_u16;
    u32 => write_u32;
    u64 => write_u64;
    u128 => write_u128;
    i8 => write_i8;
    i16 => write_i16;
    i32 => write_i32;
    i64 => write_i64;
    i128 => write_i128;
    bool => write_bool;
    char => write_char;
    f32 => write_f32;
    f64 => write_f64;
    usize => write_usize;
    isize => write_isize;
}

impl<W: Write + Seek> Encode<W> for () {
    #[inline]
    fn encode(&self, _encoder: &mut Encoder<W>) -> EncodingResult<()> {
        Ok(())
    }
}

impl<W: Write, T: ?Sized + Encode<W>> Encode<W> for &T {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        <T as Encode<W>>::encode(self, encoder)
    }
}

impl<W: Write, T: ?Sized + Encode<W>> Encode<W> for &mut T {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        <T as Encode<W>>::encode(self, encoder)
    }
}

impl<W: Write, T: Encode<W>> Encode<W> for [T] {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for i in 0..self.len() {
            encoder.with_index(|encoder| self[i].encode(encoder), i)?;
        }
        Ok(())
    }
}

impl<W: Write, T: Encode<W>, const SIZE: usize> Encode<W> for [T; SIZE] {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        for i in 0..SIZE {
            encoder.with_index(|encoder| self[i].encode(encoder), i)?;
        }
        Ok(())
    }
}

macro_rules! tuple_encode {
    ($($name:ident)+) => {
	    #[allow(non_snake_case)]
	    impl<__W: $crate::io::Write, $($name: $crate::Encode<__W>),+> $crate::Encode<__W> for ($($name),+) {
		    #[inline]
            fn encode(&self, encoder: &mut $crate::Encoder<__W>) -> $crate::EncodingResult<()> {
		        let ($($name),*) = self;
			    $(
			        $crate::Encode::encode($name, encoder)?;
			    )+
			    Ok(())
		    }
	    }
    };
}

tuple_encode! { A B }
tuple_encode! { A B C }
tuple_encode! { A B C D }
tuple_encode! { A B C D E }
tuple_encode! { A B C D E F }
tuple_encode! { A B C D E F G }
tuple_encode! { A B C D E F G H }
tuple_encode! { A B C D E F G H I }
tuple_encode! { A B C D E F G H I J }
tuple_encode! { A B C D E F G H I J K }
tuple_encode! { A B C D E F G H I J K L }
tuple_encode! { A B C D E F G H I J K L M }
tuple_encode! { A B C D E F G H I J K L M N }
tuple_encode! { A B C D E F G H I J K L M N O }
tuple_encode! { A B C D E F G H I J K L M N O P } // Up to 16

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write> Encode<W> for alloc::string::String {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_str(self.chars())
    }
}

impl<W: Write> Encode<W> for str {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_str(self.chars())
    }
}

impl<W: Write, T: Encode<W>> Encode<W> for Option<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self {
            None => encoder.write_bool(false),
            Some(value) => {
                encoder.write_bool(true)?;
                value.encode(encoder)
            }
        }
    }
}

impl<W: Write, T: Encode<W>, E: Encode<W>> Encode<W> for Result<T, E> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self {
            Err(err) => {
                encoder.write_bool(false)?;
                err.encode(encoder)?;
            }
            Ok(ok) => {
                encoder.write_bool(true)?;
                ok.encode(encoder)?;
            }
        };
        Ok(())
    }
}

impl<W: Write, T> Encode<W> for PhantomData<T> {
    #[inline]
    fn encode(&self, _encoder: &mut Encoder<W>) -> EncodingResult<()> {
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, T: Encode<W>> Encode<W> for alloc::boxed::Box<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        <T as Encode<W>>::encode(self.deref(), encoder)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, T: Encode<W>> Encode<W> for alloc::rc::Rc<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        <T as Encode<W>>::encode(self.deref(), encoder)
    }
}

#[cfg(all(feature = "alloc"))]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, T: Encode<W>> Encode<W> for alloc::sync::Arc<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        <T as Encode<W>>::encode(self.deref(), encoder)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<'a, W: Write, T: ?Sized + alloc::borrow::ToOwned> Encode<W> for alloc::borrow::Cow<'a, T>
where
    T: Encode<W>,
    <T as alloc::borrow::ToOwned>::Owned: Encode<W>,
{
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self {
            Self::Borrowed(x) => x.encode(encoder),
            Self::Owned(x) => x.encode(encoder),
        }
    }
}

impl<W: Write, T: Encode<W> + Copy> Encode<W> for Cell<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.get().encode(encoder)
    }
}

impl<W: Write, T: Encode<W>> Encode<W> for RefCell<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self.try_borrow() {
            Ok(ok) => <T as Encode<W>>::encode(ok.deref(), encoder),
            Err(_) => Err(EncodingError::LockError),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write, T: Encode<W>> Encode<W> for std::sync::Mutex<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self.lock() {
            Ok(ok) => <T as Encode<W>>::encode(ok.deref(), encoder),
            Err(_) => Err(EncodingError::LockError),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write, T: Encode<W>> Encode<W> for std::sync::RwLock<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self.read() {
            Ok(ok) => <T as Encode<W>>::encode(ok.deref(), encoder),
            Err(_) => Err(EncodingError::LockError),
        }
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, K: Encode<W>, V: Encode<W>> Encode<W> for alloc::collections::BTreeMap<K, V> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for (k, v) in self.iter() {
            k.encode(encoder)?;
            v.encode(encoder)?;
        }
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, K: Encode<W>> Encode<W> for alloc::collections::BTreeSet<K> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for k in self.iter() {
            k.encode(encoder)?;
        }
        Ok(())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write, K: Encode<W>, V: Encode<W>> Encode<W> for std::collections::hash_map::HashMap<K, V> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for (k, v) in self.iter() {
            k.encode(encoder)?;
            v.encode(encoder)?;
        }
        Ok(())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write, K: Encode<W>> Encode<W> for std::collections::hash_set::HashSet<K> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for k in self.iter() {
            k.encode(encoder)?;
        }
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, T: Encode<W>> Encode<W> for alloc::collections::BinaryHeap<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for v in self.iter() {
            v.encode(encoder)?;
        }
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, T: Encode<W>> Encode<W> for alloc::collections::LinkedList<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for (i, v) in self.iter().enumerate() {
            encoder.with_index(|encoder| v.encode(encoder), i)?;
        }
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, T: Encode<W>> Encode<W> for alloc::vec::Vec<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for (i, v) in self.iter().enumerate() {
            encoder.with_index(|encoder| v.encode(encoder), i)?;
        }
        Ok(())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write, T: Encode<W>> Encode<W> for alloc::collections::VecDeque<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_usize(self.len())?;
        for (i, v) in self.iter().enumerate() {
            encoder.with_index(|encoder| v.encode(encoder), i)?;
        }
        Ok(())
    }
}

impl<W: Write> Encode<W> for CStr {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.to_str()
            .map_err(|_| StringError::ConversionError)?
            .encode(encoder)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<W: Write> Encode<W> for alloc::ffi::CString {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.to_str()
            .map_err(|_| StringError::ConversionError)?
            .encode(encoder)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::ffi::OsStr {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.to_str()
            .ok_or(StringError::ConversionError)?
            .encode(encoder)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::ffi::OsString {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.to_str()
            .ok_or(StringError::ConversionError)?
            .encode(encoder)
    }
}

impl<W: Write> Encode<W> for Duration {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        encoder.write_u64(self.as_secs())?;
        encoder.write_u32(self.subsec_nanos())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::time::SystemTime {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        let since_epoch = self
            .duration_since(Self::UNIX_EPOCH)
            .unwrap_or(Duration::ZERO);
        since_epoch.encode(encoder)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::path::Path {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.as_os_str().encode(encoder)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::path::PathBuf {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.as_os_str().encode(encoder)
    }
}

impl<W: Write, T: Encode<W>> Encode<W> for Range<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.start.encode(encoder)?;
        self.end.encode(encoder)
    }
}

impl<W: Write, T: Encode<W>> Encode<W> for RangeInclusive<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.start().encode(encoder)?;
        self.end().encode(encoder)
    }
}

impl<W: Write, T: Encode<W>> Encode<W> for RangeFrom<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.start.encode(encoder)
    }
}

impl<W: Write, T: Encode<W>> Encode<W> for RangeTo<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.end.encode(encoder)
    }
}

impl<W: Write> Encode<W> for RangeFull {
    #[inline]
    fn encode(&self, _encoder: &mut Encoder<W>) -> EncodingResult<()> {
        Ok(())
    }
}

macro_rules! impl_nz_encode {
    ($($ty:ident => $write:ident);* $(;)? ) => {
	    $(
	    impl<W: $crate::io::Write> $crate::Encode<W> for core::num::$ty {
		    #[inline]
            fn encode(&self, encoder: &mut $crate::Encoder<W>) -> $crate::EncodingResult<()> {
		        encoder.$write(self.get())
		    }
	    }
	    )*
    };
}

impl_nz_encode! {
    NonZeroU8 => write_u8;
    NonZeroU16 => write_u16;
    NonZeroU32 => write_u32;
    NonZeroU64 => write_u64;
    NonZeroU128 => write_u128;
    NonZeroI8 => write_i8;
    NonZeroI16 => write_i16;
    NonZeroI32=> write_i32;
    NonZeroI64 => write_i64;
    NonZeroI128 => write_i128;
    NonZeroUsize => write_usize;
    NonZeroIsize => write_isize;
}

impl<W: Write, T: Encode<W>> Encode<W> for Bound<T> {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self {
            Bound::Included(x) => {
                encoder.write_uvariant(0u8)?;
                x.encode(encoder)
            }
            Bound::Excluded(x) => {
                encoder.write_uvariant(1u8)?;
                x.encode(encoder)
            }
            Bound::Unbounded => encoder.write_uvariant(2u8),
        }
    }
}

#[cfg(feature = "unstable")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "unstable")))]
impl<W: Write> Encode<W> for ! {
    #[inline]
    #[allow(unreachable_code, unused_variables)]
    fn encode(&self, _encoder: &mut Encoder<W>) -> EncodingResult<()> {
        enum Never {}
        fn never(never: &!) -> Never {
            *never
        }

        let never = never(self);
        match never {} // :O
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::net::IpAddr {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self {
            Self::V4(x) => {
                encoder.write_uvariant(0u8)?;
                x.encode(encoder)
            }
            Self::V6(x) => {
                encoder.write_uvariant(1u8)?;
                x.encode(encoder)
            }
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::net::Ipv4Addr {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.octets().encode(encoder)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::net::Ipv6Addr {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        self.octets().encode(encoder)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::net::SocketAddr {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        match self {
            Self::V4(x) => {
                encoder.write_uvariant(0u8)?;
                x.encode(encoder)
            }
            Self::V6(x) => {
                encoder.write_uvariant(1u8)?;
                x.encode(encoder)
            }
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::net::SocketAddrV4 {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        (self.ip(), self.port()).encode(encoder)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<W: Write> Encode<W> for std::net::SocketAddrV6 {
    #[inline]
    fn encode(&self, encoder: &mut Encoder<W>) -> EncodingResult<()> {
        (self.ip(), self.port(), self.flowinfo(), self.scope_id()).encode(encoder)
    }
}

macro_rules! impl_decode {
    ($($ty:ty => $read:ident);* $(;)? ) => {
	    $(
	    impl<R: $crate::io::Read> $crate::Decode<R> for $ty {
		    #[inline]
            fn decode(decoder: &mut $crate::Encoder<R>) -> $crate::EncodingResult<Self> where Self: Sized {
		        decoder.$read()
		    }
	    }
	    )*
    };
}

impl_decode! {
    u8 => read_u8;
    u16 => read_u16;
    u32 => read_u32;
    u64 => read_u64;
    u128 => read_u128;
    i8 => read_i8;
    i16 => read_i16;
    i32 => read_i32;
    i64 => read_i64;
    i128 => read_i128;
    bool => read_bool;
    char => read_char;
    f32 => read_f32;
    f64 => read_f64;
    usize => read_usize;
    isize => read_isize;
}

impl<R: Read> Decode<R> for () {
    #[inline]
    fn decode(_decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(())
    }
}

impl<R: Read, T: Decode<R>, const SIZE: usize> Decode<R> for [T; SIZE] {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        array_init::try_array_init(|i| decoder.with_index(|decoder| T::decode(decoder), i))
    }
}

macro_rules! consume {
    ($x:tt, $expr:expr) => {
        $expr
    };
}

macro_rules! tuple_decode {
    ($($name:ident)+) => {
	    #[allow(non_snake_case)]
	    impl<R: $crate::io::Read, $($name: $crate::Decode<R>),+> $crate::Decode<R> for ($($name),+) {
		    #[inline]
            fn decode(decoder: &mut $crate::Encoder<R>) -> $crate::EncodingResult<Self>{
			    Ok(($(
		            consume!($name, $crate::Decode::decode(decoder)?),
		        )+))
		    }
	    }
    };
}

tuple_decode! { A B }
tuple_decode! { A B C }
tuple_decode! { A B C D }
tuple_decode! { A B C D E }
tuple_decode! { A B C D E F }
tuple_decode! { A B C D E F G }
tuple_decode! { A B C D E F G H }
tuple_decode! { A B C D E F G H I }
tuple_decode! { A B C D E F G H I J }
tuple_decode! { A B C D E F G H I J K }
tuple_decode! { A B C D E F G H I J K L }
tuple_decode! { A B C D E F G H I J K L M }
tuple_decode! { A B C D E F G H I J K L M N }
tuple_decode! { A B C D E F G H I J K L M N O }
tuple_decode! { A B C D E F G H I J K L M N O P } // Up to 16

macro_rules! slice_borrow {
    ($($ty:ty => $borrow:ident);* $(;)?) => {
	    $(
	    #[allow(non_snake_case)]
	    impl<'data: 'a, 'a, R: BorrowRead<'data>> $crate::Decode<R> for &'a [$ty] {
		    #[inline]
            fn decode(decoder: &mut $crate::Encoder<R>) -> $crate::EncodingResult<Self>{
			    let len = decoder.read_usize()?;
			    let endianness = decoder.ctxt.settings.num_repr.endianness;
			    let num_encoding = decoder.ctxt.settings.num_repr.num_encoding;
			    decoder.$borrow(len, num_encoding, endianness)
		    }
	    }
	    )*
    };
}

slice_borrow! {
    u16 => borrow_u16_slice;
    u32 => borrow_u32_slice;
    u64 => borrow_u64_slice;
    u128 => borrow_u128_slice;
    i16 => borrow_i16_slice;
    i32 => borrow_i32_slice;
    i64 => borrow_i64_slice;
    i128 => borrow_i128_slice;
    f32 => borrow_f32_slice;
    f64 => borrow_f64_slice;
}

impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a [u8] {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let num_encoding = decoder.ctxt.settings.num_repr.num_encoding;
        decoder.borrow_u8_slice(len, num_encoding)
    }
}

impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a [i8] {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let num_encoding = decoder.ctxt.settings.num_repr.num_encoding;
        decoder.borrow_i8_slice(len, num_encoding)
    }
}

impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a [usize] {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let num_encoding = decoder.ctxt.settings.size_repr.num_encoding;
        let endianness = decoder.ctxt.settings.size_repr.endianness;
        let bit_width = decoder.ctxt.settings.size_repr.width;

        decoder.borrow_usize_slice(len, num_encoding, endianness, bit_width)
    }
}

impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a [isize] {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let num_encoding = decoder.ctxt.settings.size_repr.num_encoding;
        let endianness = decoder.ctxt.settings.size_repr.endianness;
        let bit_width = decoder.ctxt.settings.size_repr.width;

        decoder.borrow_isize_slice(len, num_encoding, endianness, bit_width)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read> Decode<R> for alloc::string::String {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        decoder.read_str()
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read> Decode<R> for alloc::boxed::Box<str> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let string = alloc::string::String::decode(decoder)?;
        Ok(string.into_boxed_str())
    }
}

impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a str {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        // Can only be borrowed when the string encoding is utf-8
        // else the user might get some surprises if we just assume it to be utf-8
        let str_encoding = decoder.ctxt.settings.string_repr.encoding;
        if str_encoding != StrEncoding::Utf8 {
            return Err(BorrowError::StrEncodingMismatch {
                found: str_encoding,
                while_decoding: StrEncoding::Utf8,
            }
            .into());
        }

        let len = decoder.read_usize()?;
        let bytes = decoder.borrow_u8_slice(len, NumEncoding::Fixed)?;
        Ok(core::str::from_utf8(bytes).map_err(|_| StringError::InvalidChar)?)
    }
}

impl<R: Read, T: Decode<R>> Decode<R> for Option<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(match decoder.read_bool()? {
            true => Some(T::decode(decoder)?),
            false => None,
        })
    }
}

impl<R: Read, T: Decode<R>, E: Decode<R>> Decode<R> for Result<T, E> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(match decoder.read_bool()? {
            true => Ok(T::decode(decoder)?),
            false => Err(E::decode(decoder)?),
        })
    }
}

impl<R: Read, T: ?Sized> Decode<R> for PhantomData<T> {
    #[inline]
    fn decode(_decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Decode<R>> Decode<R> for alloc::boxed::Box<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(alloc::boxed::Box::new(<T as Decode<R>>::decode(decoder)?))
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Decode<R>> Decode<R> for alloc::boxed::Box<[T]> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut vec = alloc::vec::Vec::new();
        vec.reserve_exact(len);

        for i in 0..len {
            vec.push(decoder.with_index(|decoder| T::decode(decoder), i)?);
        }

        Ok(vec.into_boxed_slice())
    }
}

// FIXME Re-enable this when specialization is stabilized
// Since we only disable the more generic implementation,
// re-enabling it won't be a breaking change.
//
// #[cfg(feature = "alloc")]
// #[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
// impl<'a, R: Read, T: ?Sized + alloc::borrow::ToOwned> Decode<R> for alloc::borrow::Cow<'a, T>
// where
//     <T as alloc::borrow::ToOwned>::Owned: Decode<R>,
// {
//     #[inline]
//     fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
//         Ok(Self::Owned(
//             <<T as alloc::borrow::ToOwned>::Owned as Decode<R>>::decode(decoder)?,
//         ))
//     }
// }

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<'data: 'a, 'a, R: BorrowRead<'data>, T: ?Sized + alloc::borrow::ToOwned> Decode<R>
    for alloc::borrow::Cow<'a, T>
where
    &'a T: Decode<R>,
{
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::Borrowed(<&T>::decode(decoder)?))
    }
}

impl<R: Read, T: Copy + Decode<R>> Decode<R> for Cell<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Cell::new(T::decode(decoder)?))
    }
}

impl<R: Read, T: Decode<R>> Decode<R> for RefCell<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(RefCell::new(T::decode(decoder)?))
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read, T: Decode<R>> Decode<R> for std::sync::Mutex<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(T::decode(decoder)?))
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read, T: Decode<R>> Decode<R> for std::sync::RwLock<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(T::decode(decoder)?))
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Decode<R>> Decode<R> for alloc::rc::Rc<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(T::decode(decoder)?))
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Decode<R>> Decode<R> for alloc::sync::Arc<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(T::decode(decoder)?))
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, K: Ord + Decode<R>, V: Decode<R>> Decode<R> for alloc::collections::BTreeMap<K, V> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut map = Self::new();

        for _ in 0..len {
            map.insert(K::decode(decoder)?, V::decode(decoder)?);
        }

        Ok(map)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, K: Ord + Decode<R>> Decode<R> for alloc::collections::BTreeSet<K> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut set = Self::new();

        for _ in 0..len {
            set.insert(K::decode(decoder)?);
        }

        Ok(set)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Ord + Decode<R>> Decode<R> for alloc::collections::BinaryHeap<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut heap = Self::with_capacity(len);

        for _ in 0..len {
            heap.push(T::decode(decoder)?);
        }

        Ok(heap)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read, K: core::hash::Hash + Eq + Decode<R>, V: Decode<R>> Decode<R>
    for std::collections::hash_map::HashMap<K, V>
{
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut map = Self::with_capacity(len);

        for _ in 0..len {
            map.insert(K::decode(decoder)?, V::decode(decoder)?);
        }

        Ok(map)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read, K: core::hash::Hash + Eq + Decode<R>> Decode<R>
    for std::collections::hash_set::HashSet<K>
{
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut map = Self::with_capacity(len);

        for _ in 0..len {
            map.insert(K::decode(decoder)?);
        }

        Ok(map)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Decode<R>> Decode<R> for alloc::collections::LinkedList<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut list = Self::new();

        for i in 0..len {
            list.push_back(decoder.with_index(|decoder| T::decode(decoder), i)?)
        }

        Ok(list)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Decode<R>> Decode<R> for alloc::collections::VecDeque<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut deque = Self::with_capacity(len);

        for i in 0..len {
            deque.push_back(decoder.with_index(|decoder| T::decode(decoder), i)?)
        }

        Ok(deque)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read, T: Decode<R>> Decode<R> for alloc::vec::Vec<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let len = decoder.read_usize()?;
        let mut vec = Self::with_capacity(len);

        for i in 0..len {
            vec.push(decoder.with_index(|decoder| T::decode(decoder), i)?);
        }

        Ok(vec)
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read> Decode<R> for alloc::ffi::CString {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let mut data = alloc::vec::Vec::new();
        loop {
            let val = decoder.read_byte()?;
            data.push(val);
            if val == 0 {
                break;
            }
        }
        // PANIC SAFETY:
        // The above implementation guarantees that the vec will only ever
        // contain 1 null value and that it will be in the last position.
        // It is safe to unwrap
        Ok(alloc::ffi::CString::from_vec_with_nul(data).unwrap())
    }
}

#[cfg(feature = "alloc")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "alloc")))]
impl<R: Read> Decode<R> for alloc::boxed::Box<CStr> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(alloc::ffi::CString::decode(decoder)?.into_boxed_c_str())
    }
}

impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a CStr {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let mut len = 1;
        loop {
            let slice = decoder.peek_bytes(len)?;
            if &slice[len - 1..len] == &[0] {
                break;
            }
            len += 1;
        }

        // PANIC SAFETY:
        // The above implementation guarantees that the slice will only ever
        // contain 1 null value and that it will be in the last position.
        // It is safe to unwrap
        Ok(CStr::from_bytes_with_nul(decoder.borrow_u8_slice(len, NumEncoding::Fixed)?).unwrap())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::ffi::OsString {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        use core::str::FromStr;
        let string = String::decode(decoder)?;

        // PANIC SAFETY:
        // The error is of type `Infallible`
        Ok(std::ffi::OsString::from_str(&string).unwrap())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for alloc::boxed::Box<std::ffi::OsStr> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(std::ffi::OsString::decode(decoder)?.into_boxed_os_str())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a std::ffi::OsStr {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let string = <&str>::decode(decoder)?;
        Ok(std::ffi::OsStr::new(string))
    }
}

impl<R: Read> Decode<R> for Duration {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(decoder.read_u64()?, decoder.read_u32()?))
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::time::SystemTime {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        let duration = Duration::decode(decoder)?;
        Ok(Self::UNIX_EPOCH + duration)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::path::PathBuf {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::from(std::ffi::OsString::decode(decoder)?))
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for alloc::boxed::Box<std::path::Path> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(std::path::PathBuf::decode(decoder)?.into_boxed_path())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<'data: 'a, 'a, R: BorrowRead<'data>> Decode<R> for &'a std::path::Path {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(std::path::Path::new(<&std::ffi::OsStr>::decode(
            decoder,
        )?))
    }
}

impl<R: Read, T: Decode<R>> Decode<R> for Range<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self {
            start: T::decode(decoder)?,
            end: T::decode(decoder)?,
        })
    }
}

impl<R: Read, T: Decode<R>> Decode<R> for RangeInclusive<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(T::decode(decoder)?, T::decode(decoder)?))
    }
}

impl<R: Read, T: Decode<R>> Decode<R> for RangeTo<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self {
            end: T::decode(decoder)?,
        })
    }
}

impl<R: Read, T: Decode<R>> Decode<R> for RangeFrom<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self {
            start: T::decode(decoder)?,
        })
    }
}

impl<R: Read> Decode<R> for RangeFull {
    #[inline]
    fn decode(_decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self)
    }
}

impl<R: Read, T: Decode<R>> Decode<R> for Bound<T> {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(match decoder.read_uvariant::<u8>()? {
            0 => Self::Included(T::decode(decoder)?),
            1 => Self::Excluded(T::decode(decoder)?),
            2 => Self::Unbounded,
            x => return Err(EncodingError::invalid_variant(x)),
        })
    }
}

macro_rules! impl_nz_decode {
    ($($ty:ident => $read:ident);* $(;)? ) => {
	    $(
	    impl<R: $crate::io::Read> $crate::Decode<R> for core::num::$ty {
		    #[inline]
            fn decode(decoder: &mut $crate::Encoder<R>) -> $crate::EncodingResult<Self> {
		        Ok(
			        Self::new(decoder.$read()?)
			        .ok_or($crate::val_error!(concat!("Found a value of 0 while decoding a ", stringify!($ty))))?
		        )
		    }
	    }
	    )*
    };
}

impl_nz_decode! {
    NonZeroU8 => read_u8;
    NonZeroU16 => read_u16;
    NonZeroU32 => read_u32;
    NonZeroU64 => read_u64;
    NonZeroU128 => read_u128;
    NonZeroI8 => read_i8;
    NonZeroI16 => read_i16;
    NonZeroI32=> read_i32;
    NonZeroI64 => read_i64;
    NonZeroI128 => read_i128;
    NonZeroUsize => read_usize;
    NonZeroIsize => read_isize;
}

#[cfg(feature = "unstable")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "unstable")))]
impl<R: Read> Decode<R> for ! {
    #[inline]
    fn decode(_decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        return Err(EncodingError::invalid_variant(0usize));
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::net::IpAddr {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(match decoder.read_uvariant::<u8>()? {
            0 => Self::V4(decoder.decode_value()?),
            1 => Self::V6(decoder.decode_value()?),
            x => return Err(EncodingError::invalid_variant(x)),
        })
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::net::Ipv4Addr {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(<Self as From<[u8; 4]>>::from(decoder.decode_value()?))
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::net::Ipv6Addr {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(<Self as From<[u8; 16]>>::from(decoder.decode_value()?))
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::net::SocketAddr {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(match decoder.read_uvariant::<u8>()? {
            0 => Self::V4(decoder.decode_value()?),
            1 => Self::V6(decoder.decode_value()?),
            x => return Err(EncodingError::invalid_variant(x)),
        })
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::net::SocketAddrV4 {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(
            std::net::Ipv4Addr::decode(decoder)?,
            u16::decode(decoder)?,
        ))
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "unstable", doc(cfg(feature = "std")))]
impl<R: Read> Decode<R> for std::net::SocketAddrV6 {
    #[inline]
    fn decode(decoder: &mut Encoder<R>) -> EncodingResult<Self> {
        Ok(Self::new(
            std::net::Ipv6Addr::decode(decoder)?,
            u16::decode(decoder)?,
            u32::decode(decoder)?,
            u32::decode(decoder)?,
        ))
    }
}
