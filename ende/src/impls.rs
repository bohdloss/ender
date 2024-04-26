use core::cell::{Cell, RefCell};
use core::ffi::CStr;
use core::marker::PhantomData;
use core::ops::{Bound, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo};
use core::ops::Deref;
use core::time::Duration;

use crate::{BorrowDecode, Decode, Encode, Encoder, EncodingError, EncodingResult, NumEncoding, StringError};
use crate::io::{BorrowRead, Read, Write};

// Primitives

macro_rules! impl_encode {
    ($($ty:ty => $write:ident);* $(;)? ) => {
	    $(
	    impl $crate::Encode for $ty {
		    fn encode<T: $crate::io::Write>(&self, encoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<()> {
		        encoder.$write(*self)
		    }
	    }
	    )*
    };
}

impl_encode!{
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

impl Encode for () {
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		Ok(())
	}
}

impl<T: ?Sized + Encode> Encode for &T {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self, encoder)
	}
}

impl<T: ?Sized + Encode> Encode for &mut T {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self, encoder)
	}
}

impl<T: Encode> Encode for &[T] {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for i in 0..self.len() {
			self[i].encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Encode, const SIZE: usize> Encode for [T; SIZE] {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		for i in 0..SIZE {
			self[i].encode(encoder)?;
		}
		Ok(())
	}
}

macro_rules! tuple_encode {
    ($($name:ident)+) => {
	    #[allow(non_snake_case)]
	    impl<$($name: $crate::Encode),+> $crate::Encode for ($($name),+) {
		    fn encode<__T: $crate::io::Write>(&self, encoder: &mut $crate::Encoder<__T>) -> $crate::EncodingResult<()> {
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
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Encode for alloc::string::String {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.chars())
	}
}

impl Encode for str {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.chars())
	}
}

impl<T: Encode> Encode for Option<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			None => encoder.write_state(false),
			Some(value) => {
				encoder.write_state(true)?;
				value.encode(encoder)
			}
		}
	}
}

impl<T: Encode, E: Encode> Encode for Result<T, E> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			Err(err) => {
				encoder.write_state(false)?;
				err.encode(encoder)
			},
			Ok(ok) => {
				encoder.write_state(true)?;
				ok.encode(encoder)
			}
		}
	}
}

impl<T> Encode for PhantomData<T> {
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		Ok(())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::boxed::Box<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::rc::Rc<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

#[cfg(all(feature = "alloc"))]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::sync::Arc<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<'a, T: ?Sized + alloc::borrow::ToOwned> Encode for alloc::borrow::Cow<'a, T>
	where T: Encode,
	      <T as alloc::borrow::ToOwned>::Owned: Encode
{
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			Self::Borrowed(x) => x.encode(encoder),
			Self::Owned(x) => x.encode(encoder),
		}
	}
}

impl<T: Encode + Copy> Encode for Cell<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.get().encode(encoder)
	}
}

impl<T: Encode> Encode for RefCell<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self.try_borrow() {
			Ok(ok) => {
				<T as Encode>::encode(ok.deref(), encoder)
			}
			Err(_) => Err(EncodingError::LockError),
		}
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Encode> Encode for std::sync::Mutex<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self.lock() {
			Ok(ok) => {
				<T as Encode>::encode(ok.deref(), encoder)
			}
			Err(_) => Err(EncodingError::LockError),
		}
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Encode> Encode for std::sync::RwLock<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self.read() {
			Ok(ok) => {
				<T as Encode>::encode(ok.deref(), encoder)
			}
			Err(_) => Err(EncodingError::LockError),
		}
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<K: Encode, V: Encode> Encode for alloc::collections::BTreeMap<K, V> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for (k, v) in self.iter() {
			k.encode(encoder)?;
			v.encode(encoder)?;
		}
		Ok(())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<K: Encode> Encode for alloc::collections::BTreeSet<K> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for k in self.iter() {
			k.encode(encoder)?;
		}
		Ok(())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<K: Encode, V: Encode> Encode for std::collections::hash_map::HashMap<K, V> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for (k, v) in self.iter() {
			k.encode(encoder)?;
			v.encode(encoder)?;
		}
		Ok(())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<K: Encode> Encode for std::collections::hash_set::HashSet<K> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for k in self.iter() {
			k.encode(encoder)?;
		}
		Ok(())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::collections::BinaryHeap<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::collections::LinkedList<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::vec::Vec<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::collections::VecDeque<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl Encode for CStr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.to_str().map_err(|_| StringError::ConversionError)?.encode(encoder)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Encode for alloc::ffi::CString {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.to_str().map_err(|_| StringError::ConversionError)?.encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::ffi::OsStr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.to_str().ok_or(StringError::ConversionError)?.encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::ffi::OsString {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.to_str().ok_or(StringError::ConversionError)?.encode(encoder)
	}
}

impl Encode for Duration {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_u64(self.as_secs())?;
		encoder.write_u32(self.subsec_nanos())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::time::SystemTime {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		let since_epoch = self.duration_since(Self::UNIX_EPOCH).unwrap_or(Duration::ZERO);
		since_epoch.encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::path::Path {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.as_os_str().encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::path::PathBuf {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.as_os_str().encode(encoder)
	}
}

impl<T: Encode> Encode for Range<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.start.encode(encoder)?;
		self.end.encode(encoder)
	}
}

impl<T: Encode> Encode for RangeInclusive<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.start().encode(encoder)?;
		self.end().encode(encoder)
	}
}

impl<T: Encode> Encode for RangeFrom<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.start.encode(encoder)
	}
}

impl<T: Encode> Encode for RangeTo<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.end.encode(encoder)
	}
}

impl Encode for RangeFull {
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		Ok(())
	}
}

macro_rules! impl_nz_encode {
    ($($ty:ident => $write:ident);* $(;)? ) => {
	    $(
	    impl $crate::Encode for core::num::$ty {
		    fn encode<T: $crate::io::Write>(&self, encoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<()> {
		        encoder.$write(self.get())
		    }
	    }
	    )*
    };
}

impl_nz_encode!{
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

impl<T: Encode> Encode for Bound<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			Bound::Included(x) => {
				encoder.write_uvariant(0)?;
				x.encode(encoder)
			}
			Bound::Excluded(x) => {
				encoder.write_uvariant(1)?;
				x.encode(encoder)
			}
			Bound::Unbounded => {
				encoder.write_uvariant(2)
			}
		}
	}
}

#[cfg(feature = "unstable")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "unstable")))]
impl Encode for ! {
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		loop { /* :) */ }
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::IpAddr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			Self::V4(x) => {
				encoder.write_uvariant(0)?;
				x.encode(encoder)
			}
			Self::V6(x) => {
				encoder.write_uvariant(1)?;
				x.encode(encoder)
			}
		}
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::Ipv4Addr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.octets().encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::Ipv6Addr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.octets().encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::SocketAddr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			Self::V4(x) => {
				encoder.write_uvariant(0)?;
				x.encode(encoder)
			}
			Self::V6(x) => {
				encoder.write_uvariant(1)?;
				x.encode(encoder)
			}
		}
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::SocketAddrV4 {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		(self.ip(), self.port()).encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::SocketAddrV6 {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		(self.ip(), self.port(), self.flowinfo(), self.scope_id()).encode(encoder)
	}
}

macro_rules! impl_decode {
    ($($ty:ty => $read:ident);* $(;)? ) => {
	    $(
	    impl $crate::Decode for $ty {
		    fn decode<T: $crate::io::Read>(decoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<Self> where Self: Sized {
		        decoder.$read()
		    }
	    }
	    )*
    };
}

impl_decode!{
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

impl Decode for () {
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(())
	}
}

impl<T: Decode, const SIZE: usize> Decode for [T; SIZE] {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		array_init::try_array_init(|_| T::decode(decoder))
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
	    impl<$($name: $crate::Decode),+> $crate::Decode for ($($name),+) {
		    fn decode<__T: $crate::io::Read>(decoder: &mut $crate::Encoder<__T>) -> $crate::EncodingResult<Self>{
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
	    impl<'data> $crate::BorrowDecode<'data> for &'data [$ty] {
		    fn borrow_decode<Reader: $crate::io::BorrowRead<'data>>(decoder: &mut $crate::Encoder<Reader>) -> $crate::EncodingResult<Self>{
			    let len = decoder.read_length()?;
			    let endianness = decoder.ctxt.settings.num_repr.endianness;
			    let num_encoding = decoder.ctxt.settings.num_repr.num_encoding;
			    decoder.$borrow(len, num_encoding, endianness)
		    }
	    }
	    )*
    };
}

slice_borrow!{
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

impl<'data> BorrowDecode<'data> for &'data [u8] {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let num_encoding = decoder.ctxt.settings.num_repr.num_encoding;
		decoder.borrow_u8_slice(len, num_encoding)
	}
}

impl<'data> BorrowDecode<'data> for &'data [i8] {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let num_encoding = decoder.ctxt.settings.num_repr.num_encoding;
		decoder.borrow_i8_slice(len, num_encoding)
	}
}

impl<'data> BorrowDecode<'data> for &'data [usize] {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let num_encoding = decoder.ctxt.settings.size_repr.num_encoding;
		let endianness = decoder.ctxt.settings.size_repr.endianness;
		let bit_width = decoder.ctxt.settings.size_repr.width;
		
		decoder.borrow_usize_slice(len, num_encoding, endianness, bit_width)
	}
}

impl<'data> BorrowDecode<'data> for &'data [isize] {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let num_encoding = decoder.ctxt.settings.size_repr.num_encoding;
		let endianness = decoder.ctxt.settings.size_repr.endianness;
		let bit_width = decoder.ctxt.settings.size_repr.width;

		decoder.borrow_isize_slice(len, num_encoding, endianness, bit_width)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Decode for alloc::string::String {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		decoder.read_string()
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Decode for alloc::boxed::Box<str> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let string = alloc::string::String::decode(decoder)?;
		Ok(string.into_boxed_str())
	}
}

impl<'data> BorrowDecode<'data> for &'data str {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let bytes = decoder.borrow_u8_slice(len, NumEncoding::Fixed)?;
		Ok(core::str::from_utf8(bytes).map_err(|_| StringError::InvalidUtf8)?)
	}
}

impl<T: Decode> Decode for Option<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(match decoder.read_state()? {
			true => Some(T::decode(decoder)?),
			false => None,
		})
	}
}

impl<T: Decode, E: Decode> Decode for Result<T, E> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(match decoder.read_state()? {
			true => Ok(T::decode(decoder)?),
			false => Err(E::decode(decoder)?),
		})
	}
}

impl<T: ?Sized> Decode for PhantomData<T> {
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::boxed::Box<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(alloc::boxed::Box::new(<T as Decode>::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::boxed::Box<[T]> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut vec = alloc::vec::Vec::new();
		vec.reserve_exact(len);

		for _ in 0..len {
			vec.push(T::decode(decoder)?);
		}

		Ok(vec.into_boxed_slice())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<'a, T: ?Sized + alloc::borrow::ToOwned> Decode for alloc::borrow::Cow<'a, T>
where <T as alloc::borrow::ToOwned>::Owned: Decode
{
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::Owned(<<T as alloc::borrow::ToOwned>::Owned as Decode>::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<'a, T: ?Sized + alloc::borrow::ToOwned> BorrowDecode<'a> for alloc::borrow::Cow<'a, T>
	where &'a T: BorrowDecode<'a>
{
	fn borrow_decode<Reader: BorrowRead<'a>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::Borrowed(<&T>::borrow_decode(decoder)?))
	}
}

impl<T: Copy + Decode> Decode for Cell<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Cell::new(T::decode(decoder)?))
	}
}

impl<T: Decode> Decode for RefCell<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(RefCell::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Decode> Decode for std::sync::Mutex<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Decode> Decode for std::sync::RwLock<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::rc::Rc<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::sync::Arc<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<K: Ord + Decode, V: Decode> Decode for alloc::collections::BTreeMap<K, V> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut map = Self::new();
		
		for _ in 0..len {
			map.insert(K::decode(decoder)?, V::decode(decoder)?);
		}
		
		Ok(map)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<K: Ord + Decode> Decode for alloc::collections::BTreeSet<K> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut set = Self::new();

		for _ in 0..len {
			set.insert(K::decode(decoder)?);
		}

		Ok(set)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Ord + Decode> Decode for alloc::collections::BinaryHeap<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut heap = Self::with_capacity(len);
		
		for _ in 0..len {
			heap.push(T::decode(decoder)?);
		}
		
		Ok(heap)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<K: core::hash::Hash + Eq + Decode, V: Decode> Decode for std::collections::hash_map::HashMap<K, V> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut map = Self::with_capacity(len);

		for _ in 0..len {
			map.insert(K::decode(decoder)?, V::decode(decoder)?);
		}

		Ok(map)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<K: core::hash::Hash + Eq + Decode> Decode for std::collections::hash_set::HashSet<K> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut map = Self::with_capacity(len);

		for _ in 0..len {
			map.insert(K::decode(decoder)?);
		}

		Ok(map)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::collections::LinkedList<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut list = Self::new();

		for _ in 0..len {
			list.push_back(T::decode(decoder)?)
		}

		Ok(list)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::collections::VecDeque<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut deque = Self::with_capacity(len);

		for _ in 0..len {
			deque.push_back(T::decode(decoder)?)
		}

		Ok(deque)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::vec::Vec<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut vec = Self::with_capacity(len);
		
		for _ in 0..len {
			vec.push(T::decode(decoder)?);
		}
		
		Ok(vec)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Decode for alloc::ffi::CString {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let mut data = alloc::vec::Vec::new();
		loop {
			let val = decoder.read_u8_direct(NumEncoding::Fixed, crate::Endianness::native())?;
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
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Decode for alloc::boxed::Box<CStr> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(alloc::ffi::CString::decode(decoder)?.into_boxed_c_str())
	}
}

impl<'data> BorrowDecode<'data> for &'data CStr {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
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
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::ffi::OsString {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		use core::str::FromStr;
		let string = String::decode(decoder)?;
		
		// PANIC SAFETY:
		// The error is of type `Infallible`
		Ok(std::ffi::OsString::from_str(&string).unwrap())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for alloc::boxed::Box<std::ffi::OsStr> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(std::ffi::OsString::decode(decoder)?.into_boxed_os_str())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<'data> BorrowDecode<'data> for &'data std::ffi::OsStr {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let string = <&str>::borrow_decode(decoder)?;
		Ok(std::ffi::OsStr::new(string))
	}
}

impl Decode for Duration {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(decoder.read_u64()?, decoder.read_u32()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::time::SystemTime {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let duration = Duration::decode(decoder)?;
		Ok(Self::UNIX_EPOCH + duration)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::path::PathBuf {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> { 
		Ok(Self::from(std::ffi::OsString::decode(decoder)?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for alloc::boxed::Box<std::path::Path> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(std::path::PathBuf::decode(decoder)?.into_boxed_path())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<'data> BorrowDecode<'data> for &'data std::path::Path {
	fn borrow_decode<Reader: BorrowRead<'data>>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(std::path::Path::new(<&std::ffi::OsStr>::borrow_decode(decoder)?))
	}
}

impl<T: Decode> Decode for Range<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self {
			start: T::decode(decoder)?,
			end: T::decode(decoder)?,
		})
	}
}

impl<T: Decode> Decode for RangeInclusive<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(T::decode(decoder)?, T::decode(decoder)?))
	}
}

impl<T: Decode> Decode for RangeTo<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self {
			end: T::decode(decoder)?,
		})
	}
}

impl<T: Decode> Decode for RangeFrom<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self {
			start: T::decode(decoder)?,
		})
	}
}

impl Decode for RangeFull {
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self)
	}
}

impl<T: Decode> Decode for Bound<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(match decoder.read_uvariant()? {
			0 => Self::Included(T::decode(decoder)?),
			1 => Self::Excluded(T::decode(decoder)?),
			2 => Self::Unbounded,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

macro_rules! impl_nz_decode {
    ($($ty:ident => $read:ident);* $(;)? ) => {
	    $(
	    impl $crate::Decode for core::num::$ty {
		    fn decode<T: $crate::io::Read>(decoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<Self> {
		        Ok(
			        Self::new(decoder.$read()?)
			        .ok_or($crate::EncodingError::validation_error(format_args!("Found a value of 0 while decoding a NonZero*")))?
		        )
		    }
	    }
	    )*
    };
}

impl_nz_decode!{
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
#[cfg_attr(doc_cfg, doc(cfg(feature = "unstable")))]
impl Decode for ! {
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		return Err(EncodingError::InvalidVariant)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::IpAddr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(match decoder.read_uvariant()? {
			0 => Self::V4(decoder.decode_value()?),
			1 => Self::V6(decoder.decode_value()?),
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::Ipv4Addr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(<Self as From<[u8; 4]>>::from(decoder.decode_value()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::Ipv6Addr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(<Self as From<[u8; 16]>>::from(decoder.decode_value()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::SocketAddr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(match decoder.read_uvariant()? {
			0 => Self::V4(decoder.decode_value()?),
			1 => Self::V6(decoder.decode_value()?),
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::SocketAddrV4 {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(std::net::Ipv4Addr::decode(decoder)?, u16::decode(decoder)?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::SocketAddrV6 {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::new(
			std::net::Ipv6Addr::decode(decoder)?,
			u16::decode(decoder)?,
			u32::decode(decoder)?,
			u32::decode(decoder)?,
		))
	}
}