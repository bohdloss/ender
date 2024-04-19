use core::marker::PhantomData;
use core::cell::{Cell, RefCell};
use core::ffi::CStr;
use core::ops::Deref;
use core::ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, Bound};
use core::time::Duration;
use crate::{Decode, Encode, Encoder, EncodingError, EncodingResult, StringError, Write, Read};

// Primitives

macro_rules! impl_encode {
    ($($ty:ty => $write:ident);* $(;)? ) => {
	    $(
	    impl $crate::Encode for $ty {
		    fn encode<T: $crate::Write>(&self, encoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<(), T::Error> {
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
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		Ok(())
	}
}

impl<T: ?Sized + Encode> Encode for &T {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		<T as Encode>::encode(self, encoder)
	}
}

impl<T: ?Sized + Encode> Encode for &mut T {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		<T as Encode>::encode(self, encoder)
	}
}

impl<T: Encode> Encode for &[T] {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_length(self.len())?;
		for i in 0..self.len() {
			self[i].encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Encode, const SIZE: usize> Encode for [T; SIZE] {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
		    fn encode<__T: $crate::Write>(&self, encoder: &mut $crate::Encoder<__T>) -> $crate::EncodingResult<(), __T::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self.as_str())
	}
}

impl Encode for str {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self)
	}
}

impl<T: Encode> Encode for Option<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		Ok(())
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::boxed::Box<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::rc::Rc<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

#[cfg(all(feature = "alloc"))]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Encode> Encode for alloc::sync::Arc<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<'a, T: ?Sized + alloc::borrow::ToOwned> Encode for alloc::borrow::Cow<'a, T>
	where T: Encode,
	      <T as alloc::borrow::ToOwned>::Owned: Encode
{
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		match self {
			Self::Borrowed(x) => x.encode(encoder),
			Self::Owned(x) => x.encode(encoder),
		}
	}
}

impl<T: Encode + Copy> Encode for Cell<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		self.get().encode(encoder)
	}
}

impl<T: Encode> Encode for RefCell<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl Encode for CStr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self.to_str().map_err(|_| StringError::InvalidUtf8)?)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Encode for alloc::ffi::CString {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self.to_str().map_err(|_| StringError::InvalidUtf8)?)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::ffi::OsStr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::ffi::OsString {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
	}
}

impl Encode for Duration {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_u64(self.as_secs())?;
		encoder.write_u32(self.subsec_nanos())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::time::SystemTime {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		let since_epoch = self.duration_since(Self::UNIX_EPOCH).unwrap_or(Duration::ZERO);
		since_epoch.encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::path::Path {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::path::PathBuf {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
	}
}

impl<T: Encode> Encode for Range<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		self.start.encode(encoder)?;
		self.end.encode(encoder)
	}
}

impl<T: Encode> Encode for RangeInclusive<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		self.start().encode(encoder)?;
		self.end().encode(encoder)
	}
}

impl<T: Encode> Encode for RangeFrom<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		self.start.encode(encoder)
	}
}

impl<T: Encode> Encode for RangeTo<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		self.end.encode(encoder)
	}
}

impl Encode for RangeFull {
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		Ok(())
	}
}

macro_rules! impl_nz_encode {
    ($($ty:ident => $write:ident);* $(;)? ) => {
	    $(
	    impl $crate::Encode for core::num::$ty {
		    fn encode<T: $crate::Write>(&self, encoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<(), T::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, _encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		loop { /* :) */ }
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::IpAddr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		self.octets().encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::Ipv6Addr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		self.octets().encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::SocketAddr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
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
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		(self.ip(), self.port()).encode(encoder)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Encode for std::net::SocketAddrV6 {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<(), Writer::Error> {
		(self.ip(), self.port(), self.flowinfo(), self.scope_id()).encode(encoder)
	}
}

macro_rules! impl_decode {
    ($($ty:ty => $read:ident);* $(;)? ) => {
	    $(
	    impl $crate::Decode for $ty {
		    fn decode<T: $crate::Read>(decoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<Self, T::Error> where Self: Sized {
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
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(())
	}
}

impl<T: Decode, const SIZE: usize> Decode for [T; SIZE] {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
		    fn decode<__T: $crate::Read>(decoder: &mut $crate::Encoder<__T>) -> $crate::EncodingResult<Self, __T::Error> where Self: Sized {
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

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Decode for alloc::string::String {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		decoder.read_string()
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Decode for alloc::boxed::Box<str> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		let string = alloc::string::String::decode(decoder)?;
		Ok(string.into_boxed_str())
	}
}

impl<T: Decode> Decode for Option<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(match decoder.read_state()? {
			true => Some(T::decode(decoder)?),
			false => None,
		})
	}
}

impl<T: Decode, E: Decode> Decode for Result<T, E> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(match decoder.read_state()? {
			true => Ok(T::decode(decoder)?),
			false => Err(E::decode(decoder)?),
		})
	}
}

impl<T: ?Sized> Decode for PhantomData<T> {
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::boxed::Box<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(alloc::boxed::Box::new(<T as Decode>::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::boxed::Box<[T]> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::Owned(<<T as alloc::borrow::ToOwned>::Owned as Decode>::decode(decoder)?))
	}
}

impl<T: Copy + Decode> Decode for Cell<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Cell::new(T::decode(decoder)?))
	}
}

impl<T: Decode> Decode for RefCell<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(RefCell::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Decode> Decode for std::sync::Mutex<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl<T: Decode> Decode for std::sync::RwLock<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::rc::Rc<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<T: Decode> Decode for alloc::sync::Arc<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(T::decode(decoder)?))
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl<K: Ord + Decode, V: Decode> Decode for alloc::collections::BTreeMap<K, V> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(decoder.read_string()?).map_err(|_| StringError::InvalidCString)?)
	}
}

#[cfg(feature = "alloc")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "alloc")))]
impl Decode for alloc::boxed::Box<CStr> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(alloc::ffi::CString::decode(decoder)?.into_boxed_c_str())
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::ffi::OsString {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::from(decoder.read_string()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for alloc::boxed::Box<std::ffi::OsStr> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(std::ffi::OsString::decode(decoder)?.into_boxed_os_str())
	}
}

impl Decode for Duration {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(decoder.read_u64()?, decoder.read_u32()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::time::SystemTime {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		let duration = Duration::decode(decoder)?;
		Ok(Self::UNIX_EPOCH + duration)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::path::PathBuf {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::from(decoder.read_string()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for alloc::boxed::Box<std::path::Path> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(std::path::PathBuf::decode(decoder)?.into_boxed_path())
	}
}

impl<T: Decode> Decode for Range<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self {
			start: T::decode(decoder)?,
			end: T::decode(decoder)?,
		})
	}
}

impl<T: Decode> Decode for RangeInclusive<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(T::decode(decoder)?, T::decode(decoder)?))
	}
}

impl<T: Decode> Decode for RangeTo<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self {
			end: T::decode(decoder)?,
		})
	}
}

impl<T: Decode> Decode for RangeFrom<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self {
			start: T::decode(decoder)?,
		})
	}
}

impl Decode for RangeFull {
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self)
	}
}

impl<T: Decode> Decode for Bound<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
		    fn decode<T: $crate::Read>(decoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<Self, T::Error> {
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
	fn decode<Reader: Read>(_decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		return Err(EncodingError::InvalidVariant)
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::IpAddr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(<Self as From<[u8; 4]>>::from(decoder.decode_value()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::Ipv6Addr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(<Self as From<[u8; 16]>>::from(decoder.decode_value()?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::SocketAddr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
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
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(std::net::Ipv4Addr::decode(decoder)?, u16::decode(decoder)?))
	}
}

#[cfg(feature = "std")]
#[cfg_attr(doc_cfg, doc(cfg(feature = "std")))]
impl Decode for std::net::SocketAddrV6 {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self, Reader::Error> {
		Ok(Self::new(
			std::net::Ipv6Addr::decode(decoder)?,
			u16::decode(decoder)?,
			u32::decode(decoder)?,
			u32::decode(decoder)?,
		))
	}
}