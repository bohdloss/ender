use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::{BinaryHeap, BTreeMap, BTreeSet, HashMap, HashSet, LinkedList, VecDeque};
use std::ffi::{CStr, CString, OsStr, OsString};
use std::hash::Hash;
use std::io::{Read, Write};
use std::marker::PhantomData;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6};
use std::ops::{Bound, Deref, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};
use std::time::{Duration, SystemTime};
use crate::{Decode, Encode, Encoder, EncodingError, EncodingResult, StringError};

// Primitives

macro_rules! impl_encode {
    ($($ty:ty => $write:ident);* $(;)? ) => {
	    $(
	    impl $crate::Encode for $ty {
		    fn encode<T: std::io::Write>(&self, encoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<()> {
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
		    fn encode<__T: std::io::Write>(&self, encoder: &mut $crate::Encoder<__T>) -> $crate::EncodingResult<()> {
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

impl Encode for String {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.as_str())
	}
}

impl Encode for str {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self)
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

impl<T: Encode> Encode for Box<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

impl<T: Encode> Encode for Rc<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

impl<T: Encode> Encode for Arc<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		<T as Encode>::encode(self.deref(), encoder)
	}
}

impl<'a, T: ?Sized + ToOwned> Encode for Cow<'a, T>
	where T: Encode,
	      <T as ToOwned>::Owned: Encode
{
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			Cow::Borrowed(x) => x.encode(encoder),
			Cow::Owned(x) => x.encode(encoder),
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

impl<T: Encode> Encode for Mutex<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self.lock() {
			Ok(ok) => {
				<T as Encode>::encode(ok.deref(), encoder)
			}
			Err(_) => Err(EncodingError::LockError),
		}
	}
}

impl<T: Encode> Encode for RwLock<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self.read() {
			Ok(ok) => {
				<T as Encode>::encode(ok.deref(), encoder)
			}
			Err(_) => Err(EncodingError::LockError),
		}
	}
}

impl<K: Encode, V: Encode> Encode for BTreeMap<K, V> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for (k, v) in self.iter() {
			k.encode(encoder)?;
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl<K: Encode> Encode for BTreeSet<K> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for k in self.iter() {
			k.encode(encoder)?;
		}
		Ok(())
	}
}

impl<K: Encode, V: Encode> Encode for HashMap<K, V> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for (k, v) in self.iter() {
			k.encode(encoder)?;
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl<K: Encode> Encode for HashSet<K> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for k in self.iter() {
			k.encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Encode> Encode for BinaryHeap<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Encode> Encode for LinkedList<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Encode> Encode for Vec<T> {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_length(self.len())?;
		for v in self.iter() {
			v.encode(encoder)?;
		}
		Ok(())
	}
}

impl<T: Encode> Encode for VecDeque<T> {
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
		encoder.write_str(self.to_str().map_err(|_| StringError::InvalidUtf8)?)
	}
}

impl Encode for CString {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.to_str().map_err(|_| StringError::InvalidUtf8)?)
	}
}

impl Encode for OsStr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
	}
}

impl Encode for OsString {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
	}
}

impl Encode for Duration {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_u64(self.as_secs())?;
		encoder.write_u32(self.subsec_nanos())
	}
}
 
impl Encode for SystemTime {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		let since_epoch = self.duration_since(Self::UNIX_EPOCH).unwrap_or(Duration::ZERO);
		since_epoch.encode(encoder)
	}
}

impl Encode for Path {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
	}
}

impl Encode for PathBuf {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		encoder.write_str(self.to_str().ok_or(StringError::InvalidUtf8)?)
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
	    impl $crate::Encode for std::num::$ty {
		    fn encode<T: std::io::Write>(&self, encoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<()> {
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

impl Encode for IpAddr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			IpAddr::V4(x) => {
				encoder.write_uvariant(0)?;
				x.encode(encoder)
			}
			IpAddr::V6(x) => {
				encoder.write_uvariant(1)?;
				x.encode(encoder)
			}
		}
	}
}

impl Encode for Ipv4Addr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.octets().encode(encoder)
	}
}

impl Encode for Ipv6Addr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		self.octets().encode(encoder)
	}
}

impl Encode for SocketAddr {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		match self {
			SocketAddr::V4(x) => {
				encoder.write_uvariant(0)?;
				x.encode(encoder)
			}
			SocketAddr::V6(x) => {
				encoder.write_uvariant(1)?;
				x.encode(encoder)
			}
		}
	}
}

impl Encode for SocketAddrV4 {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		(self.ip(), self.port()).encode(encoder)
	}
}

impl Encode for SocketAddrV6 {
	fn encode<Writer: Write>(&self, encoder: &mut Encoder<Writer>) -> EncodingResult<()> {
		(self.ip(), self.port(), self.flowinfo(), self.scope_id()).encode(encoder)
	}
}

macro_rules! impl_decode {
    ($($ty:ty => $read:ident);* $(;)? ) => {
	    $(
	    impl $crate::Decode for $ty {
		    fn decode<T: std::io::Read>(decoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<Self> where Self: Sized {
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
		    fn decode<__T: std::io::Read>(decoder: &mut $crate::Encoder<__T>) -> $crate::EncodingResult<Self> where Self: Sized {
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

impl Decode for String {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		decoder.read_string()
	}
}

impl Decode for Box<str> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let string = String::decode(decoder)?;
		Ok(string.into_boxed_str())
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

impl<T: Decode> Decode for Box<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Box::new(<T as Decode>::decode(decoder)?))
	}
}

impl<T: Decode> Decode for Box<[T]> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut vec = Vec::new();
		vec.reserve_exact(len);

		for _ in 0..len {
			vec.push(T::decode(decoder)?);
		}

		Ok(vec.into_boxed_slice())
	}
}

impl<'a, T: ?Sized + ToOwned> Decode for Cow<'a, T>
where <T as ToOwned>::Owned: Decode
{
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Self::Owned(<<T as ToOwned>::Owned as Decode>::decode(decoder)?))
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

impl<T: Decode> Decode for Mutex<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Mutex::new(T::decode(decoder)?))
	}
}

impl<T: Decode> Decode for RwLock<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(RwLock::new(T::decode(decoder)?))
	}
}

impl<T: Decode> Decode for Rc<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Rc::new(T::decode(decoder)?))
	}
}

impl<T: Decode> Decode for Arc<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Arc::new(T::decode(decoder)?))
	}
}

impl<K: Ord + Decode, V: Decode> Decode for BTreeMap<K, V> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut map = BTreeMap::new();
		
		for _ in 0..len {
			map.insert(K::decode(decoder)?, V::decode(decoder)?);
		}
		
		Ok(map)
	}
}

impl<K: Ord + Decode> Decode for BTreeSet<K> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut set = BTreeSet::new();

		for _ in 0..len {
			set.insert(K::decode(decoder)?);
		}

		Ok(set)
	}
}

impl<T: Ord + Decode> Decode for BinaryHeap<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut heap = BinaryHeap::with_capacity(len);
		
		for _ in 0..len {
			heap.push(T::decode(decoder)?);
		}
		
		Ok(heap)
	}
}

impl<K: Hash + Eq + Decode, V: Decode> Decode for HashMap<K, V> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut map = HashMap::with_capacity(len);

		for _ in 0..len {
			map.insert(K::decode(decoder)?, V::decode(decoder)?);
		}

		Ok(map)
	}
}

impl<K: Hash + Eq + Decode> Decode for HashSet<K> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut map = HashSet::with_capacity(len);

		for _ in 0..len {
			map.insert(K::decode(decoder)?);
		}

		Ok(map)
	}
}

impl<T: Decode> Decode for LinkedList<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut list = LinkedList::new();

		for _ in 0..len {
			list.push_back(T::decode(decoder)?)
		}

		Ok(list)
	}
}

impl<T: Decode> Decode for VecDeque<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut deque = VecDeque::with_capacity(len);

		for _ in 0..len {
			deque.push_back(T::decode(decoder)?)
		}

		Ok(deque)
	}
}

impl<T: Decode> Decode for Vec<T> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let len = decoder.read_length()?;
		let mut vec = Vec::with_capacity(len);
		
		for _ in 0..len {
			vec.push(T::decode(decoder)?);
		}
		
		Ok(vec)
	}
}

impl Decode for CString {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(CString::new(decoder.read_string()?).map_err(|_| StringError::InvalidCString)?)
	}
}

impl Decode for Box<CStr> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(CString::decode(decoder)?.into_boxed_c_str())
	}
}

impl Decode for OsString {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(OsString::from(decoder.read_string()?))
	}
}

impl Decode for Box<OsStr> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(OsString::decode(decoder)?.into_boxed_os_str())
	}
}

impl Decode for Duration {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(Duration::new(decoder.read_u64()?, decoder.read_u32()?))
	}
}

impl Decode for SystemTime {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		let duration = Duration::decode(decoder)?;
		Ok(SystemTime::UNIX_EPOCH + duration)
	}
}

impl Decode for PathBuf {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(PathBuf::from(decoder.read_string()?))
	}
}

impl Decode for Box<Path> {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(PathBuf::decode(decoder)?.into_boxed_path())
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
	    impl $crate::Decode for std::num::$ty {
		    fn decode<T: std::io::Read>(decoder: &mut $crate::Encoder<T>) -> $crate::EncodingResult<Self> {
		        Ok(
			        Self::new(decoder.$read()?)
			        .ok_or($crate::EncodingError::ValidationError("Found a value of 0 while decoding a NonZero*".to_owned()))?
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

impl Decode for IpAddr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(match decoder.read_uvariant()? {
			0 => Self::V4(decoder.decode_value()?),
			1 => Self::V6(decoder.decode_value()?),
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

impl Decode for Ipv4Addr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(<Ipv4Addr as From<[u8; 4]>>::from(decoder.decode_value()?))
	}
}

impl Decode for Ipv6Addr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(<Ipv6Addr as From<[u8; 16]>>::from(decoder.decode_value()?))
	}
}

impl Decode for SocketAddr {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(match decoder.read_uvariant()? {
			0 => Self::V4(decoder.decode_value()?),
			1 => Self::V6(decoder.decode_value()?),
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

impl Decode for SocketAddrV4 {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(SocketAddrV4::new(Ipv4Addr::decode(decoder)?, u16::decode(decoder)?))
	}
}

impl Decode for SocketAddrV6 {
	fn decode<Reader: Read>(decoder: &mut Encoder<Reader>) -> EncodingResult<Self> {
		Ok(SocketAddrV6::new(
			Ipv6Addr::decode(decoder)?,
			u16::decode(decoder)?,
			u32::decode(decoder)?,
			u32::decode(decoder)?,
		))
	}
}