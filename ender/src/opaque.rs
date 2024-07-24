use crate::{BitWidth, EncodingError, Signedness};
use parse_display::Display;

trait Sealed {}

pub(crate) enum Signed {}
pub(crate) enum Unsigned {}

#[allow(private_bounds)]
pub(crate) trait Sign: Sealed {
    type Sign;
}

macro_rules! sign_impl {
    ($($ty:ty => $ret:ident);* $(;)?) => {
	    $(
	    impl Sealed for $ty {}
	    impl Sign for $ty {
		    type Sign = $ret;
	    }
	    )*
    };
}

sign_impl! {
    u8 => Unsigned;
    u16 => Unsigned;
    u32 => Unsigned;
    u64 => Unsigned;
    u128 => Unsigned;
    usize => Unsigned;
    i8 => Signed;
    i16 => Signed;
    i32 => Signed;
    i64 => Signed;
    i128 => Signed;
    isize => Signed;
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[non_exhaustive]
enum OpaqueInner {
    #[display("U-{0}")]
    Unsigned(u128),
    #[display("I-{0}")]
    Signed(i128),
}

/// An opaque integer type, used to represent enum variants,
/// `usize` and `isize`.
///
/// Supports converting to and from all integer primitive types.
///
/// Conversion errors where the value wouldn't fit in the requested
/// type, are coerced to [`EncodingError`]s
/// 
/// **Beware of providing an [`Opaque`] with an integer literal without a specific type!**
/// 
/// By default, rust will infer `i32` as the type, thus it will be converted to a *Signed*
/// enum variant value, and you will get a (bad) surprise when you try to then encode/decode
/// an enum that uses *Unsigned* variant discriminants (most enums).
/// 
/// How to avoid this?
/// - Write the type in the num literal E.G. `Opaque::from(3u32)` or `Opaque::from(3 as u32)`
/// - Better yet, use an explicit opaque value ([`Opaque::signed`], [`Opaque::unsigned`])
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
#[repr(transparent)]
pub struct Opaque(OpaqueInner);

impl Opaque {
	/// Constructs a new opaque integer, enforcing at compile time that the number type is **signed**
	#[allow(private_bounds)]
	#[inline]
	pub fn signed<V>(value: V) -> Self
	where V: Sign<Sign = Signed>,
	      Self: From<V>
	{
		Self::from(value)
	}

	/// Constructs a new opaque integer, enforcing at compile time that the number type is **unsigned**
	#[allow(private_bounds)]
	#[inline]
	pub fn unsigned<V>(value: V) -> Self
	where V: Sign<Sign = Unsigned>,
	      Self: From<V>
	{
		Self::from(value)
	}
	
    /// Returns the [`Signedness`] of the opaque integer.
    #[inline]
    pub fn sign(&self) -> Signedness {
        match self.0 {
            OpaqueInner::Unsigned(_) => Signedness::Unsigned,
            OpaqueInner::Signed(_) => Signedness::Signed,
        }
    }
}

/* Int-to-Variant conversions always succeed */

macro_rules! from_impl {
    ($($ty:ty => $var:ident);* $(;)?) => {
	    $(
	    impl From<$ty> for Opaque {
			#[inline]
			fn from(value: $ty) -> Self {
				Self(OpaqueInner::$var(value as _))
			}
		}
	    )*
    };
}

from_impl! {
    u8 => Unsigned;
    u16 => Unsigned;
    u32 => Unsigned;
    u64 => Unsigned;
    u128 => Unsigned;
    usize => Unsigned;
    i8 => Signed;
    i16 => Signed;
    i32 => Signed;
    i64 => Signed;
    i128 => Signed;
    isize => Signed;
}

/* Variant-to-Int conversions are fallible, because the integer might not fit */

macro_rules! into_impl {
    ($($($width:tt)::* => $ty:ty => $var:ident != $isnt:ident);* $(;)?) => {
	    $(
	    impl TryInto<$ty> for Opaque {
			type Error = EncodingError;
			#[inline]
			fn try_into(self) -> Result<$ty, Self::Error> {
				match self.0 {
					OpaqueInner::$var(x)  => {
						x.try_into().map_err(|_| EncodingError::TooLarge {
							value: self,
							requested_width: BitWidth::$($width)*,
						})
					},
					OpaqueInner::$isnt(_) => Err(EncodingError::SignMismatch {
						expected: Signedness::$var,
						got: Signedness::$isnt,
					}),
				}
			}
		}
	    )*
    };
}

into_impl! {
    Bit8 => u8 => Unsigned != Signed;
    Bit16 => u16 => Unsigned != Signed;
    Bit32 => u32 => Unsigned != Signed;
    Bit64 => u64 => Unsigned != Signed;
    Bit128 => u128 => Unsigned != Signed;
    native::() => usize => Unsigned != Signed;
    Bit8 => i8 => Signed != Unsigned;
    Bit16 => i16 => Signed != Unsigned;
    Bit32 => i32 => Signed != Unsigned;
    Bit64 => i64 => Signed != Unsigned;
    Bit128 => i128 => Signed != Unsigned;
    native::() => isize => Signed != Unsigned;
}
