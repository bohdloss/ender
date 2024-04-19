//! The following file contains enums and functions copied from the
//! `ende` library. Any modifications done to the enums in the original library
//! must be ALWAYS reflected here.

use parse_display::Display;

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum Endianness {
	LittleEndian,
	BigEndian
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum NumEncoding {
	Fixed,
	Leb128,
	ProtobufWasteful,
	ProtobufZigzag,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum BitWidth {
	Bit8,
	Bit16,
	Bit32,
	Bit64,
	Bit128
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum StrEncoding {
	Ascii,
	Utf8,
	Utf16,
	Utf32,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum StrLenEncoding {
	LenPrefixed,
	NullTerminated,
}