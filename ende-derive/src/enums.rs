//! The following file contains enums and functions copied from the
//! `ende` library. Any modifications done to the enums in the original library
//! must be ALWAYS reflected here.

use std::fmt::Display;
use syn::Expr;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Endianness {
    LittleEndian,
    BigEndian,
}

impl Display for Endianness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Endianness::LittleEndian => "LittleEndian",
            Endianness::BigEndian => "BigEndian",
        }
        .to_owned();
        write!(f, "{}", str)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum NumEncoding {
    Fixed,
    Leb128,
    ProtobufWasteful,
    ProtobufZigzag,
}

impl Display for NumEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            NumEncoding::Fixed => "Fixed",
            NumEncoding::Leb128 => "Leb128",
            NumEncoding::ProtobufWasteful => "ProtobufWasteful",
            NumEncoding::ProtobufZigzag => "ProtobufZigzag",
        }
        .to_owned();
        write!(f, "{}", str)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum BitWidth {
    Bit8,
    Bit16,
    Bit32,
    Bit64,
    Bit128,
}

impl Display for BitWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            BitWidth::Bit8 => "Bit8",
            BitWidth::Bit16 => "Bit16",
            BitWidth::Bit32 => "Bit32",
            BitWidth::Bit64 => "Bit64",
            BitWidth::Bit128 => "Bit128",
        }
        .to_owned();
        write!(f, "{}", str)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum StrEncoding {
    Ascii,
    Utf8,
    Utf16,
    Utf32,
    Windows1252,
}

impl Display for StrEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            StrEncoding::Ascii => "Ascii",
            StrEncoding::Utf8 => "Utf8",
            StrEncoding::Utf16 => "Utf16",
            StrEncoding::Utf32 => "Utf32",
            StrEncoding::Windows1252 => "Windows1252",
        }
        .to_owned();
        write!(f, "{}", str)
    }
}

#[derive(Clone)]
pub enum StrLen {
    LengthPrefixed,
    NullTerminated,
    NullTerminatedFixed(Expr),
}
