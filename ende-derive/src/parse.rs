// Reasoning: many tokens are simply written into the structs and never read.
// Consider temporarily removing this if you intend to mess with the parser.
#![allow(unused)]

use crate::ctxt::Scope;
use crate::enums::BitWidth;
use parse_display::Display;
use proc_macro2::{Ident, Span};
use std::str::FromStr;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Paren;
use syn::{parenthesized, Error, Expr, LitInt, LitStr, Path, Token, Type, Lifetime};

const FLAGS_USAGE: &str = r#"Unknown Flag. Please refer to the documentation of the macro for a list of valid flags and their usage."#;

const MODIFIER_USAGE: &str = r#"Unknown modifier. Modifier can be bit-width (8, 16, 32, 64, 128), endianness (big_endian, little_endian), num-encoding (fixed, leb128), max-size (max = $expr)"#;

const BITWIDTH_USAGE: &str =
    r#"Invalid bit-width modifier. Allowed bit-widths are 8, 16, 32, 64, 128"#;

pub mod kw {
    use syn::custom_keyword;

    /* Conversions */
    custom_keyword!(into);

    /* Keywords used for flags */
    custom_keyword!(en);
    custom_keyword!(de);
    custom_keyword!(serde);
    custom_keyword!(skip);
    custom_keyword!(with);
    custom_keyword!(flatten);
    custom_keyword!(validate);
    custom_keyword!(borrow);

    /* Stream modifiers */
    custom_keyword!(redir);

    /* Flatten flag helpers */
    custom_keyword!(some);
    custom_keyword!(none);

    /* Keywords used for modifiers TARGETS */
    custom_keyword!(num);
    custom_keyword!(size);
    custom_keyword!(variant);
    custom_keyword!(string);

    /* Keywords used for the modifiers themselves */
    // Numerical encodings
    custom_keyword!(fixed);
    custom_keyword!(leb128);
    custom_keyword!(protobuf_wasteful);
    custom_keyword!(protobuf_zz);
    // Endianness
    custom_keyword!(big_endian);
    custom_keyword!(little_endian);
    // Max size
    custom_keyword!(max);
    // String encoding
    custom_keyword!(ascii);
    custom_keyword!(utf_8);
    custom_keyword!(utf_16);
    custom_keyword!(utf_32);
}

/// The `#[repr($ty)]` attribute
#[derive(Clone)]
pub struct ReprAttribute {
    pub ty: Type,
}

/// Represents a formatting, like `format!("A{}G", "BCDEF");`
#[derive(Clone)]
pub struct Formatting {
    pub format: LitStr,
    pub args: Option<(Token![,], Punctuated<Expr, Token![,]>)>,
}

#[derive(Clone)]
pub enum FlattenParam {
    Some(kw::some),
    None(kw::none),
    Expr(Expr),
}

/// Represents every kind of modifiers that can be applied to a [`ModTarget`].
/// Example: `$target: $modifier, $modifier, ...`
#[derive(Clone)]
pub enum Modifier {
    Fixed {
        kw: kw::fixed,
    },
    Leb128 {
        kw: kw::leb128,
    },
    ProtobufWasteful {
        kw: kw::protobuf_wasteful,
    },
    ProtobufZZ {
        kw: kw::protobuf_zz,
    },
    BigEndian {
        kw: kw::big_endian,
    },
    LittleEndian {
        kw: kw::little_endian,
    },
    Max {
        kw: kw::max,
        eq: Token![=],
        max: Expr,
    },
    BitWidth {
        lit: LitInt,
        width: BitWidth,
    },
    Ascii {
        kw: kw::ascii,
    },
    Utf8 {
        kw: kw::utf_8,
    },
    Utf16 {
        kw: kw::utf_16,
    },
    Utf32 {
        kw: kw::utf_32,
    },
}

/// Represents every possible target for a [`Modifier`].
#[derive(Clone, Display)]
pub enum ModTarget {
    #[display("num")]
    Num { kw: kw::num },
    #[display("size")]
    Size { kw: kw::size },
    #[display("variant")]
    Variant { kw: kw::variant },
    #[display("string")]
    String { kw: kw::string },
}

impl ModTarget {
    pub fn num(&self) -> bool {
        match self {
            Self::Num { .. } => true,
            _ => false,
        }
    }

    pub fn size(&self) -> bool {
        match self {
            Self::Size { .. } => true,
            _ => false,
        }
    }

    pub fn variant(&self) -> bool {
        match self {
            Self::Variant { .. } => true,
            _ => false,
        }
    }

    pub fn string(&self) -> bool {
        match self {
            Self::String { .. } => true,
            _ => false,
        }
    }

    fn peek(input: ParseStream) -> bool {
        input.peek(kw::num)
            || input.peek(kw::size)
            || input.peek(kw::variant)
            || input.peek(kw::string)
    }

    fn span(&self) -> Span {
        match self {
            ModTarget::Num { kw, .. } => kw.span,
            ModTarget::Size { kw, .. } => kw.span,
            ModTarget::Variant { kw, .. } => kw.span,
            ModTarget::String { kw, .. } => kw.span,
        }
    }
}

#[derive(Clone)]
pub struct FnArgs {
    pub paren: Paren,
    pub args: Punctuated<Expr, Token![,]>,
}

/// Every possible flag that can be attached to a field or item.
#[derive(Clone)]
pub enum Flag {
    /// All the following flags only apply to the encoding step
    En { kw: kw::en },
    /// All the following flags only apply to the decoding step
    De { kw: kw::de },
    /// Changes the name of the `ende` crate used in the derivation process
    Crate {
        kw: Token![crate],
        colon: Token![:],
        crate_name: Ident,
    },
    /// The field or item should be encoded/decoded using serde.
    Serde {
        kw: kw::serde,
        crate_name: Option<(Token![:], Ident)>,
    },
    /// The field or item shouldn't be encoded/decoded. During decoding, it will fall back to
    /// the default for that field (can be overridden by the `default` flag).
    Skip { kw: kw::skip },
    /// The field should only be encoded if the condition is true. If the condition is false
    /// while decoding, it will fall back to the default for that field
    /// (can be overridden by the `default` flag).
    If {
        kw: Token![if],
        colon: Token![:],
        expr: Expr,
    },
    /// The default value for a field that cannot be decoded for whatever reason is
    /// `Default::default()`. This flag allows changing that to whatever expression is specified.
    Default {
        kw: Token![default],
        colon: Token![:],
        expr: Expr,
    },
    /// The field should be encoded/decoded using the given function. If the Encode scope
    /// is specified, it must be callable as
    With {
        kw: kw::with,
        colon: Token![:],
        path: Path,
        args: Option<FnArgs>,
    },
    /// The field should be encoded/decoded as if it was of the given type. It should then be
    /// converted back to the appropriate type. The conversion method is the as keyword.
    As {
        kw: Token![as],
        colon: Token![:],
        ty: Type,
    },
    /// The field should be encoded/decoded as if it was of the given type. It should then be
    /// converted back to the appropriate type. The conversion methods are the From and Into traits.
    Into {
        kw: kw::into,
        colon: Token![:],
        ty: Type,
    },
    /// Changes the flatten state variable. See the documentation for `Encode` and `Decode` for
    /// more info.
    Flatten {
        kw: kw::flatten,
        colon: Token![:],
        param: FlattenParam,
    },
    /// Performs a check before encoding and after decoding a field. If the check fails, an error
    /// is returned. Allows specifying a custom error message with fmt arguments.
    Validate {
        kw: kw::validate,
        colon: Token![:],
        expr: Expr,
        fmt: Option<(Token![,], Formatting)>,
    },
    /// The field has a number of modifiers attached to it.
    Modifiers {
        target: ModTarget,
        colon: Token![:],
        modifiers: Punctuated<Modifier, Token![,]>,
    },
    /// A transform should be applied to the underlying stream while encoding/decoding this field.
    Redir {
        kw: kw::redir,
        colon: Token![:],
        path: Path,
        args: Option<FnArgs>,
    },
    /// The field must be borrowed from the encoder when deriving `BorrowDecode`
    Borrow {
        kw: kw::borrow,
        lifetimes: Option<(Token![:], Punctuated<Lifetime, Token![,]>)>,
    }
}

impl Flag {
    pub fn span(&self) -> Span {
        match self {
            Flag::En { kw, .. } => kw.span,
            Flag::De { kw, .. } => kw.span,
            Flag::Crate { kw, .. } => kw.span,
            Flag::Serde { kw, .. } => kw.span,
            Flag::Skip { kw, .. } => kw.span,
            Flag::If { kw, .. } => kw.span,
            Flag::Default { kw, .. } => kw.span,
            Flag::With { kw, .. } => kw.span,
            Flag::As { kw, .. } => kw.span,
            Flag::Into { kw, .. } => kw.span,
            Flag::Flatten { kw, .. } => kw.span,
            Flag::Validate { kw, .. } => kw.span,
            Flag::Modifiers { target, .. } => target.span(),
            Flag::Redir { kw, .. } => kw.span,
            Flag::Borrow { kw, .. } => kw.span,
        }
    }
}

/// Represents the `#[ende(... stuff ...)]` attribute
#[derive(Clone)]
pub struct EndeAttribute {
    pub flags: Punctuated<Flag, Token![;]>,
}

impl EndeAttribute {
    pub fn scope(&self) -> Scope {
        self.flags
            .iter()
            .nth(0)
            .map(|x| match x {
                Flag::En { .. } => Scope::Encode,
                Flag::De { .. } => Scope::Decode,
                _ => Scope::Both,
            })
            .unwrap_or(Scope::Both)
    }
}

/* ┌─────────────────────────────────────────────────────────────────────┐
** │The following section contains the parsing code for the above structs│
** └─────────────────────────────────────────────────────────────────────┘*/

impl Parse for ReprAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self { ty: input.parse()? })
    }
}

impl Parse for Formatting {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            format: input.parse()?,
            args: if input.peek(Token![,]) {
                Some((input.parse()?, Punctuated::parse_separated_nonempty(input)?))
            } else {
                None
            },
        })
    }
}

impl Parse for FlattenParam {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::some) {
            Ok(Self::Some(input.parse()?))
        } else if input.peek(kw::none) {
            Ok(Self::None(input.parse()?))
        } else {
            Ok(Self::Expr(input.parse()?))
        }
    }
}

impl Parse for Modifier {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::fixed) {
            Ok(Self::Fixed { kw: input.parse()? })
        } else if input.peek(kw::leb128) {
            Ok(Self::Leb128 { kw: input.parse()? })
        } else if input.peek(kw::protobuf_wasteful) {
            Ok(Self::ProtobufWasteful { kw: input.parse()? })
        } else if input.peek(kw::protobuf_zz) {
            Ok(Self::ProtobufZZ { kw: input.parse()? })
        } else if input.peek(kw::big_endian) {
            Ok(Self::BigEndian { kw: input.parse()? })
        } else if input.peek(kw::little_endian) {
            Ok(Self::LittleEndian { kw: input.parse()? })
        } else if input.peek(kw::max) {
            Ok(Self::Max {
                kw: input.parse()?,
                eq: input.parse()?,
                max: input.parse()?,
            })
        } else if input.peek(LitInt) {
            let lit: LitInt = input.parse()?;
            let width = match lit.base10_parse::<u8>()? {
                8 => BitWidth::Bit8,
                16 => BitWidth::Bit16,
                32 => BitWidth::Bit32,
                64 => BitWidth::Bit64,
                128 => BitWidth::Bit128,
                _ => return Err(Error::new(lit.span(), BITWIDTH_USAGE)),
            };

            Ok(Self::BitWidth { lit, width })
        } else if input.peek(kw::ascii) {
            Ok(Self::Ascii { kw: input.parse()? })
        } else if input.peek(kw::utf_8) {
            Ok(Self::Utf8 { kw: input.parse()? })
        } else if input.peek(kw::utf_16) {
            Ok(Self::Utf16 { kw: input.parse()? })
        } else if input.peek(kw::utf_32) {
            Ok(Self::Utf32 { kw: input.parse()? })
        } else {
            Err(Error::new(input.span(), MODIFIER_USAGE))
        }
    }
}

impl Parse for ModTarget {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::num) {
            Ok(Self::Num { kw: input.parse()? })
        } else if input.peek(kw::size) {
            Ok(Self::Size { kw: input.parse()? })
        } else if input.peek(kw::variant) {
            Ok(Self::Variant { kw: input.parse()? })
        } else if input.peek(kw::string) {
            Ok(Self::String { kw: input.parse()? })
        } else {
            // We peek before parsing a Target, so this should be unreachable
            unreachable!()
        }
    }
}

impl Parse for FnArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inside;
        Ok(Self {
            paren: parenthesized!(inside in input),
            args: Punctuated::parse_terminated(&inside)?,
        })
    }
}

impl Parse for Flag {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::en) {
            Ok(Self::En { kw: input.parse()? })
        } else if input.peek(kw::de) {
            Ok(Self::De { kw: input.parse()? })
        } else if input.peek(kw::serde) {
            Ok(Self::Serde {
                kw: input.parse()?,
                crate_name: if input.peek(Token![:]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                },
            })
        } else if input.peek(Token![crate]) {
            Ok(Self::Crate {
                kw: input.parse()?,
                colon: input.parse()?,
                crate_name: input.parse()?,
            })
        } else if input.peek(kw::skip) {
            Ok(Self::Skip { kw: input.parse()? })
        } else if input.peek(Token![if]) {
            Ok(Self::If {
                kw: input.parse()?,
                colon: input.parse()?,
                expr: input.parse()?,
            })
        } else if input.peek(Token![default]) {
            Ok(Self::Default {
                kw: input.parse()?,
                colon: input.parse()?,
                expr: input.parse()?,
            })
        } else if input.peek(kw::with) {
            Ok(Self::With {
                kw: input.parse()?,
                colon: input.parse()?,
                path: input.parse()?,
                args: if input.peek(Paren) {
                    Some(input.parse()?)
                } else {
                    None
                },
            })
        } else if input.peek(Token![as]) {
            Ok(Self::As {
                kw: input.parse()?,
                colon: input.parse()?,
                ty: input.parse()?,
            })
        } else if input.peek(kw::into) {
            Ok(Self::Into {
                kw: input.parse()?,
                colon: input.parse()?,
                ty: input.parse()?,
            })
        } else if input.peek(kw::flatten) {
            Ok(Self::Flatten {
                kw: input.parse()?,
                colon: input.parse()?,
                param: input.parse()?,
            })
        } else if input.peek(kw::validate) {
            Ok(Self::Validate {
                kw: input.parse()?,
                colon: input.parse()?,
                expr: input.parse()?,
                fmt: if input.peek(Token![,]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                },
            })
        } else if ModTarget::peek(input) {
            Ok(Self::Modifiers {
                target: input.parse()?,
                colon: input.parse()?,
                modifiers: Punctuated::parse_separated_nonempty(input)?,
            })
        } else if input.peek(kw::redir) {
            Ok(Self::Redir {
                kw: input.parse()?,
                colon: input.parse()?,
                path: input.parse()?,
                args: if input.peek(Paren) {
                    Some(input.parse()?)
                } else {
                    None
                },
            })
        } else if input.peek(kw::borrow) {
            Ok(Self::Borrow {
                kw: input.parse()?,
                lifetimes: if input.peek(Token![:]) {
                    Some((input.parse()?, Punctuated::parse_terminated(input)?))
                } else {
                    None
                }
            })
        } else {
            Err(Error::new(input.span(), FLAGS_USAGE))
        }
    }
}

impl Parse for EndeAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            flags: Punctuated::parse_terminated(input)?,
        })
    }
}
