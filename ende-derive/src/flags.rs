use proc_macro2::Ident;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::{parse_quote, Error, Expr, Lifetime, Path, Type};

use crate::ctxt::Scope;
use crate::enums::{BitWidth, Endianness, NumEncoding, StrEncoding};
use crate::parse::{Flag, FlattenTarget, Formatting, ModTarget, Modifier, SeekTarget};
use crate::{dollar_crate, ENDE};

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum FlagTarget {
    Item,
    Field,
}

#[derive(Clone)]
pub enum Param<T> {
    Default(T),
    Other(T),
}

impl<T> Param<T> {
    pub fn is_default(&self) -> bool {
        match self {
            Param::Default(_) => true,
            _ => false,
        }
    }
}

impl<T: ToTokens> ToTokens for Param<T> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Param::Default(x) => x.to_tokens(tokens),
            Param::Other(x) => x.to_tokens(tokens),
        }
    }
}

#[derive(Clone)]
pub enum Function {
    Default,
    Serde(Ident),
    With(Path, Vec<Expr>, Scope),
}

impl Function {
    pub fn is_default(&self) -> bool {
        match self {
            Self::Default => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub enum TypeModifier {
    As(Type),
    Into(Type),
}

impl TypeModifier {
    pub fn ty(&self) -> &Type {
        match self {
            TypeModifier::As(ty) => ty,
            TypeModifier::Into(ty) => ty,
        }
    }
}

/// Represents all the possible modifiers a target can have. Use the apply() method to
/// have all the modifiers validated
#[derive(Clone)]
pub struct ModifierGroup {
    pub target: ModTarget,
    pub num_encoding: Option<NumEncoding>,
    pub endianness: Option<Endianness>,
    pub max: Option<Expr>,
    pub bit_width: Option<BitWidth>,
    pub str_encoding: Option<StrEncoding>,
}

impl ModifierGroup {
    pub fn new(target: ModTarget) -> Self {
        Self {
            target,
            num_encoding: None,
            endianness: None,
            max: None,
            bit_width: None,
            str_encoding: None,
        }
    }

    pub fn empty(&self) -> bool {
        self.num_encoding.is_none()
            && self.endianness.is_none()
            && self.max.is_none()
            && self.bit_width.is_none()
    }

    pub fn apply(&mut self, modifier: Modifier) -> syn::Result<()> {
        const REPEATED_NUM_ENCODING: &str =
            "Num encoding modifier declared twice for the same target";
        const REPEATED_ENDIANNESS: &str = "Endianness modifier declared twice for the same target";
        const REPEATED_MAX: &str = "Max size modifier declared twice for the same target";
        const REPEATED_BIT_WIDTH: &str = "Bit width modifier declared twice for the same target";
        const REPEATED_STR_ENCODING: &str =
            "String encoding modifier declared twice for the same target";

        const NOT_STRING: &str = r#"This modifier can't be applied to the "string" target"#;
        const ONLY_STRING: &str = r#"This modifier can only be applied to the "string" target"#;
        const ONLY_SIZE: &str = r#"This modifier can only be applied to the "size" target"#;
        const ONLY_VARIANT_AND_SIZE: &str =
            r#"This modifier can only be applied to the "size" and "variant" targets"#;

        match modifier {
            Modifier::Fixed { kw, .. } => {
                if self.target.string() {
                    return Err(Error::new(kw.span(), NOT_STRING));
                }
                if self.num_encoding.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_NUM_ENCODING));
                }

                self.num_encoding = Some(NumEncoding::Fixed);
            }
            Modifier::Leb128 { kw, .. } => {
                if self.target.string() {
                    return Err(Error::new(kw.span(), NOT_STRING));
                }
                if self.num_encoding.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_NUM_ENCODING));
                }

                self.num_encoding = Some(NumEncoding::Leb128);
            }
            Modifier::ProtobufWasteful { kw, .. } => {
                if self.target.string() {
                    return Err(Error::new(kw.span(), NOT_STRING));
                }
                if self.num_encoding.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_NUM_ENCODING));
                }

                self.num_encoding = Some(NumEncoding::ProtobufWasteful);
            }
            Modifier::ProtobufZZ { kw, .. } => {
                if self.target.string() {
                    return Err(Error::new(kw.span(), NOT_STRING));
                }
                if self.num_encoding.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_NUM_ENCODING));
                }

                self.num_encoding = Some(NumEncoding::ProtobufZigzag);
            }
            Modifier::BigEndian { kw, .. } => {
                if self.endianness.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_ENDIANNESS));
                }

                self.endianness = Some(Endianness::BigEndian);
            }
            Modifier::LittleEndian { kw, .. } => {
                if self.endianness.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_ENDIANNESS));
                }

                self.endianness = Some(Endianness::LittleEndian);
            }
            Modifier::Max { kw, max, .. } => {
                if !self.target.size() {
                    return Err(Error::new(kw.span(), ONLY_SIZE));
                }
                if self.max.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_MAX));
                }

                self.max = Some(max);
            }
            Modifier::BitWidth { span, width, .. } => {
                if !self.target.variant() && !self.target.size() {
                    return Err(Error::new(span, ONLY_VARIANT_AND_SIZE));
                }
                if self.bit_width.is_some() {
                    return Err(Error::new(span, REPEATED_BIT_WIDTH));
                }

                self.bit_width = Some(width);
            }
            Modifier::Utf8 { kw, .. } => {
                if !self.target.string() {
                    return Err(Error::new(kw.span(), ONLY_STRING));
                }
                if self.str_encoding.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_STR_ENCODING));
                }

                self.str_encoding = Some(StrEncoding::Utf8);
            }
            Modifier::Utf16 { kw, .. } => {
                if !self.target.string() {
                    return Err(Error::new(kw.span(), ONLY_STRING));
                }
                if self.str_encoding.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_STR_ENCODING));
                }

                self.str_encoding = Some(StrEncoding::Utf16);
            }
            Modifier::Utf32 { kw, .. } => {
                if !self.target.string() {
                    return Err(Error::new(kw.span(), ONLY_STRING));
                }
                if self.str_encoding.is_some() {
                    return Err(Error::new(kw.span(), REPEATED_STR_ENCODING));
                }

                self.str_encoding = Some(StrEncoding::Utf32);
            }
        }
        Ok(())
    }
}

/// Holds modifiers for each target
#[derive(Clone)]
pub struct AllModifiers {
    pub num: ModifierGroup,
    pub size: ModifierGroup,
    pub variant: ModifierGroup,
    pub string: ModifierGroup,
    pub bool_flatten: Option<Expr>,
    pub variant_flatten: Option<Expr>,
    pub size_flatten: Option<Expr>,
}

impl AllModifiers {
    pub fn new() -> Self {
        Self {
            num: ModifierGroup::new(ModTarget::Num {
                kw: Default::default(),
            }),
            size: ModifierGroup::new(ModTarget::Size {
                kw: Default::default(),
            }),
            variant: ModifierGroup::new(ModTarget::Variant {
                kw: Default::default(),
            }),
            string: ModifierGroup::new(ModTarget::String {
                kw: Default::default(),
            }),
            bool_flatten: None,
            variant_flatten: None,
            size_flatten: None,
        }
    }

    pub fn empty(&self) -> bool {
        self.num.empty()
            && self.size.empty()
            && self.variant.empty()
            && self.bool_flatten.is_none()
            && self.variant_flatten.is_none()
            && self.size_flatten.is_none()
    }

    pub fn apply(&mut self, target: ModTarget, modifier: Modifier) -> syn::Result<()> {
        match target {
            ModTarget::Num { .. } => {
                self.num.target = target;
                self.num.apply(modifier)
            }
            ModTarget::Size { .. } => {
                self.size.target = target;
                self.size.apply(modifier)
            }
            ModTarget::Variant { .. } => {
                self.variant.target = target;
                self.variant.apply(modifier)
            }
            ModTarget::String { .. } => {
                self.string.target = target;
                self.string.apply(modifier)
            }
        }
    }
}

/// A stream modifier - refer to the docs for the Encode and Decode derive macros
#[derive(Clone)]
pub enum StreamModifier {
    Transform {
        path: Path,
        args: Vec<Expr>,
        scope: Scope,
    },
    Ptr {
        seek: SeekParam,
    },
}

impl StreamModifier {
    pub fn is_ptr(&self) -> bool {
        match self {
            Self::Ptr { .. } => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct SeekParam {
    pub target: SeekTarget,
    pub seek: Expr,
}

/// All the possible flags a field or item can have. The target allows the apply method to
/// check whether each flag is supported.
#[derive(Clone)]
pub struct Flags {
    /// Whether this is an item or field
    pub target: FlagTarget,
    /// The name of the crate - ende by default
    pub crate_name: Param<Ident>,
    /// Only set when the "skip" flag is specified. Will generate empty Encode and Decode
    /// implementations. Can only be accompanied by the "default" flag.
    pub skip: bool,
    /// The fallback used whenever a field or item can't be parsed for whatever reason
    pub default: Param<Expr>,
    /// The function used for encoding / decoding.
    pub function: Function,
    /// The type modifiers of this item.
    pub ty_mods: Option<TypeModifier>,
    /// The modifiers to apply to this item
    pub mods: AllModifiers,
    /// If present, indicates the item should be validated using the given expression before
    /// encoding and after decoding.
    pub validate: Option<(Expr, Option<Formatting>)>,
    /// If present indicates a field should only be encoded or decoded if the given condition
    /// is true. During decoding, if the condition is false, the default value will be used
    pub condition: Option<Expr>,
    /// Modifiers to the underlying Write/Read object itself. Indicate something should be
    /// encrypted or compressed before being encoded or decoded.
    pub stream_modifiers: Vec<StreamModifier>,
    /// The field requires to be borrowed for these lifetimes, possibly introducing further
    /// bounds in the derived impl.
    pub borrow: Option<Vec<Lifetime>>,
    /// Before encoding this field seek to the given offset, and don't come back (affects
    /// all the following fields)
    pub seek: Option<SeekParam>,
}

impl Flags {
    pub fn new(target: FlagTarget) -> Self {
        Self {
            target,
            crate_name: Param::Default(dollar_crate(ENDE)),
            skip: false,
            default: Param::Default(parse_quote!(Default::default())),
            function: Function::Default,
            ty_mods: None,
            mods: AllModifiers::new(),
            validate: None,
            condition: None,
            stream_modifiers: Vec::new(),
            borrow: None,
            seek: None,
        }
    }

    pub fn requires_seeking_impl(&self) -> bool {
        self.seek.is_some() || self.stream_modifiers.iter().any(StreamModifier::is_ptr)
    }

    pub fn skip_compatible(&self) -> bool {
        self.function.is_default()
            && self.mods.empty()
            && self.condition.is_none()
            && self.stream_modifiers.is_empty()
            && self.ty_mods.is_none()
            && self.borrow.is_none()
            && self.seek.is_none()
    }
}

impl Flags {
    /// Applies a flag to the item, performing consistency checks.
    pub fn apply(&mut self, flag: Flag, scope: Scope) -> syn::Result<()> {
        const MULTIPLE_FUNCTION_MODS: &str = "Multiple function-modifier flags declared";
        const MULTIPLE_TY_MODS: &str = "Multiple type-modifier flags declared";

        let span = flag.span();
        match flag {
            Flag::Crate { crate_name, .. } => {
                if self.target == FlagTarget::Field {
                    return Err(Error::new(
                        span,
                        r#""crate" flag can only be applied at the item level"#,
                    ));
                }

                if !self.crate_name.is_default() {
                    return Err(Error::new(span, r#""crate" flag declared more than once"#));
                }

                self.crate_name = Param::Other(crate_name);
            }
            Flag::Serde { crate_name, .. } => {
                if !self.function.is_default() {
                    return Err(Error::new(span, MULTIPLE_FUNCTION_MODS));
                }

                // If no name is specified, it is assumed to be "serde"
                let crate_name = crate_name
                    .map(|(_, x)| x)
                    .unwrap_or(Ident::new("serde", span));

                self.function = Function::Serde(crate_name);
            }
            Flag::Skip { .. } => {
                if self.skip {
                    return Err(Error::new(span, r#""skip" flag declared more than once"#));
                }

                self.skip = true;
            }
            Flag::Default { expr, .. } => {
                if !self.default.is_default() {
                    return Err(Error::new(
                        span,
                        r#""default" flag declared more than once"#,
                    ));
                }

                self.default = Param::Other(expr);
            }
            Flag::With { path, args, .. } => {
                if !self.function.is_default() {
                    return Err(Error::new(span, MULTIPLE_FUNCTION_MODS));
                }

                let args = args
                    .map(|x| x.args.into_iter().collect::<Vec<Expr>>())
                    .unwrap_or(Vec::new());

                self.function = Function::With(path, args, scope);
            }
            Flag::As { ty, .. } => {
                if self.ty_mods.is_some() {
                    return Err(Error::new(span, MULTIPLE_TY_MODS));
                }

                self.ty_mods = Some(TypeModifier::As(ty));
            }
            Flag::Into { ty, .. } => {
                if self.ty_mods.is_some() {
                    return Err(Error::new(span, MULTIPLE_TY_MODS));
                }

                self.ty_mods = Some(TypeModifier::Into(ty));
            }
            Flag::Flatten { target, param, .. } => match target {
                FlattenTarget::Bool { .. } => {
                    if self.mods.bool_flatten.is_some() {
                        return Err(Error::new(
                            span,
                            r#""flatten" flag declared more than once"#,
                        ));
                    }

                    let expr = parse_quote!(#param);
                    self.mods.bool_flatten = Some(expr);
                }
                FlattenTarget::Variant { .. } => {
                    if self.mods.variant_flatten.is_some() {
                        return Err(Error::new(
                            span,
                            r#""flatten" flag declared more than once"#,
                        ));
                    }

                    let expr = parse_quote!(#param);
                    self.mods.variant_flatten = Some(expr);
                }
                FlattenTarget::Size { .. } => {
                    if self.mods.size_flatten.is_some() {
                        return Err(Error::new(
                            span,
                            r#""flatten" flag declared more than once"#,
                        ));
                    }

                    let expr = parse_quote!(#param);
                    self.mods.size_flatten = Some(expr);
                }
            },
            Flag::Validate { expr, fmt, .. } => {
                if self.validate.is_some() {
                    return Err(Error::new(
                        span,
                        r#""validate" flag declared more than once"#,
                    ));
                }

                self.validate = Some((expr, fmt.map(|x| x.1)));
            }
            Flag::Modifiers {
                target, modifiers, ..
            } => {
                for modifier in modifiers {
                    self.mods.apply(target.clone(), modifier)?;
                }
            }
            Flag::If { expr, .. } => {
                if self.target == FlagTarget::Item {
                    return Err(Error::new(
                        span,
                        r#""if" flag can only be applied to fields"#,
                    ));
                }

                if self.condition.is_some() {
                    return Err(Error::new(span, r#""if" flag declared more than once"#));
                }

                self.condition = Some(expr);
            }
            Flag::Redir { path, args, .. } => {
                let args = args
                    .map(|x| x.args.into_iter().collect::<Vec<Expr>>())
                    .unwrap_or(Vec::new());

                self.stream_modifiers
                    .push(StreamModifier::Transform { path, args, scope })
            }
            Flag::Ptr { target, seek, .. } => {
                if self.stream_modifiers.iter().any(StreamModifier::is_ptr) {
                    return Err(Error::new(span, r#""ptr" flag declared more than once"#));
                }

                self.stream_modifiers.push(StreamModifier::Ptr {
                    seek: SeekParam { target, seek },
                })
            }
            Flag::En { .. } | Flag::De { .. } => {
                return Err(Error::new(
                    span,
                    r#"The flags "en" and "de" must be the first"#,
                ))
            }
            Flag::Borrow { lifetimes, .. } => {
                /* Lifetime bounds are solved in a later pass */
                /* Here we simply track the flag if it is explicitly passed */
                /* And error when it is duplicated */

                if self.borrow.is_some() {
                    return Err(Error::new(span, r#""borrow" flag declared more than once"#));
                }

                if let Some(lifetimes) = lifetimes {
                    self.borrow = Some(lifetimes.1.into_iter().collect());
                } else {
                    self.borrow = Some(Vec::new());
                }
            }
            Flag::Goto { target, seek, .. } => {
                if self.seek.is_some() {
                    return Err(Error::new(span, r#""goto" flag declared more than once"#));
                }

                self.seek = Some(SeekParam { target, seek })
            }
        }

        if self.skip && !self.skip_compatible() {
            return Err(Error::new(
                span,
                r#""skip" flag can only be accompanied by "default" or "validate" flags"#,
            ));
        }

        Ok(())
    }
}
