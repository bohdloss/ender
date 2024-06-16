use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Ident, Span};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::fmt::Display;
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, Data, DeriveInput, Error, Expr, Fields, Generics, Index, Lifetime, Type,
};

use crate::flags::{FlagTarget, Flags, TypeModifier};
use crate::lifetime::process_field_lifetimes;
use crate::parse::{EndeAttribute, Flag, ReprAttribute};

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum EnumRepr {
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
}

impl EnumRepr {
    pub fn signed(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::I128 | Self::ISize => true,
            _ => false,
        }
    }
}

impl TryFrom<&Type> for EnumRepr {
    type Error = Error;
    fn try_from(value: &Type) -> Result<Self, Self::Error> {
        let string = &value.to_token_stream().to_string() as &str;
        Ok(match string {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "u128" => Self::U128,
            "usize" => Self::USize,
            "i8" => Self::I8,
            "i16" => Self::I16,
            "i32" => Self::I32,
            "i64" => Self::I64,
            "i128" => Self::I128,
            "isize" => Self::ISize,
            // When this happens, the underlying code would fail to compile regardless
            _ => return Err(Error::new(value.span(), "Invalid repr attribute")),
        })
    }
}

impl ToTokens for EnumRepr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(match self {
            Self::U8 => quote!(u8),
            Self::U16 => quote!(u16),
            Self::U32 => quote!(u32),
            Self::U64 => quote!(u64),
            Self::U128 => quote!(u128),
            Self::USize => quote!(usize),
            Self::I8 => quote!(i8),
            Self::I16 => quote!(i16),
            Self::I32 => quote!(i32),
            Self::I64 => quote!(i64),
            Self::I128 => quote!(i128),
            Self::ISize => quote!(isize),
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ItemType {
    Struct,
    Enum,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Flavor {
    Unit,
    Tuple,
    Struct,
}

#[derive(Clone)]
pub struct Struct {
    pub flavor: Flavor,
    pub fields: Vec<Field>,
}

#[derive(Clone)]
pub struct Variant {
    pub name: Ident,
    pub index: VariantIndex,
    pub flavor: Flavor,
    pub fields: Vec<Field>,
    pub flags: Flags,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Target {
    Encode,
    Decode,
    BorrowDecode,
}

impl Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Target::Encode => "Encode",
            Target::Decode => "Decode",
            Target::BorrowDecode => "BorrowDecode",
        }
        .to_owned();
        write!(f, "{}", str)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Scope {
    Encode,
    Decode,
    Both,
}

impl Scope {
    fn matches(&self, target: Target) -> bool {
        match self {
            Scope::Encode => target == Target::Encode,
            Scope::Decode => target == Target::Decode || target == Target::BorrowDecode,
            Scope::Both => true,
        }
    }
}

#[derive(Clone)]
pub struct Field {
    /// The accessor for this field. For named fields, it is equal to its name.
    /// For unnamed fields, it is equal to its index.
    pub accessor: TokenStream2,
    /// The field name for named fields, but for unnamed fields it is its index prefixed by 'm'.
    pub name: Ident,
    /// The type of the field.
    pub ty: Type,
    /// The flags of the field.
    pub flags: Flags,
}

impl Field {
    pub fn new(accessor: TokenStream2, name: Ident, ty: Type) -> Self {
        Self {
            accessor,
            name,
            ty,
            flags: Flags::new(FlagTarget::Field),
        }
    }

    /// Returns the type this field will be encoded to / decoded from.
    /// In practice, this means if any type modifier flag is present,
    /// its type is returned, otherwise the original field type is returned.
    pub fn virtual_ty(&self) -> &Type {
        self.flags
            .ty_mods
            .as_ref()
            .map(TypeModifier::ty)
            .unwrap_or(&self.ty)
    }
}

#[derive(Clone)]
pub struct VariantIndex {
    pub idx: usize,
    pub base_ident: Ident,
    pub ident: Ident,
    pub expr: Expr,
}

impl VariantIndex {
    /// The first or zero-index variant
    pub fn zero() -> Self {
        let base_ident = Ident::new("__INDEX", Span::call_site());
        Self {
            idx: 0,
            ident: format_ident!("{}_{}", base_ident, 0usize),
            base_ident,
            expr: parse_quote!(0),
        }
    }

    /// Replaces the expression
    pub fn value(&mut self, value: &Expr) {
        self.expr = syn::parse2(quote!( (#value) )).unwrap();
    }

    /// Increments the index of the variant, as well as the expression
    pub fn increment(&mut self) {
        let ref expr = self.expr;
        self.idx += 1;
        self.ident = format_ident!("{}_{}", self.base_ident, self.idx);
        self.expr = parse_quote!( (#expr + 1) );
    }
}

pub struct FlagGroup {
    pub scope: Scope,
    pub flags: Vec<Flag>,
}

impl FlagGroup {
    /// Extracts a list of [`Flag`] from a list of attributes
    pub fn from_attrs(attrs: &[Attribute]) -> syn::Result<Vec<Self>> {
        let mut groups = Vec::new();

        for attr in attrs.iter().filter(|x| x.meta.path().is_ident("ende")) {
            let ende_attr: EndeAttribute = attr.parse_args()?;
            let scope = ende_attr.scope();

            // If the scope is not "Both", either "en" or "de" was specified, we need to remove them
            let flags: Vec<Flag> = if scope == Scope::Both {
                ende_attr.flags.into_iter().collect()
            } else {
                ende_attr.flags.into_iter().skip(1).collect()
            };

            groups.push(FlagGroup { scope, flags })
        }

        Ok(groups)
    }
}

/// Holds information about the lifetimes being borrowed when deriving `BorrowDecode`
pub struct BorrowData {
    pub decoder: Lifetime,
    pub sub_lifetimes: Vec<Lifetime>,
}

/// Holds information about the item we are deriving the implementation for and its fields
pub struct Ctxt {
    /// Additional flags applied to the item
    pub flags: Flags,
    /// The derive target - Encode or Decode
    pub target: Target,
    /// The name of the encoder - __encoder of __decoder
    pub encoder: Ident,
    /// The name of the encoder generic parameter - currently __T
    pub encoder_generic: Ident,
    /// Generics for impl block
    pub generics: Generics,
    /// The name of the item
    pub item_name: Ident,
    /// The item type
    pub item_type: ItemType,
    /// The data for every variant. Empty for structs.
    pub variants: Vec<Variant>,
    /// Either signed or unsigned, indicates how the variant discriminant should be encoded
    pub enum_repr: EnumRepr,
    /// Struct related data. Empty unless ItemType is Struct
    pub struct_data: Struct,
    /// Lifetime data. Empty unless `target` is `BorrowDecode`
    pub borrow_data: BorrowData,
}

impl Ctxt {
    pub fn parse_from(input: &DeriveInput, target: Target) -> syn::Result<Self> {
        // Find the fields, item type and flavor
        let mut enum_repr = EnumRepr::USize;
        let mut struct_data = Struct {
            flavor: Flavor::Unit,
            fields: Vec::new(),
        };
        let mut variants = Vec::new();

        let mut lifetimes = Vec::new();

        let item_type = match &input.data {
            Data::Struct(data) => {
                // Extract the fields
                let (flavor, mut fields) = extract_fields_and_flavor(&data.fields, target)?;
                process_field_lifetimes(target, &mut fields, &mut lifetimes)?;

                struct_data = Struct { flavor, fields };

                ItemType::Struct
            }
            Data::Enum(data) => {
                // Find the variant repr attribute and try to parse it
                let repr: Option<syn::Result<ReprAttribute>> = input
                    .attrs
                    .iter()
                    .find(|x| x.meta.path().is_ident("repr"))
                    .map(|x| x.parse_args());

                if let Some(repr) = repr {
                    enum_repr = EnumRepr::try_from(&repr?.ty)?;
                }

                // Construct a list of variants.
                // In this step we also compute the values associated with each variant
                let mut variant_index = VariantIndex::zero();
                for variant in data.variants.iter() {
                    // Extract the variant fields and flavor
                    let (flavor, mut fields) = extract_fields_and_flavor(&variant.fields, target)?;
                    process_field_lifetimes(target, &mut fields, &mut lifetimes)?;

                    if let Some((_, discriminant)) = &variant.discriminant {
                        variant_index.value(discriminant);
                    }

                    // Register it
                    variants.push(Variant {
                        name: variant.ident.clone(),
                        index: variant_index.clone(),
                        flavor,
                        fields,
                        flags: Flags::new(FlagTarget::Variant),
                    });

                    // Increment the variant after each iteration
                    variant_index.increment();
                }

                ItemType::Enum
            }
            Data::Union(data) => {
                return Err(Error::new(
                    data.union_token.span(),
                    format!("{} cannot be derived on unions", target),
                ))
            }
        };

        // Deduplicate lifetime bounds
        if target == Target::BorrowDecode {
            lifetimes.sort();
            lifetimes.dedup();
        }

        // Finally construct a context
        let mut ctxt = Ctxt {
            flags: Flags::new(FlagTarget::Item),
            target,
            encoder: match target {
                Target::Encode => Ident::new("__encoder", Span::call_site()),
                Target::Decode | Target::BorrowDecode => Ident::new("__decoder", Span::call_site()),
            },
            encoder_generic: Ident::new("__T", Span::call_site()),
            generics: input.generics.clone(),
            item_name: input.ident.clone(),
            item_type,
            variants,
            enum_repr,
            struct_data,
            borrow_data: BorrowData {
                decoder: Lifetime::new("'__data", Span::call_site()),
                sub_lifetimes: lifetimes,
            },
        };

        // Then obtain the item level flags and apply them
        let groups = FlagGroup::from_attrs(&input.attrs)?;
        for group in groups {
            if group.scope.matches(target) {
                for flag in group.flags {
                    ctxt.flags.apply(flag, group.scope)?;
                }
            }
        }

        let seek = ctxt.requires_seeking_impl();
        ctxt.flags.force_seek = seek;

        // TODO Detect and fix any potential name clash here

        Ok(ctxt)
    }

    pub fn requires_seeking_impl(&self) -> bool {
        self.flags.requires_seeking_impl()
            || self
                .struct_data
                .fields
                .iter()
                .map(|x| &x.flags)
                .any(Flags::requires_seeking_impl)
            || self
                .variants
                .iter()
                .flat_map(|x| &x.fields)
                .map(|x| &x.flags)
                .any(Flags::requires_seeking_impl)
    }
}

/// Extracts the Flavor (Struct, Tuple, Unit) of a list of fields as well as the fields themselves
pub fn extract_fields_and_flavor(
    fields: &Fields,
    target: Target,
) -> syn::Result<(Flavor, Vec<Field>)> {
    match fields {
        Fields::Named(fields) => Ok((
            Flavor::Struct,
            process_field_flags(fields.named.iter(), target)?,
        )),
        Fields::Unnamed(fields) => Ok((
            Flavor::Tuple,
            process_field_flags(fields.unnamed.iter(), target)?,
        )),
        Fields::Unit => Ok((Flavor::Unit, Vec::new())),
    }
}

/// Processes a syn field into our custom field type, applying flags on the way and
/// extracting necessary data while discarding unneeded data.
pub fn process_field_flags<'a>(
    syn_fields: impl Iterator<Item = &'a syn::Field>,
    target: Target,
) -> syn::Result<Vec<Field>> {
    let mut idx: usize = 0;
    let mut fields = Vec::new();

    for syn_field in syn_fields {
        // Fields in tuples need to be accessed differently, with the special syntax
        // of `value.0, value.1, ...`. This ensures they can be accessed correctly
        // while also generating human friendly names for creating references to the
        // fields. Example: `let ref m0 = value.0; let ref m1 = value.1; ...;
        let (accessor, name) = if let Some(name) = &syn_field.ident {
            (name.clone().to_token_stream(), name.clone())
        } else {
            (
                Index {
                    index: idx as u32,
                    span: syn_field.span(),
                }
                .to_token_stream(),
                format_ident!("m{}", idx),
            )
        };

        // Construct a new field without storing any flag
        let mut field = Field::new(accessor, name, syn_field.ty.clone());

        // Retrieve the flags and apply each in order
        let groups = FlagGroup::from_attrs(&syn_field.attrs)?;
        for group in groups {
            if group.scope.matches(target) {
                for flag in group.flags {
                    field.flags.apply(flag, group.scope)?;
                }
            }
        }

        fields.push(field);

        idx += 1;
    }

    Ok(fields)
}
