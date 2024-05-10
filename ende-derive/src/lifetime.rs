use std::ops::Deref;

use quote::ToTokens;
use syn::{
    GenericArgument, Lifetime, Path, PathArguments, Type, TypeParamBound, TypeReference, TypeSlice,
};

use crate::ctxt::{Field, Target};

/// Searches through the fields to find all the lifetime bounds
/// required by a `BorrowDecode` implementation.
/// If the field has a type modifier, that type will be searched for lifetimes instead.
/// If the target is `Decode`, this will discard all the borrow flags of the field.
pub fn process_field_lifetimes(
    target: Target,
    fields: &mut [Field],
    out_lifetimes: &mut Vec<Lifetime>,
) -> syn::Result<()> {
    if target == Target::BorrowDecode {
        let mut lifetimes = Vec::new();

        for field in fields.iter_mut() {
            let field_lifetimes = discover_field_lifetime_bounds(field)?;
            lifetimes.append(&mut field_lifetimes.clone());

            if field_lifetimes.len() > 0 {
                field.flags.borrow = Some(field_lifetimes);
            } else {
                field.flags.borrow = None;
            }
        }

        out_lifetimes.append(&mut lifetimes);
    } else {
        for field in fields.iter_mut() {
            field.flags.borrow = None;
        }
    }
    Ok(())
}

/// Figures out the lifetime bounds introduced by a single field.
///
/// The field's [virtual type][`Field::virtual_ty`] is used
fn discover_field_lifetime_bounds(field: &Field) -> syn::Result<Vec<Lifetime>> {
    if let Some(borrow) = &field.flags.borrow {
        if borrow.len() > 0 {
            // Simple resolution:
            // The user provided explicit lifetime bounds, so we just use those
            Ok(borrow.clone())
        } else {
            // Lifetime discover:
            // The user declared the "borrow" flag but didn't provide any explicit lifetimes
            // Scan the type signature for lifetimes
            let mut lifetimes = Vec::new();

            recursive_type_lifetime_discover(field.virtual_ty(), &mut lifetimes)?;

            Ok(lifetimes)
        }
    } else {
        // No borrow flag was declared, but maybe we can infer the lifetime for simple types
        match field.virtual_ty() {
            Type::Reference(TypeReference {
                lifetime: Some(lif),
                elem,
                ..
            }) => match elem.deref() {
                Type::Slice(TypeSlice { elem, .. }) => {
                    let name = elem.to_token_stream().to_string();
                    match name.as_str() {
                        "u8" | "u16" | "u32" | "u64" | "u128" | "i8" | "i16" | "i32" | "i64"
                        | "i128" | "usize" | "isize" => {
                            return Ok(vec![lif.clone()]);
                        }
                        _ => {}
                    }
                }
                Type::Path(path) => {
                    let name = path.to_token_stream().to_string();
                    match name.as_str() {
                        "str" => {
                            return Ok(vec![lif.clone()]);
                        }
                        _ => {}
                    }
                }
                _ => {}
            },
            _ => {}
        }
        // No simple type detected
        Ok(Vec::new())
    }
}

fn recursive_type_lifetime_discover(ty: &Type, vec: &mut Vec<Lifetime>) -> syn::Result<()> {
    match ty {
        Type::Array(array) => recursive_type_lifetime_discover(&array.elem, vec),
        Type::BareFn(_) => Ok(()),
        Type::Group(group) => recursive_type_lifetime_discover(&group.elem, vec),
        Type::ImplTrait(_) | Type::Infer(_) => {
            // Not allowed in structs
            Ok(())
        }
        Type::Macro(_) => {
            // We can't detect lifetimes in this case!
            // When proc_macro_diagnostic is stabilized, provide a warning instead
            Ok(())
        }
        Type::Never(_) => {
            // No lifetimes here
            Ok(())
        }
        Type::Paren(paren) => recursive_type_lifetime_discover(&paren.elem, vec),
        Type::Path(path) => {
            if let Some(qself) = &path.qself {
                recursive_type_lifetime_discover(&qself.ty, vec)?;
            }
            recursive_path_lifetime_discover(&path.path, vec)
        }
        Type::Ptr(ptr) => recursive_type_lifetime_discover(&ptr.elem, vec),
        Type::Reference(reference) => {
            if let Some(lif) = &reference.lifetime {
                vec.push(lif.clone());
            }
            recursive_type_lifetime_discover(&reference.elem, vec)
        }
        Type::Slice(slice) => recursive_type_lifetime_discover(&slice.elem, vec),
        Type::TraitObject(trait_obj) => {
            for bound in trait_obj.bounds.iter() {
                recursive_type_param_bound_lifetime_discover(bound, vec)?;
            }
            Ok(())
        }
        Type::Tuple(tuple) => {
            for ty in tuple.elems.iter() {
                recursive_type_lifetime_discover(ty, vec)?;
            }
            Ok(())
        }
        Type::Verbatim(_) => Ok(()),
        _ => Ok(()),
    }
}

fn recursive_path_lifetime_discover(path: &Path, vec: &mut Vec<Lifetime>) -> syn::Result<()> {
    for segment in path.segments.iter() {
        match &segment.arguments {
            PathArguments::AngleBracketed(angle_bracketed) => {
                for arg in angle_bracketed.args.iter() {
                    recursive_generics_lifetime_discover(arg, vec)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn recursive_type_param_bound_lifetime_discover(
    bound: &TypeParamBound,
    vec: &mut Vec<Lifetime>,
) -> syn::Result<()> {
    match bound {
        TypeParamBound::Trait(trait_bound) => {
            recursive_path_lifetime_discover(&trait_bound.path, vec)?;
        }
        TypeParamBound::Lifetime(lif) => vec.push(lif.clone()),
        _ => {}
    }
    Ok(())
}

fn recursive_generics_lifetime_discover(
    arg: &GenericArgument,
    vec: &mut Vec<Lifetime>,
) -> syn::Result<()> {
    match arg {
        GenericArgument::Lifetime(lif) => vec.push(lif.clone()),
        GenericArgument::Type(ty) => recursive_type_lifetime_discover(ty, vec)?,
        GenericArgument::Const(_) => {}
        GenericArgument::AssocType(ty) => {
            if let Some(generics) = &ty.generics {
                for arg in generics.args.iter() {
                    recursive_generics_lifetime_discover(arg, vec)?;
                }
            }
            recursive_type_lifetime_discover(&ty.ty, vec)?;
        }
        GenericArgument::AssocConst(assoc_const) => {
            if let Some(generics) = &assoc_const.generics {
                for arg in generics.args.iter() {
                    recursive_generics_lifetime_discover(arg, vec)?;
                }
            }
        }
        GenericArgument::Constraint(constraint) => {
            if let Some(generics) = &constraint.generics {
                for arg in generics.args.iter() {
                    recursive_generics_lifetime_discover(arg, vec)?;
                }
            }
            for bound in constraint.bounds.iter() {
                recursive_type_param_bound_lifetime_discover(bound, vec)?;
            }
        }
        _ => {}
    }
    Ok(())
}
