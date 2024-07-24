use std::ops::Deref;

use quote::ToTokens;
use syn::{
    Lifetime, Type, TypeReference, TypeSlice,
};
use syn::visit::{Visit, visit_type};

use crate::ctxt::Field;

/// Searches through the fields to find all the lifetime bounds
/// required by a `BorrowDecode` implementation.
/// If the field has a type modifier, that type will be searched for lifetimes instead.
/// If the target is `Decode`, this will discard all the borrow flags of the field.
pub fn process_field_lifetimes(
    fields: &mut [Field],
    out_lifetimes: &mut Vec<Lifetime>,
) -> syn::Result<()> {
    for field in fields.iter_mut() {
        let field_lifetimes = discover_field_lifetime_bounds(field)?;
        out_lifetimes.append(&mut field_lifetimes.clone());

        if field_lifetimes.len() > 0 {
            field.flags.borrow = Some(field_lifetimes);
        } else {
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

fn recursive_type_lifetime_discover(ty: &Type, lifetimes: &mut Vec<Lifetime>) -> syn::Result<()> {
    struct LifVisitor<'a>(&'a mut Vec<Lifetime>);
    
    impl Visit<'_> for LifVisitor<'_> {
        fn visit_lifetime(&mut self, lif: &Lifetime) {
            self.0.push(lif.clone());
        }
    }
    
    let mut visitor = LifVisitor(lifetimes);
    visit_type(&mut visitor, ty);
    Ok(())
}
