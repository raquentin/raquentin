use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

/// Derives the `DyckToken` trait for the given enum.
///
/// This derive macro automatically implements the `DyckToken` trait and its required
/// traits (`Clone`, `Copy`, `Eq`, `PartialEq`, and `Hash`) for the enum it is applied to.
///
/// For usage and more information, see the main dyck crate documentation.
#[proc_macro_derive(DyckToken)]
pub fn derive_token(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let expanded = quote! {
        impl DyckToken for #name {}
        impl Clone for #name {
            fn clone(&self) -> Self {
                *self
            }
        }
        impl Copy for #name {}
        impl Eq for #name {}
        impl PartialEq for #name {
            fn eq(&self, other: &Self) -> bool {
                std::mem::discriminant(self) == std::mem::discriminant(other)
            }
        }
        impl std::hash::Hash for #name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                std::mem::discriminant(self).hash(state);
            }
        }
    };

    TokenStream::from(expanded)
}
