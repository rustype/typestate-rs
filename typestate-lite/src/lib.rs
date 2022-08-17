use std::fmt;

use heck::ToSnakeCase;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::ItemImpl;
use syn::{parse::Parser, parse_macro_input, parse_quote, Field, Fields, ItemStruct};
use syn::{Ident, ItemEnum};

#[proc_macro_attribute]
pub fn automaton(_: TokenStream, input: TokenStream) -> TokenStream {
    let mut parsed_struct = parse_macro_input!(input as syn::ItemStruct);
    let struct_ident = &parsed_struct.ident;

    // Generate the sealed module
    let sealed_mod_name = seal_mod_ident(struct_ident);
    let sealed_trait_name = format_ident!("{}", "Seal");
    let state_trait_name = format_ident!("{}State", struct_ident);
    let sealed_mod = quote! {
        #[automatically_derived]
        mod #sealed_mod_name {
            pub trait #sealed_trait_name {}
        }
        trait #state_trait_name: #sealed_mod_name::#sealed_trait_name {}
    };

    let state_type_param = format_ident!("State");
    let state_generic_param = parse_quote!(#state_type_param: #state_trait_name);
    parsed_struct.generics.params.push(state_generic_param);

    let state_field = Field::parse_named
        .parse2(quote! { state: #state_type_param })
        .unwrap();

    if let Fields::Named(named_fields) = &mut parsed_struct.fields {
        named_fields.named.push(state_field);
    }

    quote! {
        #parsed_struct
        #sealed_mod
    }
    .into()
}

#[proc_macro_attribute]
pub fn state(args: TokenStream, input: TokenStream) -> TokenStream {
    let automaton_ident = parse_macro_input!(args as syn::Ident);
    let parsed_input = parse_macro_input!(input as syn::Item);
    match parsed_input {
        syn::Item::Struct(item_struct) => Ok(parse_struct_state(automaton_ident, item_struct)),
        syn::Item::Enum(item_enum) => Ok(parse_enum_state(automaton_ident, item_enum)),
        _ => Err(syn::Error::new(
            Span::call_site(),
            "expected struct or enum",
        )),
    }
    .unwrap_or_else(|e| e.to_compile_error())
    .into()
}

fn parse_struct_state(automaton_ident: Ident, item_struct: ItemStruct) -> TokenStream2 {
    let state_ident = &item_struct.ident;
    let (impl_seal, impl_state) = sealed_impl(&automaton_ident, state_ident);
    quote! {
        #item_struct
        #impl_seal
        #impl_state
    }
}

fn parse_enum_state(automaton_ident: Ident, item_enum: ItemEnum) -> TokenStream2 {
    let state_ident = &item_enum.ident;
    let (impl_seal, impl_state) = sealed_impl(&automaton_ident, state_ident);

    println!("{:#?}", item_enum);
    // TODO: expand the enum variants to their state counter parts
    // ISSUE: expanding them with the typestate dsl only requires more information in the presence of generics
    // IDEA: provide an extra annotation which declares the generic parameters
    // IDEA: allow the user to declare the states manually, when detected - dont expand those variants
    // basically - if the variant is unit style -> expand, otherwise, don't

    quote! {
        #item_enum
        #impl_seal
        #impl_state
    }
}

fn seal_mod_ident<D: fmt::Display>(seal: D) -> syn::Ident {
    format_ident!("__seal_{}", &seal.to_string().to_snake_case())
}

fn sealed_impl(automaton_ident: &Ident, state_ident: &Ident) -> (ItemImpl, ItemImpl) {
    let mod_seal_ident = seal_mod_ident(automaton_ident);
    let trait_seal_ident = format_ident!("Seal");
    let trait_state_ident = format_ident!("{}State", automaton_ident);
    let impl_seal = parse_quote!(impl #mod_seal_ident::#trait_seal_ident for #state_ident {});
    let impl_state = parse_quote!(impl #trait_state_ident for #state_ident {});
    (impl_seal, impl_state)
}
