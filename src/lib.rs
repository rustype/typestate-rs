use quote::ToTokens;
use syn::spanned::Spanned;

use {
    proc_macro2::TokenStream,
    syn::{parse_macro_input, Error, Item, Result},
};

#[proc_macro_attribute]
pub fn typestate(
    _: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let module = parse_macro_input!(input as Item);
    println!("{:#?}", module);
    match parse_module_item(module) {
        Ok(tt) => tt,
        Err(error) => error.to_compile_error(),
    }
    .into()
}

fn parse_module_item(item: Item) -> Result<TokenStream> {
    match item {
        Item::Mod(module) => Ok(module.to_token_stream()),
        _ => Err(Error::new(item.span(), "expected mod")),
    }
}
