use quote::ToTokens;
use syn::spanned::Spanned;

use {
    proc_macro2::TokenStream,
    syn::{parse_macro_input, Error, ItemMod, Result},
};

#[proc_macro_attribute]
pub fn typestate(
    _: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let module = parse_macro_input!(input as ItemMod);
    println!("{:#?}", module);
    match parse_module_item(module) {
        Ok(tt) => tt,
        Err(error) => error.to_compile_error(),
    }
    .into()
    // TokenStream::new().into()
}

fn parse_module_item(item_mod: ItemMod) -> Result<TokenStream> {
    if let Some((_, module_items)) = item_mod.content {
        Ok(module_items.iter().map(|it| it.to_token_stream()).collect())
    } else {
        Err(Error::new(item_mod.span(), "module cannot be empty"))
    }
}
