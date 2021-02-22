use std::option;

use quote::ToTokens;
use syn::ItemEnum;

use {
    proc_macro2::TokenStream,
    syn::{parse_macro_input, spanned::Spanned, Error, ItemMod, ItemStruct, Result},
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
}

fn parse_module_item(item_mod: ItemMod) -> Result<TokenStream> {
    if let None = item_mod.content {
        return Err(Error::new(item_mod.span(), "module cannot be empty"));
    }
    let mut state_machine_ast = StateMachineAST::new();
    let (_, module_items) = item_mod.content.unwrap(); // safe unwrap, checked before
    'item_loop: for item in module_items.iter() {
        match item {
            syn::Item::Enum(it_enum) => {
                state_machine_ast.non_det_states.push(it_enum);
            }
            syn::Item::Fn(_) => unimplemented!(),
            syn::Item::Struct(it_struct) => {
                if it_struct.attrs.is_empty() {
                    // no attribute = main structure
                    if state_machine_ast.main_struct.is_none() {
                        state_machine_ast.main_struct = Some(it_struct);
                    } else {
                        return Err(Error::new(
                            it_struct.span(),
                            "there can only be a single main `struct`",
                        ));
                    }
                } else {
                    // check for the `state` attribute
                    // TODO add support for attributes which are carried to the final expansion
                    for attr in &it_struct.attrs {
                        if attr.path.is_ident("state") {
                            state_machine_ast.det_states.push(it_struct);
                            continue 'item_loop;
                        }
                    }
                    // this case should consider that the main struct may have other attributes
                    // adding a special attribute to the main automata fixes this
                    return Err(Error::new(it_struct.span(), "expected #[state]"))
                }
            }
            _ => {
                return Err(Error::new(
                    item.span(),
                    "`#[typestate] only supports `struct`, `enum` and `fn`",
                ))
            }
        };
    }
    Ok(module_items.iter().map(|it| it.to_token_stream()).collect())
}

struct StateMachineAST<'a> {
    /// Main structure (aka Automata ?)
    main_struct: Option<&'a ItemStruct>, // late init
    /// Deterministic states (`struct`s)
    det_states: Vec<&'a ItemStruct>,
    /// Non-deterministic states (`enum`s)
    non_det_states: Vec<&'a ItemEnum>,
}

impl<'a> StateMachineAST<'a> {
    fn new() -> Self {
        Self {
            main_struct: None,
            det_states: Vec::new(),
            non_det_states: Vec::new(),
        }
    }
}
