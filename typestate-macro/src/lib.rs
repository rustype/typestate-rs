use quote::{ToTokens, TokenStreamExt};
use syn::{
    visit_mut::{self, VisitMut},
    Attribute, Item, ItemEnum,
};

use {
    proc_macro2::TokenStream,
    syn::{parse_macro_input, spanned::Spanned, Error, ItemMod, ItemStruct, Result},
};

#[proc_macro_attribute]
pub fn typestate(
    _: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let ref mut module = parse_macro_input!(input as Item);
    // match parse_module_item(module) {
    //     Ok(tt) => tt,
    //     Err(error) => error.to_compile_error(),
    // }
    // .into()
    let mut visitor = StateMachineVisitor::new();
    visitor.visit_item_mut(module);

    let errors = visitor.errors;
    if !errors.is_empty() {
        let mut err_out = TokenStream::new();
        for error in errors {
            err_out.append_all(error.into_compile_error())
        }
        return err_out.into()
    }

    module.to_token_stream().into()
}

struct StateMachineVisitor {
    /// Main structure (aka Automata ?)
    main_struct: Option<ItemStruct>, // late init
    /// Deterministic states (`struct`s)
    det_states: Vec<ItemStruct>,
    /// Non-deterministic states (`enum`s)
    non_det_states: Vec<ItemEnum>,
    /// Errors found during expansion
    errors: Vec<syn::Error>,
}

impl StateMachineVisitor {
    fn new() -> Self {
        Self {
            main_struct: None,
            det_states: Vec::new(),
            non_det_states: Vec::new(),
            errors: Vec::new(),
        }
    }
}

// /// This function finds valid attributes in the
// fn get_struct_attribute()

const AUTOMATA_ATTR_IDENT: &'static str = "automata";
const STATE_ATTR_IDENT: &'static str = "state";

impl visit_mut::VisitMut for StateMachineVisitor {
    fn visit_item_struct_mut(&mut self, it_struct: &mut ItemStruct) {
        println!("{:#?}", it_struct);

        let attributes = &it_struct.attrs;
        let mut remove_idx = vec![true; attributes.len()];

        for (idx, attr) in attributes.iter().enumerate() {
            if attr.path.is_ident(AUTOMATA_ATTR_IDENT) {
                if self.main_struct.is_some() {
                    // automata was previously defined
                    self.errors.push(syn::Error::new_spanned(
                        &it_struct,
                        "`automata` redefinition is not allowed",
                    ))
                }
                self.main_struct = Some(it_struct.clone());
                remove_idx[idx] = false;
            }
            if attr.path.is_ident(STATE_ATTR_IDENT) {
                remove_idx[idx] = false;
            }
        }

        remove_attrs(&mut it_struct.attrs, &remove_idx);
    }
}

fn remove_attrs(attrs: &mut Vec<Attribute>, indexes: &Vec<bool>) {
    let mut idx = 0;
    attrs.retain(|_| (indexes[idx], idx += 1).0)
}
