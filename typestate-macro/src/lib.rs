use std::{borrow::Borrow, convert::TryFrom, option};

use quote::ToTokens;
use syn::{
    visit_mut::{self, visit_item_fn_mut, VisitMut},
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
    let output = input.clone();
    let ref mut module = parse_macro_input!(input as Item);
    // match parse_module_item(module) {
    //     Ok(tt) => tt,
    //     Err(error) => error.to_compile_error(),
    // }
    // .into()
    let mut visitor = StateMachineVisitor::new();
    visitor.visit_item_mut(module);

    module.to_token_stream().into()
}

struct StateMachineVisitor<'a> {
    /// Main structure (aka Automata ?)
    main_struct: Option<&'a ItemStruct>, // late init
    /// Deterministic states (`struct`s)
    det_states: Vec<&'a ItemStruct>,
    /// Non-deterministic states (`enum`s)
    non_det_states: Vec<&'a ItemEnum>,
    /// Errors found during expansion
    errors: Vec<syn::Error>,
}

impl<'a> StateMachineVisitor<'a> {
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

impl<'a> visit_mut::VisitMut for StateMachineVisitor<'a> {
    fn visit_item_struct_mut(&mut self, it_struct: &mut ItemStruct) {
        let attributes = &it_struct.attrs;
        let mut remove_idx = vec![true; attributes.len()];
        for (idx, attr) in attributes.iter().enumerate() {
            if attr.path.is_ident(AUTOMATA_ATTR_IDENT) {
                if let Some(_) = self.main_struct {
                    // automata was previously defined
                    self.errors.push(syn::Error::new_spanned(
                        &attr,
                        "`automata` redefinition is not allowed",
                    ))
                }
                println!("{:#?}", it_struct);
                remove_idx[idx] = false;
            }
            if attr.path.is_ident(STATE_ATTR_IDENT) {
                println!("{:#?}", it_struct);
                remove_idx[idx] = false;
            }
        }
        let mut idx = 0;
        (&mut it_struct.attrs).retain(|_| (remove_idx[idx], idx += 1).0);
        println!("{:#?}", &it_struct.attrs);
    }
}
