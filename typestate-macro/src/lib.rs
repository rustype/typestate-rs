use std::{convert::TryFrom, option};

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

    output
}

struct StateMachineVisitor<'a> {
    /// Main structure (aka Automata ?)
    main_struct: Option<&'a ItemStruct>, // late init
    /// Deterministic states (`struct`s)
    det_states: Vec<&'a ItemStruct>,
    /// Non-deterministic states (`enum`s)
    non_det_states: Vec<&'a ItemEnum>,
}

impl<'a> StateMachineVisitor<'a> {
    fn new() -> Self {
        Self {
            main_struct: None,
            det_states: Vec::new(),
            non_det_states: Vec::new(),
        }
    }
}

enum AutomataAttributes {
    Automata,
    State,
}

/// This function finds valid attributes in the
fn get_struct_attribute()

impl<'a> visit_mut::VisitMut for StateMachineVisitor<'a> {
    fn visit_item_struct_mut(&mut self, it_struct: &mut ItemStruct) {
        println!("{:#?}", it_struct);

    }
}
