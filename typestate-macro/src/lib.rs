use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::{collections::HashMap, iter::once};
use syn::{
    parse::Parser,
    parse_macro_input, parse_quote,
    spanned::Spanned,
    visit_mut::{self, VisitMut},
    Attribute, Error, Field, Fields, Item, ItemEnum, ItemMod, ItemStruct, Result, TypeParam,
};

#[proc_macro_attribute]
pub fn typestate(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // allow macro "misuses"
    // e.g. #[typestate(non_existent)]
    // this approach does not break future implementations
    let _: syn::parse::Nothing = parse_macro_input!(attrs);
    // parse the input as a mod
    let mut module: ItemMod = parse_macro_input!(input);
    // start visitor
    let mut visitor = StateMachineVisitor::new();
    visitor.visit_item_mod_mut(&mut module);

    let mut errors = visitor.errors;
    if errors.is_empty() {
        // if errors do not exist, return the token stream
        let ret = module.into_token_stream();
        // println!("{}", ret);
        ret
    } else {
        // if errors exist, return all errors
        let fst_err = errors.swap_remove(0);
        errors
            .into_iter()
            .fold(fst_err, |mut all, curr| {
                all.combine(curr);
                all
            })
            .to_compile_error()
    }
    .into()
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

    fn push_multiple_attr_error(&mut self, attr: &Attribute) {
        self.errors
            .push(Error::new_spanned(attr, "multiple attributes are declared"));
    }

    fn push_multiple_decl_error(&mut self, attr: &Attribute, ident: &str) {
        self.errors.push(Error::new_spanned(
            attr,
            format!("`{}` can only be used once", ident),
        ));
    }

    fn push_multiple_automata_decl_error(&mut self, it: &ItemStruct) {
        self.errors
            .push(Error::new_spanned(it, "`automata` redefinition here"));
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

        // filter out relevant attributes
        let mut filtered = HashMap::new();
        for (idx, attr) in attributes.iter().enumerate() {
            if attr.path.is_ident(AUTOMATA_ATTR_IDENT) {
                match filtered.get_mut(AUTOMATA_ATTR_IDENT) {
                    None => {
                        filtered.insert(AUTOMATA_ATTR_IDENT, vec![(idx, attr)]);
                    }
                    Some(v) => {
                        v.push((idx, attr));
                    }
                }
                // mark for removal
                remove_idx[idx] = false;
            } else if attr.path.is_ident(STATE_ATTR_IDENT) {
                match filtered.get_mut(STATE_ATTR_IDENT) {
                    None => {
                        filtered.insert(STATE_ATTR_IDENT, vec![(idx, attr)]);
                    }
                    Some(v) => {
                        v.push((idx, attr));
                    }
                }
                // mark for removal
                remove_idx[idx] = false;
            }
        }

        // TODO consider the 0 case
        let mut attr_type = Option::None;
        match filtered.keys().len() {
            1 => filtered.into_iter().for_each(|(k, v)| {
                if v.len() > 1 {
                    for (_, attr) in v {
                        self.push_multiple_decl_error(attr, k);
                    }
                } else {
                    attr_type = Option::Some(k);
                }
            }),
            2 => filtered.into_iter().for_each(|(k, v)| {
                if v.len() > 1 {
                    for (_, attr) in v {
                        self.push_multiple_decl_error(attr, k);
                        self.push_multiple_attr_error(attr);
                    }
                } else {
                    for (_, attr) in v {
                        self.push_multiple_attr_error(attr);
                    }
                }
            }),
            _ => {
                panic!("it was supposed to only be 1 or 2 keys")
            }
        }

        // if errors were reported stop processing
        if !self.errors.is_empty() {
            return;
        }

        match attr_type.unwrap() {
            // check for multiple automata definitions
            AUTOMATA_ATTR_IDENT => {
                match self.main_struct {
                    Some(_) => self.push_multiple_automata_decl_error(it_struct),
                    None => self.main_struct = Some(it_struct.clone()),
                };
                if let Err(e) = add_state_type_param(it_struct) {
                    self.errors.push(e);
                    return;
                }
            }
            // add state
            STATE_ATTR_IDENT => self.det_states.push(it_struct.clone()),
            _ => panic!("unknown attribute passed the filters!!!"),
        }

        remove_attrs(&mut it_struct.attrs, &remove_idx);
    }
}

fn add_state_type_param(automata_item: &mut ItemStruct) -> syn::Result<()> {
    let struct_ident = &automata_item.ident;
    let type_param_ident = format_ident!("{}State", struct_ident);

    match &mut automata_item.fields {
        syn::Fields::Named(named) => {
            named.named.push(
                Field::parse_named
                    .parse2(quote!(state: #type_param_ident))
                    .unwrap(),
            );
        }
        syn::Fields::Unnamed(_) => {
            return syn::Result::Err(Error::new_spanned(
                automata_item,
                "unnamed field structures are not supported",
            ));
        }
        syn::Fields::Unit => {
            automata_item.fields = Fields::Named(parse_quote! (state: #type_param_ident));
        }
    };

    // add type parameter
    automata_item
        .generics
        .params
        .push(parse_quote!(#type_param_ident));
    Ok(())
}

fn remove_attrs(attrs: &mut Vec<Attribute>, indexes: &Vec<bool>) {
    let mut idx = 0;
    attrs.retain(|_| (indexes[idx], idx += 1).0)
}
