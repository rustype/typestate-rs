use core::panic;
use quote::{format_ident, quote, ToTokens};
use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
};
use syn::{
    parse::Parser, parse2, parse_macro_input, parse_quote, visit_mut::VisitMut, Attribute, Error,
    Field, Fields, Ident, Item, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStruct, ItemTrait,
    TraitItemMethod, TypePath,
};

const AUTOMATA_ATTR_IDENT: &'static str = "automata";
const STATE_ATTR_IDENT: &'static str = "state";

// macro_rules! parse_quote {(
//     $($code:tt)*
// ) => (
//     (|| {
//         fn type_of_some<T> (_: Option<T>)
//           -> &'static str
//         {
//             ::core::any::type_name::<T>()
//         }
//         let target_ty = None; if false { return target_ty.unwrap(); }
//         eprintln!(
//             "[{}:{}:{}:parse_quote!]\n  - ty: `{ty}`\n  - code: `{code}`",
//             file!(), line!(), column!(),
//             code = ::quote::quote!( $($code)* ),
//             ty = type_of_some(target_ty),
//         );
//         ::syn::parse_quote!( $($code)* )
//     })()
// )}

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
    let mut state_visitor = StateVisitor::new();
    state_visitor.visit_item_mod_mut(&mut module);

    let mut transition_visitor = TransitionVisitor::new(state_visitor.state_machine_info);
    transition_visitor.visit_item_mod_mut(&mut module);

    match &mut module.content {
        Some((_, v)) => {
            v.append(&mut state_visitor.sealed_trait.into());
        }
        None => {}
    };

    let mut errors = state_visitor.errors;
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

/// Extracted information from the states
struct StateInfo {
    /// Main structure (aka Automata ?)
    main_struct: Option<ItemStruct>, // late init
    /// Deterministic states (`struct`s)
    // TODO convert det_state into HashSet<Ident>
    det_states: HashSet<ItemStruct>,
    /// Non-deterministic states (`enum`s)
    non_det_states: Vec<ItemEnum>,
}

impl StateInfo {
    fn new() -> Self {
        Self {
            main_struct: None,
            det_states: HashSet::new(),
            non_det_states: Vec::new(),
        }
    }
}

struct SealedPattern {
    /// Ident for the sealed pattern public trait
    trait_ident: Option<Ident>, // late init
    /// Idents for the sealed elements.
    state_idents: Vec<Ident>,
}

impl SealedPattern {
    fn new() -> Self {
        Self {
            trait_ident: None,
            state_idents: Vec::new(),
        }
    }
}

impl Into<Vec<syn::Item>> for SealedPattern {
    /// Convert the SealedTrait into a vector of Item.
    /// This enables the addition of new items to the main module.
    fn into(self) -> Vec<syn::Item> {
        let private_mod_ident: Ident = parse_quote!(private);
        let private_mod_trait: Ident = parse_quote!(Private);

        let states = &self.state_idents;
        let private_mod: ItemMod = parse_quote! {
            mod #private_mod_ident {
                #(use super::#states;)*
                pub trait #private_mod_trait {}
                #(impl #private_mod_trait for #states {})*
            }
        };

        let trait_ident = self.trait_ident;
        let state_trait: ItemTrait = parse_quote! {
            pub trait #trait_ident: #private_mod_ident::#private_mod_trait {}
        };

        let trait_impls = self
            .state_idents
            .iter()
            .map(|ident| -> ItemImpl { parse_quote!(impl #trait_ident for #ident {}) })
            .map(|item_trait| Item::from(item_trait));

        let mut res: Vec<Item> = vec![Item::from(private_mod), Item::from(state_trait)];
        res.extend(trait_impls);
        res
    }
}

struct StateVisitor {
    /// State machine required information
    state_machine_info: StateInfo,
    /// Sealed trait information
    sealed_trait: SealedPattern,
    /// Errors found during expansion
    errors: Vec<syn::Error>,
}

impl StateVisitor {
    fn new() -> Self {
        Self {
            state_machine_info: StateInfo::new(),
            sealed_trait: SealedPattern::new(),
            errors: Vec::new(),
        }
    }

    /// Add `multiple attributes` error to the error vector.
    fn push_multiple_attr_error(&mut self, attr: &Attribute) {
        self.errors
            .push(Error::new_spanned(attr, "multiple attributes are declared"));
    }

    /// Add `multiple declarations` error to the error vector.
    fn push_multiple_decl_error(&mut self, attr: &Attribute, ident: &str) {
        self.errors.push(Error::new_spanned(
            attr,
            format!("`{}` can only be used once", ident),
        ));
    }

    /// Add `multiple automata` error to the error vector.
    fn push_multiple_automata_decl_error(&mut self, it: &ItemStruct) {
        self.errors
            .push(Error::new_spanned(it, "`automata` redefinition here"));
    }
}

impl VisitMut for StateVisitor {
    fn visit_item_struct_mut(&mut self, it_struct: &mut ItemStruct) {
        // println!("{:#?}", it_struct);

        let attributes = &it_struct.attrs;
        let mut remove_idx = vec![true; attributes.len()];

        // TODO
        // filtering attribute duplicates can be done with a set
        // the first decides which one is the "main"
        // the following attributes will either already be present
        // or make the size of the set go up by one

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
                match self.state_machine_info.main_struct {
                    Some(_) => self.push_multiple_automata_decl_error(it_struct),
                    None => self.state_machine_info.main_struct = Some(it_struct.clone()),
                };
                match add_state_type_param(it_struct) {
                    Ok(bound_ident) => match self.sealed_trait.trait_ident {
                        Some(_) => unreachable!("this should have been checked previously"),
                        None => self.sealed_trait.trait_ident = Some(bound_ident),
                    },
                    Err(e) => {
                        self.errors.push(e);
                        return;
                    }
                }
            }
            // add state
            STATE_ATTR_IDENT => {
                self.state_machine_info.det_states.insert(it_struct.clone());
                self.sealed_trait.state_idents.push(it_struct.ident.clone());
            }
            _ => panic!("unknown attribute passed the filters!!!"),
        }

        remove_attrs(&mut it_struct.attrs, &remove_idx);
    }
}

struct TransitionVisitor {
    state_info: StateInfo,
}

impl TransitionVisitor {
    fn new(state_info: StateInfo) -> Self {
        Self { state_info }
    }
}

impl VisitMut for TransitionVisitor {
    fn visit_item_trait_mut(&mut self, i: &mut ItemTrait) {
        let ident = &i.ident;
        i.ident = format_ident!("{}State", ident);
        // go deeper
        for item in i.items.iter_mut() {
            self.visit_trait_item_mut(item);
        }
    }

    fn visit_trait_item_method_mut(&mut self, i: &mut TraitItemMethod) {
        println!("{:#?}", i);
        let return_type = &mut i.sig.output;
        match return_type {
            syn::ReturnType::Default => {} // ignore
            syn::ReturnType::Type(_, b) => {
                match b.deref_mut() {
                    syn::Type::Path(p) => {
                        if let Some(state_ident) = p.path.get_ident() {
                            // TODO convert det_state into HashSet<Ident>
                            let t: HashSet<_> = self
                                .state_info
                                .det_states
                                .iter()
                                .map(|s| &s.ident)
                                .collect();
                            // check if it is a valid state
                            if t.contains(state_ident) {
                                // if valid `State` -> `Main<State>`
                                // TODO make this call less bad
                                let automata_ident =
                                    &self.state_info.main_struct.as_ref().unwrap().ident;
                                p.path = parse_quote!(#automata_ident<#state_ident>);
                            }
                        }
                    }
                    _ => {} // ignore
                }
            }
        }
    }
}

fn add_state_type_param(automata_item: &mut ItemStruct) -> syn::Result<Ident> {
    let struct_ident = &automata_item.ident;
    let type_param_ident = format_ident!("{}State", struct_ident);

    match &mut automata_item.fields {
        syn::Fields::Named(named) => {
            named
                .named
                .push(Field::parse_named.parse2(quote!(state: State)).unwrap());
        }
        syn::Fields::Unnamed(_) => {
            return syn::Result::Err(Error::new_spanned(
                automata_item,
                "unnamed field structures are not supported",
            ));
        }
        syn::Fields::Unit => {
            automata_item.fields = Fields::Named(parse_quote!({ state: State }));
        }
    };

    // add type parameter
    automata_item.generics.params.push(parse_quote!(State));

    let where_clause = &mut automata_item.generics.where_clause;
    match where_clause {
        Some(clause) => clause
            .predicates
            .push(parse_quote!(State: #type_param_ident)),
        None => {
            let where_clause = automata_item.generics.make_where_clause();
            where_clause
                .predicates
                .push(parse_quote!(State: #type_param_ident));
        }
    }

    Ok(type_param_ident)
}

fn remove_attrs(attrs: &mut Vec<Attribute>, indexes: &Vec<bool>) {
    let mut idx = 0;
    attrs.retain(|_| (indexes[idx], idx += 1).0)
}
