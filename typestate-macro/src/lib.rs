use core::panic;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::{collections::HashSet, convert::TryFrom, ops::DerefMut};
use syn::{parse::Parser, visit_mut::VisitMut, *};

type Result<Ok, Err = Error> = ::core::result::Result<Ok, Err>;

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

#[derive(Debug, PartialEq)]
enum TypestateAttr {
    Automata,
    State,
}

impl TryFrom<&Ident> for TypestateAttr {
    type Error = ();

    fn try_from(ident: &Ident) -> Result<Self, Self::Error> {
        if ident == AUTOMATA_ATTR_IDENT {
            Ok(Self::Automata)
        } else if ident == STATE_ATTR_IDENT {
            Ok(Self::State)
        } else {
            Err(())
        }
    }
}

impl TryFrom<&Path> for TypestateAttr {
    type Error = ();

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        if path.is_ident(AUTOMATA_ATTR_IDENT) {
            Ok(Self::Automata)
        } else if path.is_ident(STATE_ATTR_IDENT) {
            Ok(Self::State)
        } else {
            Err(())
        }
    }
}

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

    let mut state_machine_info = StateMachineInfo::new();

    // start visitor
    let mut state_visitor = DeterministicStateVisitor::new(&mut state_machine_info);
    state_visitor.visit_item_mod_mut(&mut module);

    // take ownership of sealed_trait so we can stop using state_visitor
    // after stopping using state_visitor we can
    let sealed_trait = state_visitor.sealed_trait;

    // report state_visitor errors and return
    let mut errors = state_visitor.errors;
    if !errors.is_empty() {
        return errors.to_compile_error().into();
    }

    let mut non_det_state_visitor = NonDeterministicStateVisitor::new(&mut state_machine_info);
    non_det_state_visitor.visit_item_mod_mut(&mut module);
    // report non_det_state_visitor errors and return
    errors = non_det_state_visitor.errors;
    if !errors.is_empty() {
        return errors.to_compile_error().into();
    }

    // Visit transitions
    // TODO state_machine_info can be &mut and declared outside of all these visitors
    let mut transition_visitor = TransitionVisitor::new(&mut state_machine_info);
    transition_visitor.visit_item_mod_mut(&mut module);

    // report transition_visitor errors and return
    errors = transition_visitor.errors;
    if !errors.is_empty() {
        return errors.to_compile_error().into();
    }

    // appending new code should happen after all other code is processed
    // since this adds the sealed pattern traits and those aren't valid states
    // if this is done before visiting transitions the generated code is flagged as invalid transitions
    match &mut module.content {
        Some((_, v)) => {
            v.append(&mut sealed_trait.into());
        }
        None => {}
    };

    // if errors do not exist, return the token stream
    let ret = module.into_token_stream();
    // println!("{}", ret);
    ret.into()
}

/// A value to `proc_macro2::TokenStream` conversion.
/// More precisely into
trait IntoCompileError {
    fn to_compile_error(self) -> TokenStream;
}

impl IntoCompileError for Vec<Error> {
    fn to_compile_error(mut self) -> TokenStream {
        if !self.is_empty() {
            // if errors exist, return all errors
            let fst_err = self.swap_remove(0);
            return self
                .into_iter()
                .fold(fst_err, |mut all, curr| {
                    all.combine(curr);
                    all
                })
                .to_compile_error();
        } else {
            TokenStream::new()
        }
    }
}

/// Extracted information from the states
struct StateMachineInfo {
    /// Main structure (aka Automata ?)
    main_struct: Option<ItemStruct>, // late init
    /// Deterministic states (`struct`s)
    det_states: HashSet<ItemStruct>,
    /// Non-deterministic states (`enum`s)
    non_det_states: HashSet<ItemEnum>,
    /// Set of extracted identifiers.
    state_idents: HashSet<Ident>,
}

impl StateMachineInfo {
    fn new() -> Self {
        Self {
            main_struct: None,
            det_states: HashSet::new(),
            non_det_states: HashSet::new(),
            state_idents: HashSet::new(),
        }
    }

    fn add_state(&mut self, state: Item) {
        match state {
            Item::Struct(item_struct) => {
                self.state_idents.insert(item_struct.ident.clone());
                self.det_states.insert(item_struct);
            }
            Item::Enum(item_enum) => {
                self.state_idents.insert(item_enum.ident.clone());
                self.non_det_states.insert(item_enum);
            }
            _ => panic!("invalid state"),
        }
    }

    fn is_valid_state_ident(&self, ident: &Ident) -> bool {
        self.state_idents.contains(ident)
    }
}

impl Default for StateMachineInfo {
    fn default() -> Self {
        Self {
            main_struct: None,
            det_states: HashSet::new(),
            non_det_states: HashSet::new(),
            state_idents: HashSet::new(),
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

struct DeterministicStateVisitor<'sm> {
    /// State machine required information
    state_machine_info: &'sm mut StateMachineInfo,
    /// Sealed trait information
    sealed_trait: SealedPattern,
    /// Errors found during expansion
    errors: Vec<syn::Error>,
}

impl<'sm> DeterministicStateVisitor<'sm> {
    fn new(state_machine_info: &'sm mut StateMachineInfo) -> Self {
        Self {
            state_machine_info,
            sealed_trait: SealedPattern::new(),
            errors: Vec::new(),
        }
    }

    /// Add `multiple attributes` error to the error vector.
    fn push_multiple_attr_error(&mut self, attr: &Attribute) {
        self.errors
            .push(Error::new_spanned(attr, "multiple attributes are declared"));
    }

    /// Add `duplicate attribute` error to the error vector.
    fn push_multiple_decl_error(&mut self, attr: &Attribute) {
        self.errors
            .push(Error::new_spanned(attr, format!("duplicate attribute")));
    }

    /// Add `multiple automata` error to the error vector.
    fn push_multiple_automata_decl_error(&mut self, it: &ItemStruct) {
        self.errors
            .push(Error::new_spanned(it, "`automata` redefinition here"));
    }
}

impl<'sm> VisitMut for DeterministicStateVisitor<'sm> {
    fn visit_item_struct_mut(&mut self, it_struct: &mut ItemStruct) {
        let attributes = &mut it_struct.attrs;
        let mut main_attr = None;
        attributes.retain(|attr| {
            let ts_attr = TypestateAttr::try_from(&attr.path);
            match ts_attr {
                Ok(inner_ts_attr) => {
                    eprintln!("{:#?}", main_attr);
                    match &main_attr {
                        Some(curr_attr) => {
                            if *curr_attr == inner_ts_attr {
                                self.push_multiple_decl_error(attr)
                            } else {
                                self.push_multiple_attr_error(attr)
                            }
                        }
                        None => {
                            // only if it wasnt previously assigned we can assign a new value
                            main_attr = Some(inner_ts_attr)
                        }
                    }
                    false
                }
                Err(()) => true,
            }
        });

        // if errors were reported stop processing
        if !self.errors.is_empty() {
            return;
        }

        match main_attr {
            Some(TypestateAttr::Automata) => {
                // check for multiple automata definitions
                match self.state_machine_info.main_struct {
                    Some(_) => {
                        self.push_multiple_automata_decl_error(it_struct);
                        return;
                    }
                    None => self.state_machine_info.main_struct = Some(it_struct.clone()),
                };
                // eprintln!("{:#?}", self.state_machine_info.main_struct);
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
            Some(TypestateAttr::State) => {
                self.state_machine_info.add_state(it_struct.clone().into());
                self.sealed_trait.state_idents.push(it_struct.ident.clone());
            }
            None => {
                // empty attribute list
                // ignore
                // TODO maybe do something?
                return;
            }
        }
    }
}

struct NonDeterministicStateVisitor<'sm> {
    state_machine_info: &'sm mut StateMachineInfo,
    errors: Vec<Error>,
}

impl<'sm> NonDeterministicStateVisitor<'sm> {
    fn new(state_machine_info: &'sm mut StateMachineInfo) -> Self {
        Self {
            state_machine_info,
            errors: Vec::new(),
        }
    }

    /// Add `undeclared state` error to the error vector.
    fn push_undeclared_state_error(&mut self, ident: &Ident) {
        self.errors.push(Error::new_spanned(
            ident,
            "`enum` variant is not a valid state",
        ));
    }

    /// Add `unsupported variant` error to the error vector.
    fn push_unsupported_variant_error(&mut self, variant: &Variant) {
        self.errors.push(Error::new_spanned(
            variant,
            "only unnamed `enum` variants are supported",
        ));
    }
}

impl<'sm> VisitMut for NonDeterministicStateVisitor<'sm> {
    fn visit_item_enum_mut(&mut self, i: &mut ItemEnum) {
        for variant in &mut i.variants {
            let ident = &variant.ident;
            if !self.state_machine_info.is_valid_state_ident(ident) {
                self.push_undeclared_state_error(ident)
            }
            if let Fields::Unit = &variant.fields {
                // TODO make this call less bad
                let automata_ident = &self.state_machine_info.main_struct.as_ref().unwrap().ident;
                variant.fields = Fields::Unnamed(parse_quote!((#automata_ident<#ident>)));
            } else {
                self.push_unsupported_variant_error(variant);
            }
        }
    }
}

enum FnKind {
    /// Describes an initial state.
    ///
    /// For example:
    /// ```no_run
    /// fn initial() -> StateA {}
    /// ```
    Initial,
    /// Describes a state transition.
    ///
    /// For example:
    /// ```no_run
    /// fn transition(self: StateA) -> StateB {}
    /// ```
    Transition,
    /// Describes a final state.
    ///
    /// For example:
    /// ```no_run
    /// fn final(self: StateB) {}
    /// ```
    Final,
    /// Any other kind of function.
    Unknown,
}

struct TransitionVisitor<'sm> {
    state_machine_info: &'sm mut StateMachineInfo,
    errors: Vec<Error>,
}

impl<'sm> TransitionVisitor<'sm> {
    fn new(state_machine_info: &'sm mut StateMachineInfo) -> Self {
        Self {
            state_machine_info,
            errors: Vec::new(),
        }
    }

    /// Add `unknown state` error to the error vector.
    fn push_unknown_state_error(&mut self, ident: &Ident) {
        self.errors.push(Error::new_spanned(
            ident,
            format!("`{}` is not a declared state", ident),
        ));
    }

    // fn extract_fn_kind(&self, sig: &Signature) -> FnKind {
    //     let receiver = sig.receiver();
    //     let output = &sig.output;

    //     match receiver {
    //         // we have either transition or final
    //         Some(_) => {
    //             if let syn::ReturnType::Type(_, ty) = output {
    //                 if let syn::Type::Path(ty_path) = ty.deref() {
    //                     if let Some(ident) = ty_path.path.get_ident() {
    //                         if self.is_valid_state_ident(ident) {
    //                             return FnKind::Transition
    //                         }
    //                     }
    //                 }
    //             }
    //             return FnKind::Unknown
    //             // match output {
    //             //     syn::ReturnType::Default => FnKind::Unknown,
    //             //     syn::ReturnType::Type(_, ty) => {
    //             //         match ty.deref() {
    //             //             syn::Type::Path(ty_path) => {
    //             //                 if let Some(ident) = ty_path.path.get_ident() {

    //             //                 } else {
    //             //                     FnKind::Unknown
    //             //                 }
    //             //             }
    //             //             _ => FnKind::Unknown
    //             //         }
    //             //     }
    //             // }
    //         }
    //         // we have either initial or unknown
    //         None => {}
    //     }
    // }
}

impl<'sm> VisitMut for TransitionVisitor<'sm> {
    fn visit_item_trait_mut(&mut self, i: &mut ItemTrait) {
        let ident = &i.ident;

        if self.state_machine_info.state_idents.contains(ident) {
            i.ident = format_ident!("{}State", ident);
            // go deeper
            for item in i.items.iter_mut() {
                self.visit_trait_item_mut(item);
            }
        } else {
            self.push_unknown_state_error(ident);
        }
    }

    // TODO check for the following kinds of function
    // self, _ -> State
    // _ -> State
    // State -> _
    fn visit_trait_item_method_mut(&mut self, i: &mut TraitItemMethod) {
        // println!("{:#?}", i);
        let return_type = &mut i.sig.output;
        match return_type {
            syn::ReturnType::Default => {} // ignore
            syn::ReturnType::Type(_, ty) => {
                match ty.deref_mut() {
                    syn::Type::Path(p) => {
                        if let Some(state_ident) = p.path.get_ident() {
                            // check if it is a valid state
                            if self.state_machine_info.is_valid_state_ident(state_ident) {
                                // if valid `State` -> `Main<State>`
                                // TODO make this call less bad
                                let automata_ident =
                                    &self.state_machine_info.main_struct.as_ref().unwrap().ident;
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
                .push(Field::parse_named.parse2(quote!(pub state: State)).unwrap());
        }
        syn::Fields::Unnamed(_) => {
            return syn::Result::Err(Error::new_spanned(
                automata_item,
                "unnamed field structures are not supported",
            ));
        }
        syn::Fields::Unit => {
            automata_item.fields = Fields::Named(parse_quote!({ pub state: State }));
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
