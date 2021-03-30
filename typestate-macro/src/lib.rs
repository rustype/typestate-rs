use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens};
use typestate_automata::DFA;
use std::{collections::HashSet, convert::TryFrom};
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

#[proc_macro_attribute]
pub fn typestate(attrs: TokenStream, input: TokenStream) -> TokenStream {
    macro_rules! bail_if_any {
        ( $errors:expr ) => {
            match $errors {
                errors => {
                    if !errors.is_empty() {
                        return errors.to_compile_error().into();
                    }
                }
            }
        };
    }

    // conservatively deny macro "misuses"
    // e.g. #[typestate(non_existent)]
    // this approach does not break future implementations
    let _: syn::parse::Nothing = parse_macro_input!(attrs);
    // parse the input as a mod
    let mut module: ItemMod = parse_macro_input!(input);

    let mut state_machine_info = StateMachineInfo::new();

    // start visitor
    let mut state_visitor = DeterministicStateVisitor::new(&mut state_machine_info);
    state_visitor.visit_item_mod_mut(&mut module);
    // report state_visitor errors and return
    bail_if_any!(state_visitor.errors);

    let sealed_trait = state_visitor.sealed_trait;
    if sealed_trait.trait_ident.is_none() {
        return Error::new(Span::call_site(), "Missing `#[automata]` struct.")
            .to_compile_error()
            .into();
    }

    let mut non_det_state_visitor = NonDeterministicStateVisitor::new(&mut state_machine_info);
    non_det_state_visitor.visit_item_mod_mut(&mut module);
    // report non_det_state_visitor errors and return
    bail_if_any!(non_det_state_visitor.errors);

    // Visit transitions
    // TODO state_machine_info can be &mut and declared outside of all these visitors
    let mut transition_visitor = TransitionVisitor::new(&mut state_machine_info);
    transition_visitor.visit_item_mod_mut(&mut module);

    // report transition_visitor errors and return
    bail_if_any!(transition_visitor.errors);

    let dfa = DFA::new();

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

fn setup_dfa(info: StateMachineInfo) -> DFA<Ident, Ident> {
    let dfa = DFA::new();
    for state in info.det_states.iter().map(|i| i.ident) {
        dfa.add_state(state);
    }
    dfa
}

#[derive(Debug, PartialEq)]
enum TypestateAttr {
    Automata,
    State,
}

impl TryFrom<&Ident> for TypestateAttr {
    // TODO take care of this error type
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

/// A value to `proc_macro2::TokenStream2` conversion.
/// More precisely into
trait IntoCompileError {
    fn to_compile_error(self) -> TokenStream2;
}

impl IntoCompileError for Vec<Error> {
    fn to_compile_error(mut self) -> TokenStream2 {
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
            TokenStream2::new()
        }
    }
}

// Nit(Daniel): the usage of `HashSet`s seems a bit "overkill" here, for
// the `{not_,}det_states` I mean: the only real benefit I see is being able
// to detect somebody feeding a duplicate definition inside the det_states
// or inside the `non_det_states`.
// That is, using `Vec`s could be an acceptable micro-optimization here, since
// the sizes of each set are expected to be small.
//
// Another possibility, which I find interestring, is to define a
// `HashMap<Ident ,/* -> */ Either<ItemStruct, ItemEnum>>` (feel free to
// define a helper type for the value type, say:
/*
enum State {
    Deterministic(ItemStruct),
    NonDeterministic(ItemEnum),
}
*/

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
    /// TODO
    transitions: HashSet<(Ident, Ident, Ident)>,
}

impl StateMachineInfo {
    fn new() -> Self {
        Self {
            main_struct: None,
            det_states: HashSet::new(),
            non_det_states: HashSet::new(),
            state_idents: HashSet::new(),
            transitions: HashSet::new(),
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
            _ => unreachable!("invalid state"),
        }
    }

    fn is_valid_state_ident(&self, ident: &Ident) -> bool {
        self.state_idents.contains(ident)
    }

    // maybe the unwrap could be converted into a check
    // if none -> comp time error
    fn main_state_name(&'_ self) -> &'_ Ident {
        &self.main_struct.as_ref().unwrap().ident
    }
}

impl Default for StateMachineInfo {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Default)]
struct SealedPattern {
    /// Ident for the sealed pattern public trait
    trait_ident: Option<Ident>, // late init
    /// Idents for the sealed elements.
    state_idents: Vec<Ident>,
}

impl Into<Vec<Item>> for SealedPattern {
    /// Convert the SealedTrait into a vector of Item.
    /// This enables the addition of new items to the main module.
    fn into(self) -> Vec<Item> {
        let trait_ident = self.trait_ident.expect("missing `.trait_ident`");
        let private_mod_ident = format_ident!("__private");
        // or `Private` or `Sealed` or `format_ident!("{}Sealed", …)`
        // take into account that `trait_ident` may have already been used
        let private_mod_trait = &trait_ident;

        let states = &self.state_idents;
        let mut ret = vec![];

        // Sealed trait
        ret.push(parse_quote! {
            /* private */ mod #private_mod_ident {
                pub trait #private_mod_trait {}
            }
        });

        // Sealed trait impls
        ret.extend(states.iter().map(|each_state| {
            parse_quote! {
                impl #private_mod_ident::#private_mod_trait for #each_state {}
            }
        }));

        // State trait
        ret.push(parse_quote! {
            pub trait #trait_ident: #private_mod_ident::#private_mod_trait {}
        });

        // Blanket impl of state trait from sealed implementors
        // This frees us from having to provide concrete impls for each type.
        ret.push(parse_quote! {
            impl<__T : ?::core::marker::Sized> #trait_ident
                for __T
            where
                __T : #private_mod_ident::#private_mod_trait,
            {}
        });
        ret
    }
}

struct DeterministicStateVisitor<'sm> {
    /// State machine required information
    state_machine_info: &'sm mut StateMachineInfo,
    /// Sealed trait information
    sealed_trait: SealedPattern,
    /// Errors found during expansion
    errors: Vec<Error>,
}

impl<'sm> DeterministicStateVisitor<'sm> {
    fn new(state_machine_info: &'sm mut StateMachineInfo) -> Self {
        Self {
            state_machine_info,
            sealed_trait: SealedPattern::default(),
            errors: vec![],
        }
    }

    /// Add `multiple attributes` error to the error vector.
    fn push_multiple_attr_error(&mut self, attr: &Attribute) {
        self.errors.push(Error::new_spanned(
            attr,
            "Multiple attributes are declared.",
        ));
    }

    /// Add `duplicate attribute` error to the error vector.
    fn push_multiple_decl_error(&mut self, attr: &Attribute) {
        self.errors
            .push(Error::new_spanned(attr, "Duplicate attribute."));
    }

    /// Add `multiple automata` error to the error vector.
    fn push_multiple_automata_decl_error(&mut self, it: &ItemStruct) {
        self.errors
            .push(Error::new_spanned(it, "`automata` redefinition here."));
    }
}

#[derive(PartialEq)]
enum Attr {
    Retain,
    Discard,
}

impl<'sm> VisitMut for DeterministicStateVisitor<'sm> {
    fn visit_item_struct_mut(&mut self, it_struct: &mut ItemStruct) {
        let attributes = &mut it_struct.attrs;
        let mut main_attr = None;
        attributes.retain(|attr| {
            Attr::Retain == {
                let ts_attr = TypestateAttr::try_from(&attr.path);
                match ts_attr {
                    Ok(inner_ts_attr) => {
                        // eprintln!("{:#?}", main_attr);
                        match main_attr {
                            Some(ref prev_attr) => {
                                if *prev_attr == inner_ts_attr {
                                    self.push_multiple_decl_error(attr);
                                } else {
                                    self.push_multiple_attr_error(attr);
                                }
                            }
                            ref mut at_none @ None => {
                                // only if it wasnt previously assigned we can assign a new value
                                *at_none = Some(inner_ts_attr)
                            }
                        }
                        Attr::Discard
                    }
                    Err(()) => Attr::Retain,
                }
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
            errors: vec![],
        }
    }

    /// Add `undeclared state` error to the error vector.
    fn push_undeclared_state_error(&mut self, ident: &Ident) {
        self.errors.push(Error::new_spanned(
            ident,
            "`enum` variant is not a valid state.",
        ));
    }

    /// Add `unsupported variant` error to the error vector.
    fn push_unsupported_variant_error(&mut self, variant: &Variant) {
        self.errors.push(Error::new_spanned(
            variant,
            "Only unit (C-like) `enum` variants are supported.",
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
                let automata_ident = self.state_machine_info.main_state_name();
                variant.fields = Fields::Unnamed(parse_quote!(
                    /* Variant */ (
                        #automata_ident<#ident>
                    )
                ));
            } else {
                self.push_unsupported_variant_error(variant);
            }
        }
    }
}

struct TransitionVisitor<'sm> {
    current_state: Option<Ident>,
    state_machine_info: &'sm mut StateMachineInfo,
    errors: Vec<Error>,
}

impl<'sm> TransitionVisitor<'sm> {
    fn new(state_machine_info: &'sm mut StateMachineInfo) -> Self {
        Self {
            current_state: None,
            state_machine_info,
            errors: vec![],
        }
    }

    /// Add `unknown state` error to the error vector.
    fn push_unknown_state_error(&mut self, ident: &Ident) {
        self.errors.push(Error::new_spanned(
            ident,
            format!("`{}` is not a declared state.", ident),
        ));
    }

    fn input_kind(&self, sig: &Signature) -> Option<()> {
        let fn_in = &sig.inputs;
        if let Some(FnArg::Receiver(_)) = fn_in.first() {
            Some(())
        } else {
            None
        }
    }

    fn output_kind(&self, sig: &mut Signature) -> Option<&Ident> {
        let fn_out = &mut sig.output;
        if let ReturnType::Type(_, ty) = fn_out {
            if let Type::Path(ref mut ty_path) = **ty {
                if let Some(ident) = ty_path.path.get_ident() {
                    if self.state_machine_info.is_valid_state_ident(ident) {
                        let automata_ident = self.state_machine_info.main_state_name();
                        ty_path.path = parse_quote!(#automata_ident<#ident>);
                        return Some(automata_ident);
                    }
                }
            }
        }
        None
    }
}

impl<'sm> VisitMut for TransitionVisitor<'sm> {
    fn visit_item_trait_mut(&mut self, i: &mut ItemTrait) {
        let ident = &i.ident;

        if self.state_machine_info.state_idents.contains(ident) {
            self.current_state = Some(ident.clone());
            i.ident = format_ident!("{}State", ident);
            // go deeper
            for item in i.items.iter_mut() {
                self.visit_trait_item_mut(item);
            }
        } else {
            self.push_unknown_state_error(ident);
        }
    }

    fn visit_trait_item_method_mut(&mut self, i: &mut TraitItemMethod) {
        // TODO account for non-deterministic states
        let sig = &mut i.sig;
        let input = self.input_kind(sig);
        let output = self.output_kind(sig);
        match (input, output) {
            (None, None) => {}    // unknown
            (None, Some(_)) => {} // initial
            (Some(_), None) => {} // final
            (Some(_), Some(ident)) => {
                let transition = (
                    self.current_state.as_ref().unwrap().clone(),
                    i.sig.ident.clone(),
                    ident.clone(),
                );
                self.state_machine_info.transitions.insert(transition);
            } // transition
        };
    }
}

fn add_state_type_param(automata_item: &mut ItemStruct) -> syn::Result<Ident> {
    let type_param_ident = format_ident!("{}State", automata_item.ident);
    automata_item
        .generics
        .params
        .push(parse_quote!(State: #type_param_ident));

    let field_to_add = quote!(
        pub state: State
    );

    match &mut automata_item.fields {
        syn::Fields::Named(named) => {
            named
                .named
                .push(Field::parse_named.parse2(field_to_add).unwrap());
        }
        syn::Fields::Unnamed(_) => {
            return syn::Result::Err(Error::new_spanned(
                automata_item,
                "Tuple structures are not supported.",
            ));
        }
        syn::Fields::Unit => {
            automata_item.fields = Fields::Named(parse_quote!({ #field_to_add }));
        }
    };

    Ok(type_param_ident)
}
