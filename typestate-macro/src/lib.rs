use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens};
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    hash::Hash,
};
use syn::{parse::Parser, visit_mut::VisitMut, Signature, *};
#[cfg(feature = "typestate_debug")]
use typestate_automata::dot::*;
use typestate_automata::{DFA, NFA};

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
pub fn typestate(args: TokenStream, input: TokenStream) -> TokenStream {
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

    // Parse attribute arguments
    let attr_args: AttributeArgs = parse_macro_input!(args);
    let args = match TypestateArguments::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

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
        return TypestateError::MissingAutomata.to_compile_error().into();
    }

    let mut non_det_state_visitor = NonDeterministicStateVisitor::new(&mut state_machine_info);
    non_det_state_visitor.visit_item_mod_mut(&mut module);
    // report non_det_state_visitor errors and return
    bail_if_any!(non_det_state_visitor.errors);

    // Visit transitions
    let mut transition_visitor = TransitionVisitor::new(&mut state_machine_info);
    transition_visitor.visit_item_mod_mut(&mut module);

    // report transition_visitor errors and return
    bail_if_any!(transition_visitor.errors);
    bail_if_any!(state_machine_info.check_missing());

    let fa: FiniteAutomata<_, _> = state_machine_info.into();
    // eprintln!("{:#?}", fa);

    // TODO handle the duplicate code inside
    macro_rules! handle_automata {
        ($name:ident, $automata:ident) => {
            #[cfg(feature = "typestate_debug")]
            {
                let dot = Dot::from($automata.clone());
                dot.try_write_file(format!("./{}.dot", $name))
                    .expect("failed to write automata to file");
            }

            let errors: Vec<Error> = $automata
                .non_productive_states()
                .into_iter()
                .map(|ident| TypestateError::NonProductiveState(ident.clone()).into())
                .collect();
            bail_if_any!(errors);

            let errors: Vec<Error> = $automata
                .non_useful_states()
                .into_iter()
                .map(|ident| TypestateError::NonUsefulState(ident.clone()).into())
                .collect();
            bail_if_any!(errors);

            // do not parse more code
            // only generate from here

            let mut tokens: Vec<Item> = match args.enumerate {
                TOption::Some(str) => {
                    let ident = format_ident!("{}", str);
                    let states = $automata.states.iter().collect::<Vec<_>>();
                    // generate the enumeration
                    let enum_tokens = ::quote::quote! {
                        pub enum #ident {
                            #(#states(#$name<#states>),)*
                        }
                    };
                    // generate impls for conversion from type to enumeration
                    let from_tokens = states.iter().map(|state| {
                        ::quote::quote! {
                            impl ::core::convert::From<#$name<#state>> for #ident {
                                fn from(value: #$name<#state>) -> Self {
                                    Self::#state(value)
                                }
                            }
                        }
                    })
                    .map(|tokens| ::syn::parse_quote!(#tokens));
                    let mut res = vec![::syn::parse_quote!(#enum_tokens)];
                    res.extend(from_tokens);
                    res
                }
                TOption::Default => {
                    let ident = format_ident!("E{}", $name);
                    let states = $automata.states.iter().collect::<Vec<_>>();
                    // generate the enumeration
                    let enum_tokens = ::quote::quote! {
                        enum #ident {
                            #(#states(#$name<#states>),)*
                        }
                    };
                    // generate impls for conversion from type to enumeration
                    let from_tokens = states.iter().map(|state| {
                        ::quote::quote! {
                            impl ::core::convert::From<#$name<#state>> for #ident {
                                fn from(value: #$name<#state>) -> Self {
                                    Self::#state(value)
                                }
                            }
                        }
                    })
                    .map(|tokens| ::syn::parse_quote!(#tokens));
                    let mut res = vec![::syn::parse_quote!(#enum_tokens)];
                    res.extend(from_tokens);
                    res
                }
                TOption::None => vec![],
            };
            match &mut module.content {
                Some((_, v)) => {
                    v.append(&mut tokens);
                }
                None => {}
            }
        };
    }

    match fa {
        // TODO add explanations to the non-productive state and non-useful state
        FiniteAutomata::Deterministic(name, dfa) => {
            handle_automata!(name, dfa);
        }
        FiniteAutomata::NonDeterministic(name, nfa) => {
            handle_automata!(name, nfa);
        }
    }

    // appending new code should happen after all other code is processed
    // since this adds the sealed pattern traits and those aren't valid states
    // if this is done before visiting transitions the generated code is flagged as invalid transitions
    match &mut module.content {
        Some((_, v)) => {
            v.append(&mut sealed_trait.into());
        }
        None => {}
    }

    // if errors do not exist, return the token stream
    module.into_token_stream().into()
}

/// Option-like triplet. Used in argument parsing to differ between:
/// - Missing value `#[]`
/// - Concrete value `#[macro(attr = "value")]`
/// - Present but not overwritten `#[macro(attr)]`
#[derive(Debug)]
enum TOption<T> {
    Some(T),
    Default,
    None,
}

impl<T> Default for TOption<T> {
    fn default() -> Self {
        Self::None
    }
}

impl FromMeta for TOption<String> {
    /// If the input string is empty it returns `Ok(Default)`, otherwise it returns `Ok(Some(value))`.
    fn from_string(value: &str) -> darling::Result<Self> {
        if value.is_empty() {
            // arg = ""
            return Ok(Self::Default);
        } else {
            // arg = "..."
            return Ok(Self::Some(value.to_string()));
        }
    }

    /// Returns `Ok(Default)`.
    fn from_word() -> darling::Result<Self> {
        Ok(Self::Default)
    }
}

#[derive(Debug, FromMeta)]
struct TypestateArguments {
    /// Optional arguments.
    /// Declares if an enumeration is to be generated and possibly gives it a name.
    #[darling(default)]
    enumerate: TOption<String>,
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

#[derive(Debug, PartialEq, Eq, Hash)]
struct Transition {
    source: Ident,
    destination: Ident,
    symbol: Ident,
}

impl Transition {
    fn new(source: Ident, destination: Ident, symbol: Ident) -> Self {
        Self {
            source,
            destination,
            symbol,
        }
    }
}

/// Extracted information from the states
struct StateMachineInfo {
    /// Main structure (aka Automata ?)
    main_struct: Option<ItemStruct>, // late init
    /// Deterministic states (`struct`s)
    det_states: HashMap<Ident, ItemStruct>,
    /// Non-deterministic states (`enum`s)
    non_det_states: HashMap<Ident, ItemEnum>,
    /// Set of transitions.
    /// Extracted from functions with a signature like `(State) -> State`.
    transitions: HashSet<Transition>,
    /// Set of initial states.
    /// Extracted from functions with a signature like `() -> State`.
    initial_states: HashMap<Ident, HashSet<Ident>>,
    /// Set of final states.
    /// Extracted from functions with a signature like `(State) -> ()`.
    final_states: HashMap<Ident, HashSet<Ident>>,
}

impl StateMachineInfo {
    /// Construct a new [StateMachineInfo].
    fn new() -> Self {
        Self {
            main_struct: None,
            det_states: HashMap::new(),
            non_det_states: HashMap::new(),
            transitions: HashSet::new(),
            initial_states: HashMap::new(),
            final_states: HashMap::new(),
        }
    }

    /// Add a generic state to the [StateMachineInfo]
    // TODO create specialized versions which also use this one.
    fn add_state(&mut self, state: Item) {
        match state {
            Item::Struct(item_struct) => {
                self.det_states
                    .insert(item_struct.ident.clone(), item_struct);
            }
            Item::Enum(item_enum) => {
                self.non_det_states
                    .insert(item_enum.ident.clone(), item_enum);
            }
            _ => unreachable!("invalid state"),
        }
    }

    /// Return the main state identifier.
    /// This is an utility function.
    // maybe the unwrap could be converted into a check
    // if none -> comp time error
    fn main_state_name(&self) -> &Ident {
        &self.main_struct.as_ref().unwrap().ident
    }

    fn check_missing(&self) -> Vec<Error> {
        let mut errors = vec![];
        if self.initial_states.is_empty() {
            errors.push(TypestateError::MissingInitialState.into());
        }
        if self.final_states.is_empty() {
            errors.push(TypestateError::MissingFinalState.into());
        }
        errors
    }

    fn insert_initial(&mut self, state: Ident, transition: Ident) {
        if let Some(transitions) = self.initial_states.get_mut(&state) {
            transitions.insert(transition);
        } else {
            let mut transitions = HashSet::new();
            transitions.insert(transition);
            self.initial_states.insert(state, transitions);
        }
    }

    fn insert_final(&mut self, state: Ident, transition: Ident) {
        if let Some(transitions) = self.final_states.get_mut(&state) {
            transitions.insert(transition);
        } else {
            let mut transitions = HashSet::new();
            transitions.insert(transition);
            self.final_states.insert(state, transitions);
        }
    }
}

impl Default for StateMachineInfo {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
enum FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    Deterministic(Ident, DFA<State, Transition>),
    NonDeterministic(Ident, NFA<State, Transition>),
}

impl Into<FiniteAutomata<Ident, Ident>> for StateMachineInfo {
    fn into(self) -> FiniteAutomata<Ident, Ident> {
        if self.non_det_states.is_empty() {
            let mut dfa = DFA::new();
            let name = self.main_state_name().clone();
            self.det_states
                .into_iter()
                .map(|(ident, _)| ident)
                .for_each(|ident| dfa.add_state(ident));
            self.initial_states
                .into_iter()
                .for_each(|(ident, transitions)| {
                    transitions
                        .into_iter()
                        .for_each(|t| dfa.add_initial(ident.clone(), t))
                });
            self.final_states
                .into_iter()
                .for_each(|(ident, transitions)| {
                    transitions
                        .into_iter()
                        .for_each(|t| dfa.add_final(ident.clone(), t))
                });
            self.transitions
                .into_iter()
                .for_each(|t| dfa.add_transition(t.source, t.symbol, t.destination));
            FiniteAutomata::Deterministic(name, dfa)
        } else {
            let mut nfa = NFA::new();
            let name = self.main_state_name().clone();
            self.det_states
                .into_iter()
                .map(|(ident, _)| ident)
                .for_each(|ident| nfa.add_state(ident));
            self.initial_states
                .into_iter()
                .for_each(|(ident, transitions)| {
                    transitions
                        .into_iter()
                        .for_each(|t| nfa.add_initial(ident.clone(), t))
                });
            self.final_states
                .into_iter()
                .for_each(|(ident, transitions)| {
                    transitions
                        .into_iter()
                        .for_each(|t| nfa.add_final(ident.clone(), t))
                });
            for t in self.transitions {
                if let Some(state) = self.non_det_states.get(&t.destination) {
                    // nfa.add_transition(t.source, t.symbol.clone(), t.destination.clone());
                    nfa.add_non_deterministic_transitions(
                        t.source,
                        t.symbol,
                        state.variants.iter().map(|v| v.ident.clone()),
                    )
                } else {
                    nfa.add_transition(t.source, t.symbol, t.destination)
                }
            }
            FiniteAutomata::NonDeterministic(name, nfa)
        }
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
        ret.push(::syn::parse_quote! {
            /* private */ mod #private_mod_ident {
                pub trait #private_mod_trait {}
            }
        });

        // Sealed trait impls
        ret.extend(states.iter().map(|each_state| {
            ::syn::parse_quote! {
                impl #private_mod_ident::#private_mod_trait for #each_state {}
            }
        }));

        // State trait
        ret.push(::syn::parse_quote! {
            pub trait #trait_ident: #private_mod_ident::#private_mod_trait {}
        });

        // Blanket impl of state trait from sealed implementors
        // This frees us from having to provide concrete impls for each type.
        ret.push(::syn::parse_quote! {
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
        self.errors
            .push(TypestateError::ConflictingAttributes(attr.clone()).into());
    }

    /// Add `duplicate attribute` error to the error vector.
    fn push_multiple_decl_error(&mut self, attr: &Attribute) {
        self.errors
            .push(TypestateError::DuplicateAttributes(attr.clone()).into());
    }

    /// Add `multiple automata` error to the error vector.
    fn push_multiple_automata_decl_error(&mut self, it: &ItemStruct) {
        self.errors
            .push(TypestateError::AutomataRedefinition(it.clone()).into());
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
        self.errors
            .push(TypestateError::InvalidVariant(ident.clone()).into());
    }

    /// Add `unsupported variant` error to the error vector.
    fn push_unsupported_variant_error(&mut self, variant: &Variant) {
        self.errors
            .push(TypestateError::UnsupportedVariant(variant.clone()).into());
    }
}

impl<'sm> VisitMut for NonDeterministicStateVisitor<'sm> {
    fn visit_item_enum_mut(&mut self, i: &mut ItemEnum) {
        for variant in &mut i.variants {
            // check if the variant is a valid one
            // i.e. unit-style variant
            if let Fields::Unit = &variant.fields {
                let ident = &variant.ident;
                if self.state_machine_info.non_det_states.contains_key(ident) {
                    variant.fields = Fields::Unnamed(::syn::parse_quote!(
                        /* Variant */ (#ident)
                    ));
                } else if self.state_machine_info.det_states.contains_key(ident) {
                    let automata_ident = self.state_machine_info.main_state_name();
                    variant.fields = Fields::Unnamed(::syn::parse_quote!(
                        /* Variant */ (
                            #automata_ident<#ident>
                        )
                    ));
                } else {
                    self.push_undeclared_state_error(ident);
                }
            } else {
                self.push_unsupported_variant_error(variant);
            }
        }
        if self.errors.is_empty() {
            // self.state_machine_info.add_state(i.clone().into());
            self.state_machine_info
                .non_det_states
                .insert(i.ident.clone(), i.clone());
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
        self.errors
            .push(TypestateError::UnknownState(ident.to_owned()).into());
    }

    fn push_invalid_trait_error(&mut self, it: &ItemTrait) {
        self.errors
            .push(TypestateError::InvalidAssocFuntions(it.clone()).into());
    }
}

impl<'sm> VisitMut for TransitionVisitor<'sm> {
    fn visit_item_trait_mut(&mut self, i: &mut ItemTrait) {
        let ident = &i.ident;

        if self.state_machine_info.non_det_states.contains_key(ident) {
            self.push_invalid_trait_error(i);
            return;
        }

        if self.state_machine_info.det_states.contains_key(ident) {
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
        let attrs = &mut i.attrs;
        let sig = &mut i.sig;
        let mut states = HashSet::new();
        &self.state_machine_info.det_states.keys().for_each(|k| {
            states.insert(k.clone()); // HACK clone
        });
        &self.state_machine_info.non_det_states.keys().for_each(|k| {
            states.insert(k.clone()); // HACK clone
        });
        let fn_kind = sig.extract_signature_kind(&states);
        let fn_ident = sig.ident.clone();
        sig.expand_signature_state(&self.state_machine_info); // TODO check for correct expansion
        eprintln!("{} {:?}", sig.ident.to_string(), fn_kind);

        match fn_kind {
            FnKind::Initial(return_ty_ident) => {
                self.state_machine_info
                    .insert_initial(return_ty_ident, fn_ident);
            }
            FnKind::Final => {
                // add #[must_use]
                // attrs.push(::syn::parse_quote!(#[must_use]));
                let state = self.current_state.as_ref().unwrap().clone();
                self.state_machine_info.insert_final(state, fn_ident);
            }
            FnKind::Transition(return_ty_ident) => {
                // add #[must_use]
                attrs.push(::syn::parse_quote!(#[must_use]));
                let state = self.current_state.as_ref().unwrap().clone();
                let transition = Transition::new(state, return_ty_ident, fn_ident);
                self.state_machine_info.transitions.insert(transition);
            }
            FnKind::SelfTransition => {
                let state = self.current_state.as_ref().unwrap().clone();
                let transition = Transition::new(state.clone(), state, fn_ident);
                self.state_machine_info.transitions.insert(transition);
            }
            FnKind::Other => {}
        };
    }
}

fn add_state_type_param(automata_item: &mut ItemStruct) -> syn::Result<Ident> {
    // TODO make the suffix custom
    let type_param_ident = format_ident!("{}State", automata_item.ident);
    automata_item
        .generics
        .params
        .push(::syn::parse_quote!(State: #type_param_ident));

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
            return syn::Result::Err(
                TypestateError::UnsupportedStruct(automata_item.clone()).into(),
            );
        }
        syn::Fields::Unit => {
            automata_item.fields = Fields::Named(::syn::parse_quote!({ #field_to_add }));
        }
    };

    Ok(type_param_ident)
}


/// Enumeration describing a function's receiver kind.
///
/// Possible kinds are:
/// - `self`
/// - `mut self`
/// - `&self`
/// - `&mut self`
/// - `T`/`&T`/`&mut T`
#[derive(Debug)]
enum ReceiverKind {
    /// Receiver takes ownership of `self`.
    OwnedSelf,
    /// Receiver takes mutable ownership of `self`.
    MutOwnedSelf,
    /// Receiver takes a reference to `self`.
    RefSelf,
    /// Receiver takes a mutable reference to `self`.
    MutRefSelf,
    /// Receiver takes any other type.
    Other,
}

/// Enumeration describing a function's output kind in regards to existing states.
///
/// Possible kinds are:
/// - `()`
/// - `State`
/// - `T`
#[derive(Debug)]
enum OutputKind {
    /// Function does not return a value (i.e. Java's `void`).
    Unit,
    /// Function returns a `T` which is a valid state.
    ///
    /// Note: `&T` or `&mut T` are not valid states.
    State(Ident),
    /// Any other `T`.
    Other,
}

/// Enumeration describing a function's kind in regard to the typestate state machine.
///
/// Possible kinds are:
/// - `fn() -> State`
/// - `fn(self) -> T`
/// - `fn(self) -> State`
/// - `fn(&self) -> T` or `fn(&mut self) -> T`
#[derive(Debug)]
enum FnKind {
    /// Function that does not take `self` and returns a valid state.
    Initial(Ident),
    /// Function that consumes `self` and does not return a valid state.
    Final,
    /// Function that consumes `self` and returns a valid state.
    Transition(Ident),
    /// Function that takes a reference (mutable or not) to `self`, it cannot return a state.
    SelfTransition,
    /// Other kinds of functions
    Other,
}

/// Provides a series of utility methods to be used on [syn::Signature].
trait SignatureKind {
    /// Extract a [ReceiverKind] from a [syn::Signature].
    fn extract_receiver_kind(&self) -> ReceiverKind;
    /// Extract a [OutputKind] from a [syn::Signature].
    fn extract_output_kind(&self, states: &HashSet<Ident>) -> OutputKind;
    /// Extract a [FnKind] from a [syn::Signature].
    /// Takes a set of states to check for valid states.
    fn extract_signature_kind(&self, states: &HashSet<Ident>) -> FnKind;
    /// Expands a signature
    /// (e.g. `fn f() -> State => fn f() -> Automata<State>`).
    fn expand_signature_state(&mut self, info: &StateMachineInfo);
}

impl SignatureKind for Signature {
    fn extract_receiver_kind(&self) -> ReceiverKind {
        let fn_in = &self.inputs;
        if let Some(FnArg::Receiver(Receiver {
            reference,
            mutability,
            ..
        })) = fn_in.first()
        {
            match (reference, mutability) {
                (None, None) => ReceiverKind::OwnedSelf,
                (None, Some(_)) => ReceiverKind::MutOwnedSelf,
                (Some(_), None) => ReceiverKind::RefSelf,
                (Some(_), Some(_)) => ReceiverKind::MutRefSelf,
            }
        } else {
            ReceiverKind::Other
        }
    }

    // the `states: HashMap<Ident, ItemStruct>` kinda sucks
    // making a `Contains` trait with a `contains` method and implement that for `HashMap<T, _>`
    // would probably be better
    fn extract_output_kind(&self, states: &HashSet<Ident>) -> OutputKind {
        let fn_out = &self.output;
        match fn_out {
            ReturnType::Default => OutputKind::Unit,
            ReturnType::Type(_, ty) => match **ty {
                Type::Path(ref path) => {
                    if let Some(ident) = path.path.get_ident() {
                        if states.contains(ident) {
                            return OutputKind::State(ident.clone());
                        }
                    }
                    return OutputKind::Other;
                }
                _ => return OutputKind::Other,
            },
        }
    }

    fn extract_signature_kind(&self, states: &HashSet<Ident>) -> FnKind {
        let recv = self.extract_receiver_kind();
        let out = self.extract_output_kind(states);
        match (recv, out) {
            (ReceiverKind::OwnedSelf, OutputKind::State(ident))
            | (ReceiverKind::MutOwnedSelf, OutputKind::State(ident)) => FnKind::Transition(ident),
            (ReceiverKind::OwnedSelf, _) | (ReceiverKind::MutOwnedSelf, _) => FnKind::Final,
            (ReceiverKind::RefSelf, _) | (ReceiverKind::MutRefSelf, _) => FnKind::SelfTransition,
            (ReceiverKind::Other, OutputKind::State(ident)) => FnKind::Initial(ident),
            (ReceiverKind::Other, _) => FnKind::Other,
        }
    }

    fn expand_signature_state(&mut self, info: &StateMachineInfo) {
        let fn_out = &mut self.output;
        let det_states = &info.det_states;
        match fn_out {
            ReturnType::Type(_, ty) => match **ty {
                Type::Path(ref mut path) => {
                    if let Some(ident) = path.path.get_ident() {
                        if det_states.contains_key(ident) {
                            let automata_ident = info.main_state_name();
                            path.path = ::syn::parse_quote!(#automata_ident<#ident>);
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }
}

enum TypestateError {
    MissingAutomata,
    NonProductiveState(Ident),
    NonUsefulState(Ident),
    MissingInitialState,
    MissingFinalState,
    ConflictingAttributes(Attribute),
    DuplicateAttributes(Attribute),
    AutomataRedefinition(ItemStruct),
    InvalidVariant(Ident),
    UnsupportedVariant(Variant),
    UnknownState(Ident),
    InvalidAssocFuntions(ItemTrait),
    UnsupportedStruct(ItemStruct),
}

impl Into<::syn::Error> for TypestateError {
    fn into(self) -> ::syn::Error {
        match self {
            TypestateError::MissingAutomata => Error::new(Span::call_site(), "Missing `#[automata]` struct."),
            TypestateError::NonProductiveState(ident) => Error::new_spanned(ident, "Non-productive state. For a state to be productive, a path from the state to a final state is required to exist."),
            TypestateError::NonUsefulState(ident) => Error::new_spanned(ident, "Non-useful state. For a state to be useful it must first be productive and a path from initial state to the state is required to exist."),
            TypestateError::MissingInitialState => Error::new(Span::call_site(), "Missing initial state. To declare an initial state you can use a function with signature like `fn f() -> T` where `T` is a declared state."),
            TypestateError::MissingFinalState => Error::new(Span::call_site(), "Missing final state. To declare a final state you can use a function with signature like `fn f(self) -> T` where `T` is not a declared state."),
            TypestateError::ConflictingAttributes(attr) => Error::new_spanned(attr, "Conflicting attributes are declared."), // TODO add which attributes are conflicting
            TypestateError::DuplicateAttributes(attr) => Error::new_spanned(attr, "Duplicate attribute."),
            TypestateError::AutomataRedefinition(item_struct) => Error::new_spanned(item_struct, "`#[automata]` redefinition here."),
            TypestateError::InvalidVariant(ident) => Error::new_spanned(&ident, "`enum` variant is not a valid state."),
            TypestateError::UnsupportedVariant(variant) => Error::new_spanned(&variant, "Only unit (C-like) `enum` variants are supported."),
            TypestateError::UnknownState(ident) => Error::new_spanned(&ident, format!("`{}` is not a declared state.", ident)),
            TypestateError::InvalidAssocFuntions(item_trait) => Error::new_spanned(&item_trait, "Non-deterministic states cannot have associated functions"),
            TypestateError::UnsupportedStruct(item_struct) => Error::new_spanned(&item_struct, "Tuple structures are not supported."),
        }
    }
}

impl IntoCompileError for TypestateError {
    fn to_compile_error(self) -> TokenStream2 {
        let err: syn::Error = self.into();
        err.to_compile_error()
    }
}
