mod igraph;
mod visitors;

use crate::{
    igraph::{
        validate::{GenericAutomaton, NonProductiveStates, NonUsefulStates, Validate},
        IntermediateGraph,
    },
    visitors::state::AUTOMATA_ATTR_IDENT,
};
use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, ToTokens};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};
use syn::{
    parse_macro_input, Attribute, AttributeArgs, Error, Ident, Item, ItemEnum, ItemMod, ItemStruct,
    ItemTrait, Variant
};

const CRATE_NAME: &str = "typestate_proc_macro";
const GENERATED_ATTR_IDENT: &str = "generated";

#[doc(hidden)]
#[proc_macro_attribute]
pub fn generated(_: TokenStream, input: TokenStream) -> TokenStream {
    input
}

fn generated_attr() -> TokenStream2 {
    let crate_ident = ::quote::format_ident!("{}", crate::CRATE_NAME);
    let generated_ident = ::quote::format_ident!("{}", crate::GENERATED_ATTR_IDENT);
    ::quote::quote!(#[::typestate::#crate_ident::#generated_ident])
}

trait IsGeneratedAttr {
    fn is_generated_attr(&self) -> bool;
}

impl IsGeneratedAttr for syn::Attribute {
    fn is_generated_attr(&self) -> bool {
        let segments = &self.path.segments.iter().collect::<Vec<_>>();
        if segments.len() == 2 {
            // support ::typestate_proc_macro::generated
            segments[0].ident == CRATE_NAME && segments[1].ident == GENERATED_ATTR_IDENT
        } else if segments.len() == 3 {
            // support ::typestate::typestate_proc_macro::generated
            segments[0].ident == "typestate"
                && segments[1].ident == CRATE_NAME
                && segments[2].ident == GENERATED_ATTR_IDENT
        } else {
            false
        }
    }
}


/// See the module documentation for a full featured tutorial on how to use `#[typestate]`.
#[allow(clippy::too_many_lines)] // TODO handle this
#[proc_macro_attribute]
pub fn typestate(args: TokenStream, input: TokenStream) -> TokenStream {
    macro_rules! bail_if_any {
        ( $errors:expr ) => {
            match $errors {
                errors => {
                    if !errors.is_empty() {
                        return errors.into_compile_error().into();
                    }
                }
            }
        };
    }

    // Parse attribute arguments
    let attr_args: AttributeArgs = parse_macro_input!(args);

    let args = match MacroAttributeArguments::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    // parse the input as a mod
    let mut module: ItemMod = parse_macro_input!(input);

    // eprint!("{:#?}", module);

    if let Some((_, content)) = &module.content {
        if content.is_empty() {
            return quote::quote!(const _: () = {
                #[deprecated(note = "Empty module detected.")]
                #[allow(nonstandard_style)]
                enum compile_warning {}

                let _: compile_warning;
            };).into_token_stream().into();
        }
    }

    let mut state_machine_info = StateMachineInfo::new();

    bail_if_any!(visitors::state::visit_states(
        &mut module,
        &mut state_machine_info,
        if !args.state_constructors.is_empty() {
            Some(format_ident!("{}", args.state_constructors))
        } else {
            None
        }
    ));

    // Visit non-deterministic transitions
    bail_if_any!(visitors::decision::visit_non_deterministic(
        &mut module,
        &mut state_machine_info
    ));

    // Visit transitions
    bail_if_any!(visitors::transition::visit_transitions(
        &mut module,
        &mut state_machine_info
    ));

    #[cfg(any(
        feature = "export-dot",
        feature = "export-plantuml",
        feature = "export-mermaid"
    ))]
    export_diagram_files(&state_machine_info);

    #[cfg(feature = "docs-mermaid")]
    {
        use igraph::export::{mermaid::Mermaid, Export};
        // NOTE: hacky bypass to avoid rewriting the ::Write
        let mut f = Vec::<u8>::new();

        // TODO: handle the unwrap
        state_machine_info
            .intermediate_automaton
            .export(&mut f, Mermaid)
            .unwrap();

        let doc_string = String::from_utf8(f).unwrap();
        let doc_string_iter = doc_string.split('\n').filter(|s| !s.is_empty());

        module = ::syn::parse_quote!(
            #[cfg_attr(doc, ::typestate::__private__::aquamarine::aquamarine)]
            #[doc = "```mermaid"]
            #(#[doc = #doc_string_iter])*
            #[doc = "```"]
            #module
        );
    }

    // TODO: deal with the unwrap later
    let automata_ident = state_machine_info.automaton_ident.unwrap();

    let ga = GenericAutomaton::from(state_machine_info.intermediate_automaton);
    let errors: Vec<Error> = ga
        .validate(NonProductiveStates)
        .into_iter()
        .map(|ident| TypestateError::NonProductiveState(ident).into())
        .collect();
    bail_if_any!(errors);

    let errors: Vec<Error> = ga
        .validate(NonUsefulStates)
        .into_iter()
        .map(|ident| TypestateError::NonUsefulState(ident).into())
        .collect();
    bail_if_any!(errors);

    let states = ga.states.iter().collect::<Vec<_>>();

    // match the `Option<Ident>`
    let mut enumerate_tokens = if !args.enumerate.is_empty() {
        let mut res: Vec<Item> = vec![];
        let enum_ident = &format_ident!("{}", &args.enumerate);
        res.expand_enumerate(
            &automata_ident,
            enum_ident,
            &states,
            &state_machine_info.non_det_transitions,
        );
        res
    } else {
        vec![]
    };

    if let Some((_, v)) = &mut module.content {
        v.append(&mut enumerate_tokens);
    }

    // if errors do not exist, return the token stream
    module.into_token_stream().into()
}

#[cfg(any(
    feature = "export-dot",
    feature = "export-plantuml",
    feature = "export-mermaid"
))]
fn export_diagram_files(state_machine_info: &StateMachineInfo) {
    use crate::igraph::export;

    #[cfg(feature = "export-dot")]
    {
        use igraph::export::dot::Dot;
        export::export(
            &state_machine_info
                .automaton_ident
                .clone()
                .unwrap()
                .ident
                .to_string(),
            &state_machine_info.intermediate_automaton,
            Dot,
        )
        .unwrap();
    }

    #[cfg(feature = "export-plantuml")]
    {
        use igraph::export::plantuml::PlantUml;
        export::export(
            &state_machine_info
                .automaton_ident
                .clone()
                .unwrap()
                .ident
                .to_string(),
            &state_machine_info.intermediate_automaton,
            PlantUml,
        )
        .unwrap();
    }

    #[cfg(feature = "export-mermaid")]
    {
        use igraph::export::mermaid::Mermaid;
        export::export(
            &state_machine_info
                .automaton_ident
                .clone()
                .unwrap()
                .ident
                .to_string(),
            &state_machine_info.intermediate_automaton,
            Mermaid,
        )
        .unwrap();
    }
}

trait ExpandEnumerate {
    fn expand_enumerate(
        &mut self,
        automata: &ItemStruct,
        automata_enum: &Ident,
        states: &[&Ident],
        non_det_transitions: &HashMap<Ident, ItemEnum>,
    );
    /// Expand the [`ToString`] implentation for enumeration.
    /// Only available with `std` and when `enumerate` is used.
    fn expand_to_string(&mut self, automata_enum: &Ident, states: &[&Ident]);
    /// Expand the enumeration containing all states.
    /// Only available when `enumerate` is used.
    fn expand_enum(&mut self, automata: &ItemStruct, automata_enum: &Ident, states: &[&Ident]);
    /// Expand the [`From`] implementation to convert from states to enumeration and back.
    /// Only available when `enumerate` is used.
    fn expand_from(&mut self, automata: &ItemStruct, automata_enum: &Ident, states: &[&Ident]);
    /// Expand the [`From`] implementations to convert from non-deterministic transitions to enumeration.
    /// Only available when `enumerate` is used.
    fn expand_non_det_from(
        &mut self,
        automata: &ItemStruct,
        automata_enum: &Ident,
        non_det_transitions: &HashMap<Ident, ItemEnum>,
    );
}

impl ExpandEnumerate for Vec<Item> {
    fn expand_enumerate(
        &mut self,
        automata: &ItemStruct,
        automata_enum: &Ident,
        states: &[&Ident],
        non_det_transitions: &HashMap<Ident, ItemEnum>,
    ) {
        // expand the enumeration
        self.expand_enum(automata, automata_enum, states);

        // expand conversion traits: `From`
        self.expand_from(automata, automata_enum, states);
        self.expand_non_det_from(automata, automata_enum, non_det_transitions);

        // if std is present, generate `to_string` implementations
        #[cfg(feature = "std")]
        self.expand_to_string(automata_enum, states);
    }

    fn expand_to_string(&mut self, automata_enum: &Ident, states: &[&Ident]) {
        let to_string = ::quote::quote! {
            impl ::std::string::ToString for #automata_enum {
                fn to_string(&self) -> String {
                    match &self {
                        #(#automata_enum::#states(_) => stringify!(#states).to_string(),)*
                    }
                }
            }
        };
        self.push(::syn::parse_quote!(#to_string));
    }

    fn expand_from(&mut self, automata: &ItemStruct, automata_enum: &Ident, states: &[&Ident]) {
        let automata_ident = &automata.ident;
        let from_tokens = states.iter().map(|state| {
            ::syn::parse_quote! {
                impl ::core::convert::From<#automata_ident<#state>> for #automata_enum {
                    fn from(value: #automata_ident<#state>) -> Self {
                        Self::#state(value)
                    }
                }
            }
        });
        self.extend(from_tokens);
    }

    fn expand_non_det_from(
        &mut self,
        automata: &ItemStruct,
        automata_enum: &Ident,
        non_det_transitions: &HashMap<Ident, ItemEnum>,
    ) {
        let from_tokens = non_det_transitions
            .iter()
            .map(|(transition_enum, states_variants)| {
                let states = states_variants.variants.iter().map(|variant| variant.ident.clone());
                ::syn::parse_quote! {
                    impl ::core::convert::From<#transition_enum> for #automata_enum {
                        fn from(value: #transition_enum) -> Self {
                            match value {
                                #(#transition_enum::#states(state) => Self::#states(state)),*
                            }
                        }
                    }
                }
            });
        self.extend(from_tokens);
    }

    fn expand_enum(&mut self, automata: &ItemStruct, automata_enum: &Ident, states: &[&Ident]) {
        let automata_ident = &automata.ident;
        let automata_vis = &automata.vis;
        self.push(::syn::parse_quote! {
            #automata_vis enum #automata_enum {
                #(#states(#automata_ident<#states>),)*
            }
        });
    }
}

#[derive(Debug, FromMeta)]
struct MacroAttributeArguments {
    /// Optional arguments.
    #[darling(default)]
    enumerate: String,
    #[darling(default)]
    state_constructors: String,
}

/// A value to `proc_macro2::TokenStream2` conversion.
/// More precisely into
trait IntoCompileError {
    fn into_compile_error(self) -> TokenStream2;
}

impl IntoCompileError for Vec<Error> {
    fn into_compile_error(mut self) -> TokenStream2 {
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
        }
        TokenStream2::new()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
#[derive(Debug, Clone)]
struct StateMachineInfo {
    /// Main structure (aka Automata ?)
    automaton_ident: Option<ItemStruct>, // late init

    /// Deterministic states (`struct`s)
    det_states: HashMap<Ident, ItemStruct>,

    /// Non-deterministic transitions (`enum`s)
    non_det_transitions: HashMap<Ident, ItemEnum>,

    /// Non-deterministic transitions present in this collection are used.
    /// This is just so we can throw an error on unused enumerations.
    used_non_det_transitions: HashSet<Ident>,

    /// Set of transitions.
    /// Extracted from functions with a signature like `(State) -> State`.
    transitions: HashSet<Transition>,

    /// Set of initial states.
    /// Extracted from functions with a signature like `() -> State`.
    initial_states: HashMap<Ident, HashSet<Ident>>,

    /// Set of final states.
    /// Extracted from functions with a signature like `(State) -> ()`.
    final_states: HashMap<Ident, HashSet<Ident>>,

    pub intermediate_automaton: IntermediateGraph<Ident, Ident>,
}

impl StateMachineInfo {
    /// Construct a new [`StateMachineInfo`].
    fn new() -> Self {
        Self {
            automaton_ident: None,
            intermediate_automaton: IntermediateGraph::new(),
            det_states: HashMap::new(),
            non_det_transitions: HashMap::new(),
            used_non_det_transitions: HashSet::new(),
            transitions: HashSet::new(),
            initial_states: HashMap::new(),
            final_states: HashMap::new(),
        }
    }

    /// Add a generic state to the [`StateMachineInfo`]
    fn add_state(&mut self, state: Item) {
        match state {
            Item::Struct(item_struct) => {
                self.det_states
                    .insert(item_struct.ident.clone(), item_struct);
            }
            Item::Enum(item_enum) => {
                self.non_det_transitions
                    .insert(item_enum.ident.clone(), item_enum);
            }
            _ => unreachable!("invalid state"),
        }
    }

    /// Return the main state identifier.
    /// This is an utility function.
    // maybe the unwrap could be converted into a check
    // if none -> comp time error
    fn get_automaton_ident(&self) -> &Ident {
        &self.automaton_ident.as_ref().unwrap().ident
    }

    /// Check for missing initial or final states.
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

    /// Check for unused non-deterministic transitions
    fn check_unused_non_det_transitions(&self) -> Vec<Error> {
        self.non_det_transitions
            .keys()
            .collect::<HashSet<_>>()
            .difference(
                // HACK
                &self.used_non_det_transitions.iter().collect::<HashSet<_>>(),
            )
            .collect::<Vec<_>>()
            .iter()
            .map(|i| TypestateError::UnusedTransition((***i).clone()).into())
            .collect::<Vec<_>>()
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

enum TypestateError {
    MissingAutomata,
    NonProductiveState(Ident),
    NonUsefulState(Ident),
    MissingInitialState,
    MissingFinalState,
    ConflictingAttributes(Attribute),
    DuplicateAttributes(Attribute),
    AutomataRedefinition(ItemStruct),
    UndeclaredVariant(Ident),
    UnsupportedVariant(Variant),
    UnknownState(Ident),
    InvalidAssocFuntions(ItemTrait),
    UnsupportedStruct(ItemStruct),
    UnsupportedState(Ident),
    UnusedTransition(Ident),
}

impl From<TypestateError> for syn::Error {
    fn from(err: TypestateError) -> Self {
        match err {
            TypestateError::MissingAutomata => Error::new(Span::call_site(), format!("Missing `#[{}]` struct.", AUTOMATA_ATTR_IDENT)),
            TypestateError::NonProductiveState(ident) => Error::new_spanned(ident, "Non-productive state. For a state to be productive, a path from the state to a final state is required to exist."),
            TypestateError::NonUsefulState(ident) => Error::new_spanned(ident, "Non-useful state. For a state to be useful it must first be productive and a path from initial state to the state is required to exist."),
            TypestateError::MissingInitialState => Error::new(Span::call_site(), "Missing initial state. To declare an initial state you can use a function with signature like `fn f() -> T` where `T` is a declared state."),
            TypestateError::MissingFinalState => Error::new(Span::call_site(), "Missing final state. To declare a final state you can use a function with signature like `fn f(self) -> T` where `T` is not a declared state."),
            TypestateError::ConflictingAttributes(attr) => Error::new_spanned(attr, "Conflicting attributes are declared."), // TODO add which attributes are conflicting
            TypestateError::DuplicateAttributes(attr) => Error::new_spanned(attr, "Duplicate attribute."),
            TypestateError::AutomataRedefinition(item_struct) => Error::new_spanned(item_struct, format!("`#[{}]` redefinition here.", AUTOMATA_ATTR_IDENT)),
            TypestateError::UndeclaredVariant(ident) => Error::new_spanned(&ident, "`enum` variant is not a declared state."),
            TypestateError::UnsupportedVariant(variant) => Error::new_spanned(&variant, "Only unit (C-like) `enum` variants are supported."),
            TypestateError::UnknownState(ident) => Error::new_spanned(&ident, format!("`{}` is not a declared state.", ident)),
            TypestateError::InvalidAssocFuntions(item_trait) => Error::new_spanned(&item_trait, "Non-deterministic states cannot have associated functions"),
            TypestateError::UnsupportedStruct(item_struct) => Error::new_spanned(&item_struct, "Tuple structures are not supported."),
            TypestateError::UnsupportedState(ident) => Error::new_spanned(&ident, "`enum` variants cannot refer to other `enum`s."),
            TypestateError::UnusedTransition(ident) => Error::new_spanned(&ident, "Unused transitions are not allowed."),
        }
    }
}

impl IntoCompileError for TypestateError {
    fn into_compile_error(self) -> TokenStream2 {
        let err: syn::Error = self.into();
        err.to_compile_error()
    }
}
