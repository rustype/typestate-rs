use std::collections::HashSet;

use crate::{CRATE_NAME, GENERATED_ATTR_IDENT, StateMachineInfo, Transition, TypestateError, intermediate_graph::Node};
use syn::{
    visit_mut::VisitMut, Error, FnArg, Ident, ItemMod, ItemTrait, Receiver, ReturnType, Signature,
    TraitItemMethod, Type,
};

macro_rules! bail_if_any {
    ( $errors:expr ) => {
        match $errors {
            errors => {
                if !errors.is_empty() {
                    return errors;
                }
            }
        }
    };
}

pub(crate) fn visit_transitions(
    module: &mut ItemMod,
    state_machine_info: &mut StateMachineInfo,
) -> Vec<Error> {
    // Visit transitions
    let mut transition_visitor = TransitionVisitor::new(state_machine_info);
    transition_visitor.visit_item_mod_mut(module);

    // report transition_visitor errors and return
    bail_if_any!(transition_visitor.errors);
    bail_if_any!(state_machine_info.check_missing());
    bail_if_any!(state_machine_info.check_unused_non_det_transitions());
    vec![]
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
            .push(TypestateError::UnknownState(ident.clone()).into());
    }

    fn push_invalid_trait_error(&mut self, it: &ItemTrait) {
        self.errors
            .push(TypestateError::InvalidAssocFuntions(it.clone()).into());
    }
}

impl<'sm> VisitMut for TransitionVisitor<'sm> {
    fn visit_item_trait_mut(&mut self, i: &mut ItemTrait) {
        let attributes = &i.attrs;
        // check if there is a `#[generated]` attribute
        // if there is, do not process this trait
        if attributes.iter().any(|attr| {
            let segments = &attr.path.segments;
            let segments = segments.iter().collect::<Vec<_>>();
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
        }) {
            return;
        }

        let ident = &i.ident;

        if self
            .state_machine_info
            .non_det_transitions
            .contains_key(ident)
        {
            self.push_invalid_trait_error(i);
            return;
        }

        if self.state_machine_info.det_states.contains_key(ident) {
            self.current_state = Some(ident.clone());
            i.ident = ::quote::format_ident!("{}State", ident);
            // go deeper
            for item in &mut i.items {
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
        self.state_machine_info.det_states.keys().for_each(|k| {
            states.insert(k.clone()); // HACK clone
        });
        self.state_machine_info
            .non_det_transitions
            .keys()
            .for_each(|k| {
                states.insert(k.clone()); // HACK clone
            });
        let fn_kind = sig.extract_signature_kind(&states);
        let fn_ident = sig.ident.clone();
        sig.expand_signature_state(&self.state_machine_info); // TODO check for correct expansion

        match fn_kind {
            FnKind::Initial(return_ty_ident) => {
                // add a transition to an initial state
                // BOOK
                self.state_machine_info
                    .intermediate_automaton
                    .add_transition(None, fn_ident.clone().into(), return_ty_ident.clone().into());

                self.state_machine_info
                    .insert_initial(return_ty_ident, fn_ident);
            }
            FnKind::Final => {
                // add #[must_use]
                // attrs.push(::syn::parse_quote!(#[must_use]));
                let state = self.current_state.as_ref().unwrap().clone();

                // BOOK
                self.state_machine_info
                    .intermediate_automaton
                    .add_transition(Some(state.clone()), fn_ident.clone().into(), Node::State(None));

                self.state_machine_info.insert_final(state, fn_ident);
            }
            FnKind::Transition(return_ty_ident) => {
                // add #[must_use]
                attrs.push(::syn::parse_quote!(#[must_use]));
                let source = self.current_state.as_ref().unwrap().clone();
                // BOOK
                self.state_machine_info
                    .intermediate_automaton
                    .add_transition(
                        source.clone().into(),
                        fn_ident.clone().into(),
                        return_ty_ident.clone().into(),
                    );

                let transition = Transition::new(source, return_ty_ident.clone(), fn_ident);

                self.state_machine_info.transitions.insert(transition);
                // mark non det transition as used
                if self
                    .state_machine_info
                    .non_det_transitions
                    .contains_key(&return_ty_ident)
                {
                    self.state_machine_info
                        .used_non_det_transitions
                        .insert(return_ty_ident);
                }
            }
            FnKind::SelfTransition => {
                let state = self.current_state.as_ref().unwrap().clone();

                // BOOK
                self.state_machine_info
                    .intermediate_automaton
                    .add_transition(state.clone().into(), fn_ident.clone().into(), state.clone().into());

                let transition = Transition::new(state.clone(), state.clone(), fn_ident);
                self.state_machine_info.transitions.insert(transition);
                // mark non det transition as used
                if self
                    .state_machine_info
                    .non_det_transitions
                    .contains_key(&state)
                {
                    self.state_machine_info
                        .used_non_det_transitions
                        .insert(state);
                }
            }
            FnKind::Other => {}
        };
    }
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

/// Provides a series of utility methods to be used on [`syn::Signature`].
trait SignatureKind {
    /// Extract a [`ReceiverKind`] from a [`syn::Signature`].
    fn extract_receiver_kind(&self) -> ReceiverKind;
    /// Extract a [`OutputKind`] from a [`syn::Signature`].
    fn extract_output_kind(&self, states: &HashSet<Ident>) -> OutputKind;
    /// Extract a [`FnKind`] from a [`syn::Signature`].
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
                    OutputKind::Other
                }
                _ => OutputKind::Other,
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

        if let ReturnType::Type(_, ty) = fn_out {
            if let Type::Path(ref mut path) = **ty {
                if let Some(ident) = path.path.get_ident() {
                    if det_states.contains_key(ident) {
                        let automata_ident = info.get_automaton_ident();
                        path.path = ::syn::parse_quote!(#automata_ident<#ident>);
                    }
                }
            }
        }
    }
}
