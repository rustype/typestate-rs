use std::collections::HashSet;

use darling::FromMeta;
use syn::{visit_mut::VisitMut, Error, Fields, Generics, Ident, ItemEnum, ItemMod, Variant};

use crate::{
    igraph::{Metadata, StateNode},
    StateMachineInfo, TypestateError,
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

pub(crate) fn visit_non_deterministic(
    module: &mut ItemMod,
    state_machine_info: &mut StateMachineInfo,
) -> Vec<Error> {
    let mut decision_visitor = DecisionVisitor::new(state_machine_info);
    decision_visitor.visit_item_mod_mut(module);
    // report non_det_state_visitor errors and return
    bail_if_any!(decision_visitor.errors);
    vec![]
}

struct DecisionVisitor<'sm> {
    state_machine_info: &'sm mut StateMachineInfo,
    decision_generics: HashSet<Generics>,
    errors: Vec<Error>,
}

impl<'sm> DecisionVisitor<'sm> {
    fn new(state_machine_info: &'sm mut StateMachineInfo) -> Self {
        Self {
            state_machine_info,
            decision_generics: HashSet::new(),
            errors: vec![],
        }
    }

    fn visit_variant_mut(&mut self, variant: &mut Variant) -> Option<StateNode<Ident>> {
        if let Fields::Unit = &variant.fields {
            let det_states = &self.state_machine_info.det_states;
            let ident = &variant.ident;
            // check if the current ident is a valid state or another decision node
            if self
                .state_machine_info
                .non_det_transitions
                .contains_key(ident)
            {
                self.push_unsupported_state_error(ident);
            } else if let Some(it_struct) = det_states.get(ident) {
                let mut state = StateNode::new(Some(ident.clone()));
                let mut errors = vec![];
                variant.attrs.retain(|attr| {
                    if attr.path.is_ident("metadata") {
                        match attr.parse_meta() {
                            Ok(meta) => match Metadata::from_meta(&meta) {
                                Ok(metadata) => state.update_metadata(metadata),
                                Err(err) => {
                                    // TODO fix this hack
                                    errors.push(Error::new_spanned(attr, err.to_string()))
                                }
                            },
                            Err(err) => errors.push(err),
                        }
                        false
                    } else {
                        true
                    }
                });
                self.errors.append(&mut errors);

                let automata_ident = self.state_machine_info.get_automaton_ident();
                let generics = &it_struct.generics;
                self.decision_generics.insert(generics.clone());
                variant.fields = Fields::Unnamed(::syn::parse_quote!(
                    /* Variant */ (
                        #automata_ident<#ident #generics>
                    )
                ));

                return Some(state);
            } else {
                self.push_undeclared_variant_error(ident);
            }
        } else {
            self.push_unsupported_variant_error(variant);
        }
        None
    }

    /// Add `undeclared state` error to the error vector.
    fn push_undeclared_variant_error(&mut self, ident: &Ident) {
        self.errors
            .push(TypestateError::UndeclaredVariant(ident.clone()).into());
    }

    /// Add `unsupported variant` error to the error vector.
    fn push_unsupported_variant_error(&mut self, variant: &Variant) {
        self.errors
            .push(TypestateError::UnsupportedVariant(variant.clone()).into());
    }

    /// Add `unsupported state` error to the error vector.
    fn push_unsupported_state_error(&mut self, ident: &Ident) {
        self.errors
            .push(TypestateError::UnsupportedState(ident.clone()).into());
    }
}

impl<'sm> VisitMut for DecisionVisitor<'sm> {
    fn visit_item_enum_mut(&mut self, i: &mut ItemEnum) {
        let enum_ident = i.ident.clone();

        let destination_idents: Option<Vec<_>> = i
            .variants
            .iter_mut()
            .map(|v| self.visit_variant_mut(v))
            .collect();

        // NOTE: this could be `Option<Ident>`

        if let Some(dest) = destination_idents {
            self.state_machine_info
                .intermediate_automaton
                .add_choice(enum_ident.clone());
            self.state_machine_info
                .intermediate_automaton
                .add_transition(
                    enum_ident.clone().into(),
                    enum_ident.clone().into(),
                    dest.into(),
                );

            self.state_machine_info
                .non_det_transitions
                .insert(enum_ident, i.clone());
        }
    }
}
