use proc_macro2::TokenStream as TokenStream2;
use syn::{
    parse_macro_input, visit_mut::VisitMut, Error, Fields, Ident, ItemEnum, ItemMod, Variant,
};

use crate::{StateMachineInfo, TypestateError};

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
    let mut non_det_state_visitor = DecisionVisitor::new(state_machine_info);
    non_det_state_visitor.visit_item_mod_mut(module);
    // report non_det_state_visitor errors and return
    bail_if_any!(non_det_state_visitor.errors);
    vec![]
}

struct DecisionVisitor<'sm> {
    state_machine_info: &'sm mut StateMachineInfo,
    errors: Vec<Error>,
}

impl<'sm> DecisionVisitor<'sm> {
    fn new(state_machine_info: &'sm mut StateMachineInfo) -> Self {
        Self {
            state_machine_info,
            errors: vec![],
        }
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
        for variant in &mut i.variants {
            // let attrs = variant.attrs;

            // let mut transition_metadata = None;
            // for attr in attrs {
            //     if attr.path.is_ident("metadata") {
            //         let attr_tokens = attr.into();
            //         let metadata: TransitionMetadata = parse_macro_input!(attr_tokens);
            //         transition_metadata = metadata.into();
            //     }
            // }
            // eprintln!("{:#?}", transition_metadata);

            // check if the variant is a valid one
            // i.e. unit-style variant
            if let Fields::Unit = &variant.fields {
                let ident = &variant.ident;
                // check if the current ident is a valid state or another decision node
                if self
                    .state_machine_info
                    .non_det_transitions
                    .contains_key(ident)
                {
                    self.push_unsupported_state_error(ident);
                } else if self.state_machine_info.det_states.contains_key(ident) {
                    let automata_ident = self.state_machine_info.get_automaton_ident();
                    variant.fields = Fields::Unnamed(::syn::parse_quote!(
                        /* Variant */ (
                            #automata_ident<#ident>
                        )
                    ));
                } else {
                    self.push_undeclared_variant_error(ident);
                }
            } else {
                self.push_unsupported_variant_error(variant);
            }
        }
        if self.errors.is_empty() {
            let enum_ident = i.ident.clone();
            let destination_idents: Vec<Ident> =
                i.variants.iter().cloned().map(|v| v.ident).collect();
            // TODO: implement https://github.com/rustype/typestate-rs/issues/3
            // NOTE: this could be `Option<Ident>`

            self.state_machine_info
                .intermediate_automaton
                .add_transition(
                    enum_ident.clone().into(),
                    enum_ident.clone().into(),
                    destination_idents.into(),
                );

            // self.state_machine_info.add_state(i.clone().into());
            self.state_machine_info
                .non_det_transitions
                .insert(enum_ident, i.clone());
        }
    }
}
