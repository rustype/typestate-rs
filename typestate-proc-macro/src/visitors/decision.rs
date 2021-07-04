use darling::FromMeta;
use syn::{visit_mut::VisitMut, Error, Fields, Ident, ItemEnum, ItemMod, Variant};

use crate::{intermediate_graph::Metadata, StateMachineInfo, TypestateError};

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
}

impl<'sm> VisitMut for DecisionVisitor<'sm> {
    fn visit_item_enum_mut(&mut self, i: &mut ItemEnum) {
        let mut v = DecisionVariantVisitor::new(self.state_machine_info);
        for variant in &mut i.variants {
            v.visit_variant_mut(variant);
        }
        if v.errors.is_empty() {
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
        } else {
            self.errors.extend(v.errors);
        }
    }
}

struct DecisionVariantVisitor<'a> {
    info: &'a mut StateMachineInfo,
    metadata: Option<Metadata>,
    errors: Vec<Error>,
}

impl<'sm> DecisionVariantVisitor<'sm> {
    fn new(info: &'sm mut StateMachineInfo) -> Self {
        Self {
            info,
            metadata: None,
            errors: vec![],
        }
    }

    fn parse_metadata(&mut self, variant: &mut Variant) {
        variant.attrs.retain(|attr| {
            if attr.path.is_ident("metadata") {
                match attr.parse_meta() {
                    Ok(meta) => match Metadata::from_meta(&meta) {
                        Ok(metadata) => self.metadata = Some(metadata),
                        // TODO
                        Err(_err) => todo!("make enum for the errors or something"),
                    },
                    Err(err) => self.errors.push(err),
                }
                false
            } else {
                true
            }
        })
    }

    fn parse_variant(&mut self, variant: &mut Variant) {
        // check if the variant is a valid one
        // i.e. unit-style variant
        if let Fields::Unit = &variant.fields {
            let ident = &variant.ident;
            // check if the current ident is a valid state or another decision node
            if self.info.non_det_transitions.contains_key(ident) {
                self.push_unsupported_state_error(ident);
            } else if self.info.det_states.contains_key(ident) {
                let automata_ident = self.info.get_automaton_ident();
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

impl VisitMut for DecisionVariantVisitor<'_> {
    fn visit_variant_mut(&mut self, variant: &mut Variant) {
        self.parse_metadata(variant);
        self.parse_variant(variant);
    }
}
