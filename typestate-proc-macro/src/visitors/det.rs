use std::convert::TryFrom;

use crate::{StateMachineInfo, TypestateError, generated_attr};

use parse::Parser;
use syn::{
    parse, visit_mut::VisitMut, Attribute, Error, Field, Fields, Ident, Item, ItemMod, ItemStruct,
    Path,
};

const AUTOMATA_ATTR_IDENT: &str = "automata";
const STATE_ATTR_IDENT: &str = "state";

type Result<Ok, Err = Error> = ::core::result::Result<Ok, Err>;

// HACK return type
pub(crate) fn visit_states(
    module: &mut ItemMod,
    state_machine_info: &mut StateMachineInfo,
    constructor_ident: Option<Ident>,
) -> Vec<Error> {
    // start visitor
    let mut state_visitor = DeterministicStateVisitor::new(state_machine_info, constructor_ident);
    state_visitor.visit_item_mod_mut(module);
    // report state_visitor errors and return
    if !state_visitor.errors.is_empty() {
        return state_visitor.errors;
    }

    let mut constructors = state_visitor.constructors;
    if let Some((_, v)) = &mut module.content {
        v.append(&mut constructors);
    }

    let sealed_trait = state_visitor.sealed_trait;
    if sealed_trait.trait_ident.is_none() {
        return vec![TypestateError::MissingAutomata.into()];
    }

    match &mut module.content {
        Some((_, v)) => {
            v.append(&mut sealed_trait.into()); // HACK unwrap is safe because otherwise errors would've bailed
        }
        None => {}
    }

    vec![]
}

struct DeterministicStateVisitor<'sm> {
    /// State machine required information
    state_machine_info: &'sm mut StateMachineInfo,
    /// Sealed trait information
    sealed_trait: SealedPattern,
    /// Default constructors
    constructors: Vec<Item>,
    /// Default constructor ident
    constructor_ident: Option<Ident>,
    /// Errors found during expansion
    errors: Vec<Error>,
}

impl<'sm> DeterministicStateVisitor<'sm> {
    fn new(
        state_machine_info: &'sm mut StateMachineInfo,
        constructor_ident: Option<Ident>,
    ) -> Self {
        Self {
            state_machine_info,
            sealed_trait: SealedPattern::default(),
            constructors: vec![],
            constructor_ident,
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

#[derive(Default)]
pub(crate) struct SealedPattern {
    /// Ident for the sealed pattern public trait
    trait_ident: Option<Ident>, // late init
    /// Idents for the sealed elements.
    state_idents: Vec<Ident>,
}

// TODO rework this as an ExpandX trait
impl From<SealedPattern> for Vec<Item> {
    /// Convert the [`SealedTrait`] into a vector of Item.
    /// This enables the addition of new items to the main module.
    fn from(sealed_pattern: SealedPattern) -> Self {
        let trait_ident = sealed_pattern.trait_ident.expect("missing `.trait_ident`");
        let private_mod_ident = ::quote::format_ident!("__private");
        // or `Private` or `Sealed` or `format_ident!("{}Sealed", …)`
        // take into account that `trait_ident` may have already been used
        let private_mod_trait = &trait_ident;

        let generated_attr = generated_attr();

        let states = &sealed_pattern.state_idents;
        let mut ret = vec![
            // Sealed trait
            ::syn::parse_quote! {
                #generated_attr
                #[doc(hidden)]
                /* private */ mod #private_mod_ident {
                    /* to avoid the nested item being processed */
                    #generated_attr
                    pub trait #private_mod_trait {}
                }
            },
            // State trait
            ::syn::parse_quote! {
                #generated_attr
                pub trait #trait_ident: #private_mod_ident::#private_mod_trait {}
            },
            // Blanket impl of state trait from sealed implementors
            // This frees us from having to provide concrete impls for each type.
            ::syn::parse_quote! {
                #generated_attr
                impl<__T : ?::core::marker::Sized> #trait_ident
                    for __T
                where
                    __T : #private_mod_ident::#private_mod_trait,
                {}
            },
        ];

        // Sealed trait impls
        ret.extend(states.iter().map(|each_state| {
            ::syn::parse_quote! {
                #generated_attr
                impl #private_mod_ident::#private_mod_trait for #each_state {}
            }
        }));


        ret
    }
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
                match it_struct.expand_state_type_parameter() {
                    Ok(bound_ident) => match self.sealed_trait.trait_ident {
                        Some(_) => unreachable!("this should have been checked previously"),
                        None => self.sealed_trait.trait_ident = Some(bound_ident),
                    },
                    Err(e) => {
                        self.errors.push(e);
                    }
                }
            }
            Some(TypestateAttr::State) => {
                self.state_machine_info.add_state(it_struct.clone().into());
                self.sealed_trait.state_idents.push(it_struct.ident.clone());
                if let Some(ident) = &self.constructor_ident {
                    self.constructors
                        .expand_state_constructors(ident, it_struct);
                }
            }
            None => {
                // empty attribute list
            }
        }
    }
}

trait ExpandStateConstructors {
    fn expand_state_constructors(&mut self, constructor_ident: &Ident, item_struct: &ItemStruct);
}

impl ExpandStateConstructors for Vec<Item> {
    fn expand_state_constructors(&mut self, constructor_ident: &Ident, item_struct: &ItemStruct) {
        if let Fields::Named(named) = &item_struct.fields {
            let struct_ident = &item_struct.ident;
            let field_ident = named.named.iter().map(|field| &field.ident);
            let field_ident2 = named.named.iter().map(|field| &field.ident); // HACK
            let field_ty = named.named.iter().map(|field| &field.ty);
            self.push(::syn::parse_quote! {
                #[::typestate::generated]
                impl #struct_ident {
                    pub fn #constructor_ident(#(#field_ident: #field_ty,)*) -> Self {
                        Self {
                            #(#field_ident2,)*
                        }
                    }
                }
            });
        }
    }
}

trait ExpandState {
    /// Expand the state type parameter in a structure or other kind of item.
    fn expand_state_type_parameter(&mut self) -> syn::Result<Ident>;
}

impl ExpandState for ItemStruct {
    fn expand_state_type_parameter(&mut self) -> syn::Result<Ident> {
        // TODO make the suffix custom
        let type_param_ident = ::quote::format_ident!("{}State", self.ident);
        self.generics
            .params
            .push(::syn::parse_quote!(State: #type_param_ident));

        let field_to_add = ::quote::quote!(
            pub state: State
        );

        match &mut self.fields {
            syn::Fields::Named(named) => {
                named
                    .named
                    .push(Field::parse_named.parse2(field_to_add).unwrap());
            }
            syn::Fields::Unnamed(_) => {
                return syn::Result::Err(TypestateError::UnsupportedStruct(self.clone()).into());
            }
            syn::Fields::Unit => {
                self.fields = Fields::Named(::syn::parse_quote!({ #field_to_add }));
            }
        };

        Ok(type_param_ident)
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

#[derive(PartialEq)]
enum Attr {
    Retain,
    Discard,
}
