pub mod export;
pub mod validate;

use darling::FromMeta;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

#[derive(Debug, Clone)]
pub struct StateNode<S> {
    state: Option<S>,
    metadata: Metadata,
}

impl<S> StateNode<S> {
    pub fn new(state: Option<S>) -> Self {
        Self {
            state,
            metadata: Metadata::empty(),
        }
    }

    pub fn update_metadata(&mut self, metadata: Metadata) {
        self.metadata = metadata;
    }
}

#[derive(Debug, Clone)]
pub enum Node<S>
where
    S: Hash + Eq + Debug + Clone + Display,
{
    State(StateNode<S>),
    Decision(Vec<StateNode<S>>), // NOTE: instead of Vec<_>, HashSet<_> would probably be better
}

impl<S> From<S> for Node<S>
where
    S: Hash + Eq + Debug + Clone + Display,
{
    fn from(s: S) -> Self {
        Node::State(StateNode::new(Some(s)))
    }
}

impl<S> From<Option<S>> for Node<S>
where
    S: Hash + Eq + Debug + Clone + Display,
{
    fn from(s: Option<S>) -> Self {
        Node::State(StateNode::new(s))
    }
}

impl<S> From<Vec<S>> for Node<S>
where
    S: Hash + Eq + Debug + Clone + Display,
{
    fn from(s: Vec<S>) -> Self {
        Node::Decision(s.into_iter().map(|s| StateNode::new(Some(s))).collect())
    }
}

impl<S> From<Vec<StateNode<S>>> for Node<S>
where
    S: Hash + Eq + Debug + Clone + Display,
{
    fn from(s: Vec<StateNode<S>>) -> Self {
        Node::Decision(s)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    transition: T,
}

impl<T> Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    pub fn new(transition: T) -> Self {
        Self { transition }
    }
}

impl<T> Display for Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.transition))
    }
}

impl<T> From<T> for Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

/// Metadata associated with nodes and transitions,
/// to be used as additional annotations.
#[derive(Debug, Clone, Hash, PartialEq, Eq, FromMeta)]
pub struct Metadata {
    #[darling(rename = "label")]
    transition_label: Option<String>,
}

impl Metadata {
    fn empty() -> Self {
        Self {
            transition_label: None,
        }
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Debug, Clone)]
pub struct IntermediateGraph<S, T>
where
    // State type parameter.
    S: Hash + Eq + Debug + Clone + Display,
    // Transition type parameter.
    T: Hash + Eq + Debug + Clone + Display,
{
    states: HashSet<S>,
    choices: HashSet<S>,
    delta: HashMap<Option<S>, HashMap<Transition<T>, Node<S>>>,
}

impl<S, T> IntermediateGraph<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            choices: HashSet::new(),
            delta: HashMap::new(),
        }
    }

    pub fn add_state(&mut self, state: S) -> bool {
        self.states.insert(state)
    }

    pub fn add_choice(&mut self, choice: S) -> bool {
        self.choices.insert(choice)
    }

    pub fn add_transition(
        &mut self,
        source: Option<S>,
        transition: Transition<T>,
        destinations: Node<S>,
    ) {
        if let Some(source_value) = self.delta.get_mut(&source) {
            // NOTE: multi-valued transitions are disallowed because Rust does not support overloading,
            // thus, one cannot write function `f` for the same `Self` type with different signatures.
            source_value.insert(transition, destinations);
        } else {
            let mut transitions = HashMap::new();
            transitions.insert(transition, destinations);
            self.delta.insert(source, transitions);
        }
    }
}

impl<S, T> Default for IntermediateGraph<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    fn default() -> Self {
        Self::new()
    }
}
