use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};
use darling::FromMeta;

#[derive(Debug, Clone)]
pub enum Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    State(Option<S>),
    Decision(Vec<S>),
}

impl<S> From<S> for Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    fn from(s: S) -> Self {
        Node::State(Some(s))
    }
}

impl<S> From<Vec<S>> for Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    fn from(s: Vec<S>) -> Self {
        Node::Decision(s)
    }
}

// TODO: consider whether `Hash`, `PartialEq` & `Eq` should only take `transition` into account.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Transition<T>
where
    T: Hash + Eq + Debug + Clone,
{
    transition: T,
    metadata: Option<TransitionMetadata>,
}

impl<T> Transition<T>
where
    T: Hash + Eq + Debug + Clone,
{
    fn new(transition: T) -> Self {
        Self {
            transition,
            metadata: None,
        }
    }

    fn with_metadata(transition: T, metadata: TransitionMetadata) -> Self {
        Self {
            transition,
            metadata: metadata.into(),
        }
    }
}

impl<T> From<T> for Transition<T>
where
    T: Hash + Eq + Debug + Clone,
{
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, FromMeta)]
pub struct TransitionMetadata {
    label: String,
}

impl TransitionMetadata {
    fn new(label: String) -> Self {
        Self { label }
    }
}

impl Default for TransitionMetadata {
    fn default() -> Self {
        Self::new(String::new())
    }
}

#[derive(Debug, Clone)]
pub struct IntermediateAutomaton<S, T>
where
    // State type parameter.
    S: Hash + Eq + Debug + Clone,
    // Transition type parameter.
    T: Hash + Eq + Debug + Clone,
{
    states: HashSet<S>,
    delta: HashMap<Option<S>, HashMap<Transition<T>, Node<S>>>,
}

impl<S, T> IntermediateAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone,
    T: Hash + Eq + Debug + Clone,
{
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            delta: HashMap::new(),
        }
    }

    pub fn add_state(&mut self, state: S) -> bool {
        self.states.insert(state)
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

impl<S, T> Default for IntermediateAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone,
    T: Hash + Eq + Debug + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}
