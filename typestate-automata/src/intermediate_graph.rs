use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

// TODO: implement From<Vec<S>> for Node<S>
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

#[derive(Debug, Clone)]
pub struct IntermediateAutomaton<State, Transition>
where
    State: Hash + Eq + Debug + Clone,
    Transition: Hash + Eq + Debug + Clone,
{
    states: HashSet<State>,

    // // NOTE: These two might not be needed, we can simply write their deltas as (Option<State>, Option<State>)
    // initial_states: HashSet<State>,
    // final_states: HashSet<State>,

    // NOTE: Vec<State> should work here since we do not expect State duplicates
    // TODO: Initial states - Option::None -> Transition -> State
    // TODO: Final states - State -> Transition -> State' (where State == State')
    delta: HashMap<Option<State>, HashMap<Transition, Node<State>>>,
}

impl<State, Transition> IntermediateAutomaton<State, Transition>
where
    State: Hash + Eq + Debug + Clone,
    Transition: Hash + Eq + Debug + Clone,
{
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            delta: HashMap::new(),
        }
    }

    pub fn add_state(&mut self, state: State) -> bool {
        self.states.insert(state)
    }

    pub fn add_transition(
        &mut self,
        source: Option<State>,
        transition: Transition,
        destinations: Node<State>,
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

impl<State, Transition> Default for IntermediateAutomaton<State, Transition>
where
    State: Hash + Eq + Debug + Clone,
    Transition: Hash + Eq + Debug + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}
