use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub type FA<State, Transition> = FiniteAutomata<State, Transition>;

trait Automata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Add a state to the automata.
    fn add_state(&mut self, state: State);
    /// Add an initial state to the automata.
    fn add_initial(&mut self, state: State);
    /// Add a final state to the automata.
    fn add_final(&mut self, state: State);
    /// Add a new symbol to the automata alphabet.
    fn add_sigma(&mut self, sigma: Transition);
}

pub struct FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Finite automata states.
    states: HashSet<State>,
    /// Finite automata transition symbols.
    sigma: HashSet<Transition>,
    /// Finite automata initial state-indexes.
    initial_states: HashSet<State>,
    /// Finite automata final state-indexes.
    final_states: HashSet<State>,
    /// Finite automata transition functions.
    /// Map of state indexes to map of transitions to state indexes.
    delta: HashMap<State, HashMap<Transition, State>>,
}

impl<State, Transition> FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    fn new() -> Self {
        Self {
            states: HashSet::new(),
            sigma: HashSet::new(),
            initial_states: HashSet::new(),
            final_states: HashSet::new(),
            delta: HashMap::new(),
        }
    }
}

impl<State, Transition> Automata<State, Transition> for FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    fn add_state(&mut self, state: State) {
        self.states.insert(state);
    }

    fn add_initial(&mut self, state: State) {
        self.states.insert(state.clone());
        self.initial_states.insert(state);
    }

    fn add_final(&mut self, state: State) {
        self.states.insert(state.clone());
        self.final_states.insert(state);
    }

    fn add_sigma(&mut self, sigma: Transition) {
        self.sigma.insert(sigma);
    }
}

/// Implementation of the [Default] trait for a [FiniteAutomata].
/// This function is equivalent to [FiniteAutomata::new].
impl<State, Transition> Default for FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}
