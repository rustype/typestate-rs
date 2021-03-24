use std::collections::HashSet;
use std::hash::Hash;
/// An automata state.
#[derive(Debug, PartialEq, Eq, Hash)]
struct State<T>
where
    T: Eq + Hash,
{
    /// State label.
    label: T,
}

impl<T> State<T>
where
    T: Eq + Hash,
{
    /// Construct a new instance of `State<T>`
    fn new(label: T) -> Self {
        Self { label }
    }
}

/// A transition from `source` state to `destination` state through `symbol`.
#[derive(Debug, PartialEq, Eq, Hash)]
struct Transition<'s, S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    /// The state from which the transition starts.
    source: &'s State<S>,
    /// The state on which the transition ends.
    destination: &'s State<S>,
    /// The transition symbol (or function).
    symbol: T,
}

impl<'s, S, T> Transition<'s, S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    /// Construct a new instance of `Transition<'s, S, T>`
    fn new(source: &'s State<S>, destination: &'s State<S>, symbol: T) -> Self {
        Self {
            source,
            destination,
            symbol,
        }
    }
}

pub type DFA<'s, S, T> = DeterministicFiniteAutomata<'s, S, T>;

pub struct DeterministicFiniteAutomata<'s, S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    /// The set of all automata states.
    states: HashSet<&'s State<S>>,
    /// The set of all initial states.
    initial_states: HashSet<&'s State<S>>,
    /// The set of all final states.
    final_states: HashSet<&'s State<S>>,
    /// The set of state transitions.
    transitions: HashSet<&'s Transition<'s, S, T>>,
}

impl<'s, S, T> DeterministicFiniteAutomata<'s, S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    /// Add a new state to the automata.
    /// This function adds the state to the general state set.
    fn add_state(&mut self, state: &'s State<S>) {
        self.states.insert(state);
    }

    /// Add a new initial state to the automata.
    /// This function also adds the state to the general state set.
    fn add_initial_state(&mut self, state: &'s State<S>) {
        self.states.insert(state);
        self.initial_states.insert(state);
    }

    /// Add a new final state to the automata.
    /// This function also adds the state to the general state set.
    fn add_final_state(&mut self, state: &'s State<S>) {
        self.states.insert(state);
        self.final_states.insert(state);
    }

    /// Add a new transition to the automata.
    fn add_transition(&mut self, transition: &'s Transition<'s, S, T>) {
        self.transitions.insert(transition);
    }
}
