use std::collections::HashSet;

use petgraph::graph::DiGraph;
use std::hash::Hash;

/// Alias for the `DeterministicFiniteAutomata` type.
pub type DFA<State, Transition> = DeterministicFiniteAutomata<State, Transition>;

/// A deterministic finitie automata representation.
///
/// The automata itself is implemented on top of `petgraph::graphmap::DiGraphMap`.
pub struct DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash,
    // Transition: Eq + Hash,
{
    /// The set of all automata states.
    states: HashSet<State>,
    /// The set of all initial states.
    initial_states: HashSet<State>,
    /// The set of all final states.
    final_states: HashSet<State>,
    // /// The set of state transitions.
    // transitions: HashSet<Transition<S, T>>,
    /// Automata graph.
    automata: DiGraph<State, Transition>,
}

impl<State, Transition> DeterministicFiniteAutomata<State, Transition> where State: Eq + Hash {}
