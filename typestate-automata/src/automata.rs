use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub type FA<State, Transition> = FiniteAutomata<State, Transition>;

pub struct FiniteAutomata<State, Transition>
where
    Transition: Eq + Hash,
{
    // Finite automata states.
    states: Vec<State>,
    // Finite automata transition symbols.
    sigma: HashSet<Transition>,
    // Finite automata initial state-indexes.
    initial_states: HashSet<usize>,
    // Finite automata final state-indexes.
    final_states: HashSet<usize>,
    // Finite automata transition functions.
    // Map of state indexes to map of transitions to state indexes.
    delta: HashMap<usize, HashMap<Transition, usize>>,
}
