use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;

use petgraph::{
    graphmap::{DiGraphMap, NeighborsDirected, NodeTrait},
    Directed, EdgeDirection,
};
// /// An automata state.
// #[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
// pub struct State<T>(T)
// where
//     T: Eq + Ord + Copy + Hash;

// impl<T> From<T> for State<T>
// where
//     T: Eq + Ord + Copy + Hash,
// {
//     fn from(inner: T) -> Self {
//         Self(inner)
//     }
// }

// /// An automata transition symbol.
// #[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
// pub struct Symbol<T>(T)
// where
//     T: Eq + Ord + Copy + Hash;

// impl<T> From<T> for Symbol<T>
// where
//     T: Eq + Ord + Copy + Hash,
// {
//     fn from(inner: T) -> Self {
//         Self(inner)
//     }
// }

// /// A transition from `source` state to `destination` state through `symbol`.
// #[derive(Debug, PartialEq, Eq, Hash)]
// pub struct Transition<'dfa, S, T>
// where
//     S: Eq + Ord + Copy + Hash,
//     T: Eq + Ord + Copy + Hash,
// {
//     /// The state from which the transition starts.
//     source: &'dfa State<S>,
//     /// The state on which the transition ends.
//     destination: &'dfa State<S>,
//     /// The transition symbol (or function).
//     symbol: &'dfa Symbol<T>,
// }

// impl<'dfa, S, T> Transition<'dfa, S, T>
// where
//     S: Eq + Ord + Copy + Hash,
//     T: Eq + Ord + Copy + Hash,
// {
//     /// Construct a new instance of `Transition<'s, S, T>`
//     pub fn new(
//         source: &'dfa State<S>,
//         destination: &'dfa State<S>,
//         symbol: &'dfa Symbol<T>,
//     ) -> Self {
//         Self {
//             source,
//             destination,
//             symbol,
//         }
//     }
// }

/// Alias for the `DeterministicFiniteAutomata` type.
pub type DFA<State, Transition> = DeterministicFiniteAutomata<State, Transition>;

/// A deterministic finitie automata representation.
///
/// The automata itself is implemented on top of `petgraph::graphmap::DiGraphMap`.
pub struct DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Ord + Copy + Hash,
    Transition: Eq + Ord + Copy + Hash,
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
    automata: DiGraphMap<State, Transition>,
}

impl<State, Transition> DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Ord + Copy + Hash,
    Transition: Eq + Ord + Copy + Hash,
{
    /// Construct a new deterministic finite automata.
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            initial_states: HashSet::new(),
            final_states: HashSet::new(),
            // transitions: HashSet::new(),
            automata: DiGraphMap::new(),
        }
    }

    /// Add a new state to the automata.
    /// This function adds the state to the general state set and returns the added node.
    pub fn add_state(&mut self, state: State) -> State {
        self.states.insert(state);
        self.automata.add_node(state)
    }

    /// Add a new initial state to the automata.
    /// This function also adds the state to the general state set if it was not already present and returns the added node.
    pub fn add_initial_state(&mut self, state: State) -> State {
        self.initial_states.insert(state);
        self.add_state(state)
    }

    /// Add a new final state to the automata.
    /// This function also adds the state to the general state set and returns the added node.
    pub fn add_final_state(&mut self, state: State) -> State {
        self.final_states.insert(state);
        self.add_state(state)
    }

    /// Add a new transition to the automata.
    pub fn add_transition(
        &mut self,
        source: State,
        destination: State,
        transition: Transition,
    ) -> Option<Transition> {
        // self.transitions.insert(transition);
        self.automata.add_edge(source, destination, transition)
    }

    /// Generate the set of reachable states from a given state.
    pub fn reachable(&self, state: State) -> HashSet<State> {
        let automata = &self.automata;
        let mut stack = VecDeque::new();
        let mut discovered = HashSet::new();
        stack.push_front(state);
        while let Some(s) = stack.pop_front() {
            for n in automata.neighbors_outgoing(s) {
                if discovered.insert(n) {
                    stack.push_back(n)
                }
            }
        }
        discovered
    }

    /// Check if a state is productive.
    ///
    /// This function generates all reachable states from `state` and
    /// intersects the resulting set with the final state set.
    /// If the intersection has *at least* one element,
    /// the state is considered to be productive.
    pub fn is_productive(&self, state: State) -> bool {
        let reachable_states = self.reachable(state);
        let mut intersection = reachable_states.intersection(&self.final_states);
        if let Some(_) = intersection.next() {
            true
        } else {
            false
        }
    }

    /// Check if a state is useful.
    /// (i.e. if a state is reachable from an initial state and a final state is reachable from it.)
    ///
    /// This function calls `is_productive`.
    /// If the state is productive then it checks
    /// if the given state is in the set of states reachable from the initial state.
    pub fn is_useful(&self, state: State) -> bool {
        if self.is_productive(state) {
            for i in self.initial_states.iter() {
                let reachable_from_i = self.reachable(*i);
                if reachable_from_i.contains(i) {
                    return true;
                }
            }
        }
        false
    }
}

trait DiNeighbors<N>
where
    N: NodeTrait,
{
    fn neighbors_outgoing(&self, a: N) -> NeighborsDirected<N, Directed>;
    fn neighbors_incoming(&self, a: N) -> NeighborsDirected<N, Directed>;
}

impl<N, E> DiNeighbors<N> for DiGraphMap<N, E>
where
    N: NodeTrait,
{
    fn neighbors_outgoing(&self, a: N) -> NeighborsDirected<N, Directed> {
        self.neighbors_directed(a, EdgeDirection::Outgoing)
    }

    fn neighbors_incoming(&self, a: N) -> NeighborsDirected<N, Directed> {
        self.neighbors_directed(a, EdgeDirection::Incoming)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_automata() -> DFA<i32, i32> {
        let mut dfa = DFA::new();
        dfa.add_initial_state(1);
        dfa.add_initial_state(2);
        dfa.add_initial_state(3);
        dfa.add_initial_state(4);
        dfa.add_transition(1, 2, 1);
        dfa.add_transition(1, 3, 2);
        dfa.add_transition(3, 2, 3);
        dfa.add_transition(2, 3, 4);
        dfa.add_transition(2, 4, 5);
        dfa
    }

    #[test]
    fn test_reachable() {
        let dfa = setup_automata();
        assert!(dfa.reachable(1).contains(&2));
        assert!(dfa.reachable(1).contains(&3));
        assert!(dfa.reachable(1).contains(&4));
    }

    #[test]
    fn test_not_productive() {
        let dfa = setup_automata();
        assert!(!dfa.is_productive(1));
        assert!(!dfa.is_productive(2));
        assert!(!dfa.is_productive(3));
    }

    #[test]
    fn test_productive() {
        let mut dfa = setup_automata();
        dfa.add_final_state(4);
        assert!(dfa.is_productive(1));
        assert!(dfa.is_productive(2));
        assert!(dfa.is_productive(3));
    }

    #[test]
    fn test_not_useful() {
        let dfa = setup_automata();
        assert!(!dfa.is_useful(1));
        assert!(!dfa.is_useful(2));
        assert!(!dfa.is_useful(3));
    }

    #[test]
    fn test_useful() {
        let mut dfa = setup_automata();
        dfa.add_final_state(4);
        assert!(dfa.is_useful(1));
        assert!(dfa.is_useful(2));
        assert!(dfa.is_useful(3));
    }
}
