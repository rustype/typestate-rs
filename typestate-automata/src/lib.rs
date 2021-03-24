use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;

use petgraph::{
    graphmap::{DiGraphMap, NeighborsDirected, NodeTrait},
    visit::Bfs,
    Directed, EdgeDirection,
};
/// An automata state.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
struct State<T>(T)
where
    T: Eq + Ord + Copy + Hash;

impl<T> From<T> for State<T>
where
    T: Eq + Ord + Copy + Hash,
{
    fn from(inner: T) -> Self {
        Self(inner)
    }
}

/// An automata transition symbol.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
struct Symbol<T>(T)
where
    T: Eq + Ord + Copy + Hash;

impl<T> From<T> for Symbol<T>
where
    T: Eq + Ord + Copy + Hash,
{
    fn from(inner: T) -> Self {
        Self(inner)
    }
}

/// A transition from `source` state to `destination` state through `symbol`.
#[derive(Debug, PartialEq, Eq, Hash)]
struct Transition<'dfa, S, T>
where
    S: Eq + Ord + Copy + Hash,
    T: Eq + Ord + Copy + Hash,
{
    /// The state from which the transition starts.
    source: &'dfa State<S>,
    /// The state on which the transition ends.
    destination: &'dfa State<S>,
    /// The transition symbol (or function).
    symbol: &'dfa Symbol<T>,
}

impl<'dfa, S, T> Transition<'dfa, S, T>
where
    S: Eq + Ord + Copy + Hash,
    T: Eq + Ord + Copy + Hash,
{
    /// Construct a new instance of `Transition<'s, S, T>`
    fn new(source: &'dfa State<S>, destination: &'dfa State<S>, symbol: &'dfa Symbol<T>) -> Self {
        Self {
            source,
            destination,
            symbol,
        }
    }
}

pub type DFA<'dfa, S, T> = DeterministicFiniteAutomata<'dfa, S, T>;

pub struct DeterministicFiniteAutomata<'dfa, S, T>
where
    S: Eq + Ord + Copy + Hash,
    T: Eq + Ord + Copy + Hash,
{
    /// The set of all automata states.
    states: HashSet<&'dfa State<S>>,
    /// The set of all initial states.
    initial_states: HashSet<&'dfa State<S>>,
    /// The set of all final states.
    final_states: HashSet<&'dfa State<S>>,
    /// The set of state transitions.
    transitions: HashSet<&'dfa Transition<'dfa, S, T>>,
    /// Automata graph.
    automata: DiGraphMap<&'dfa State<S>, &'dfa Symbol<T>>,
}

impl<'dfa, S, T> DeterministicFiniteAutomata<'dfa, S, T>
where
    S: Eq + Ord + Copy + Hash,
    T: Eq + Ord + Copy + Hash,
{
    /// Construct a new deterministic finite automata.
    fn new() -> Self {
        Self {
            states: HashSet::new(),
            initial_states: HashSet::new(),
            final_states: HashSet::new(),
            transitions: HashSet::new(),
            automata: DiGraphMap::new(),
        }
    }

    /// Add a new state to the automata.
    /// This function adds the state to the general state set and returns the added node.
    fn add_state(&mut self, state: &'dfa State<S>) -> &'dfa State<S> {
        self.states.insert(state);
        self.automata.add_node(state)
    }

    /// Add a new initial state to the automata.
    /// This function also adds the state to the general state set if it was not already present and returns the added node.
    fn add_initial_state(&mut self, state: &'dfa State<S>) -> &'dfa State<S> {
        self.initial_states.insert(state);
        self.add_state(state)
    }

    /// Add a new final state to the automata.
    /// This function also adds the state to the general state set and returns the added node.
    fn add_final_state(&mut self, state: &'dfa State<S>) -> &'dfa State<S> {
        self.final_states.insert(state);
        self.add_state(state)
    }

    /// Add a new transition to the automata.
    fn add_transition(
        &mut self,
        transition: &'dfa Transition<'dfa, S, T>,
    ) -> Option<&'dfa Symbol<T>> {
        self.transitions.insert(transition);
        self.automata
            .add_edge(transition.source, transition.destination, transition.symbol)
    }

    /// Generate the set of reachable states from a given state.
    fn reachable(&mut self, state: &'dfa State<S>) -> HashSet<&'dfa State<S>>
    where
        S: Debug,
    {
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
    #[test]
    fn test_reachable() {
        let mut dfa = DFA::new();
        let s1 = State::from(1);
        let s2 = State::from(2);
        let s3 = State::from(3);
        let s4 = State::from(4);

        let sy1 = Symbol::from(1);
        let sy2 = Symbol::from(2);
        let sy3 = Symbol::from(3);
        let sy4 = Symbol::from(4);

        let t1 = Transition::new(&s1, &s2, &sy1);
        let t2 = Transition::new(&s1, &s3, &sy2);
        let t3 = Transition::new(&s3, &s2, &sy3);
        let t4 = Transition::new(&s2, &s3, &sy4);
        let t5 = Transition::new(&s2, &s4, &sy4);

        dfa.add_initial_state(&s1);
        dfa.add_initial_state(&s2);
        dfa.add_initial_state(&s3);
        dfa.add_initial_state(&s4);

        dfa.add_transition(&t1);
        dfa.add_transition(&t2);
        dfa.add_transition(&t3);
        dfa.add_transition(&t4);
        dfa.add_transition(&t5);

        assert!(dfa.reachable(&s1).contains(&s2));
        assert!(dfa.reachable(&s1).contains(&s3));
        assert!(dfa.reachable(&s1).contains(&s4));

        // eprintln!("{:#?}", dfa.reachable(&s1).into_iter().collect::<Vec<_>>());
    }
}
