use std::{
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

use std::hash::Hash;

#[derive(PartialEq, Eq, Hash)]
pub enum Direction {
    Incoming,
    Outgoing,
}

#[derive(PartialEq, Eq, Hash)]
pub struct DiEdge<Node, Edge>
where
    Node: Eq + Hash,
    Edge: Eq + Hash,
{
    edge: Rc<Edge>,
    node: Rc<Node>,
    direction: Direction,
}

impl<Node, Edge> DiEdge<Node, Edge>
where
    Node: Eq + Hash,
    Edge: Eq + Hash,
{
    /// Construct a new incoming edge.
    fn new_incoming(edge: Rc<Edge>, node: Rc<Node>) -> Self {
        Self {
            edge,
            node,
            direction: Direction::Incoming,
        }
    }

    /// Construct a new outgoing edge.
    fn new_outgoing(edge: Rc<Edge>, node: Rc<Node>) -> Self {
        Self {
            edge,
            node,
            direction: Direction::Outgoing,
        }
    }
}

pub struct DiGraph<Node, Edge>
where
    Node: Eq + Hash,
    Edge: Eq + Hash,
{
    pub nodes: HashSet<Rc<Node>>,
    pub edges: HashMap<Rc<Node>, HashSet<DiEdge<Node, Edge>>>,
}

impl<Node, Edge> DiGraph<Node, Edge>
where
    Node: Eq + Hash,
    Edge: Eq + Hash,
{
    /// Construct a new directed graph.
    pub fn new() -> Self {
        Self {
            nodes: HashSet::new(),
            edges: HashMap::new(),
        }
    }

    /// Add a new node to the graph.
    ///
    /// Returns `true` if the node was not present in the graph,
    /// otherwise it returns `false`.
    pub fn add_node(&mut self, node: Node) -> Rc<Node> {
        // TODO get this reviewed
        // WARN `Rc<Node>` "duplication"
        // Consider nodes `A` & `B` where `A == B` although they are different objects in memory.
        // Should the graph detect this case and return the old `Rc<Node>`?
        // If we need to return the old `Rc<Node>` we can use a `slab::Slab` along with an `HashMap<Rc<Node>, usize>`,
        // which maps `Rc`s to indices on the slab.
        let node = Rc::new(node);
        self.nodes.insert(node.clone());
        node
    }

    /// Check if the graph contains a given node.
    pub fn contains_node(&self, node: &Node) -> bool {
        self.nodes.contains(node)
    }

    fn add_outgoing_edge(&mut self, src: Rc<Node>, dst: Rc<Node>, edge: Rc<Edge>) -> bool {
        let edge = DiEdge::new_outgoing(edge, dst.clone());
        if let Some(edges) = self.edges.get_mut(&src) {
            edges.insert(edge)
        } else {
            let mut edges = HashSet::new();
            let res = edges.insert(edge);
            self.edges.insert(src, edges);
            res
        }
    }

    fn add_incoming_edge(&mut self, dst: Rc<Node>, src: Rc<Node>, edge: Rc<Edge>) -> bool {
        let edge = DiEdge::new_incoming(edge, src.clone());
        if let Some(edges) = self.edges.get_mut(&dst) {
            edges.insert(edge)
        } else {
            let mut edges = HashSet::new();
            let res = edges.insert(edge);
            self.edges.insert(dst, edges);
            res
        }
    }

    /// Add a new edge to the graph.
    ///
    /// The edge's nodes are also added to the graph.
    ///
    /// Returns `true` if the node was not present in the graph,
    /// otherwise it returns `false`.
    pub fn add_edge(&mut self, src: Node, dst: Node, edge: Edge) -> bool {
        let src = Rc::new(src);
        let dst = Rc::new(dst);
        let edge = Rc::new(edge);
        self.nodes.insert(src.clone());
        self.nodes.insert(dst.clone());
        self.add_outgoing_edge(src.clone(), dst.clone(), edge.clone())
            && self.add_incoming_edge(dst.clone(), src.clone(), edge.clone())
    }

    /// Return an iterator over a given node's incoming neighbors.
    ///
    /// Returns `None`, if the node does not exist in the graph.
    pub fn neighbors(&self, node: &Node) -> Option<impl Iterator<Item = &DiEdge<Node, Edge>>> {
        self.edges.get(node).map(|set| set.iter())
    }

    /// Return an iterator over a given node's neighbors.
    ///
    /// Returns `None`, if the node does not exist in the graph.
    pub fn neighbors_incoming(
        &self,
        node: &Node,
    ) -> Option<impl Iterator<Item = &DiEdge<Node, Edge>>> {
        self.edges.get(node).map(|set| {
            set.iter()
                .filter(|edge| edge.direction == Direction::Incoming)
        })
    }

    /// Return an iterator over a given node's outgoing neighbors.
    ///
    /// Returns `None`, if the node does not exist in the graph.
    pub fn neighbors_outgoing(
        &self,
        node: &Node,
    ) -> Option<impl Iterator<Item = &DiEdge<Node, Edge>>> {
        self.edges.get(node).map(|set| {
            set.iter()
                .filter(|edge| edge.direction == Direction::Outgoing)
        })
    }

    /// Compute the set of nodes reachable in any direction from a given starting node.
    // HACK deal with the `Rc`
    // HACK the amount of shadowing is unreal, care with that
    pub fn reachable(&self, node: Rc<Node>) -> HashSet<Rc<Node>> {
        let mut stack = VecDeque::new();
        let mut discovered = HashSet::new();
        // should the starting node be added as discovered?
        stack.push_front(node);
        while let Some(node) = stack.pop_front() {
            match self.neighbors(&node) {
                Some(edge_iter) => {
                    edge_iter
                        .map(|edge| &edge.node) // we dont care for labels
                        .for_each(|node| {
                            if discovered.insert(node.clone()) {
                                stack.push_back(node.clone())
                            }
                        })
                }
                None => {}
            }
        }
        discovered
    }

    /// Compute the set of nodes reachable in the incoming direction from a given starting node.
    // HACK deal with the `Rc`
    // HACK the amount of shadowing is unreal, care with that
    pub fn reachable_incoming(&self, node: Rc<Node>) -> HashSet<Rc<Node>> {
        let mut stack = VecDeque::new();
        let mut discovered = HashSet::new();
        // should the starting node be added as discovered?
        stack.push_front(node);
        while let Some(node) = stack.pop_front() {
            match self.neighbors_incoming(&node) {
                Some(edge_iter) => {
                    edge_iter
                        .map(|edge| &edge.node) // we dont care for labels
                        .for_each(|node| {
                            if discovered.insert(node.clone()) {
                                stack.push_back(node.clone())
                            }
                        })
                }
                None => {}
            }
        }
        discovered
    }

    /// Compute the set of nodes reachable in the outgoing direction from a given starting node.
    // HACK deal with the `Rc`
    // HACK the amount of shadowing is unreal, care with that
    pub fn reachable_outgoing(&self, node: Rc<Node>) -> HashSet<Rc<Node>> {
        let mut stack = VecDeque::new();
        let mut discovered = HashSet::new();
        // should the starting node be added as discovered?
        stack.push_front(node);
        while let Some(node) = stack.pop_front() {
            match self.neighbors_outgoing(&node) {
                Some(edge_iter) => {
                    edge_iter
                        .map(|edge| &edge.node) // we dont care for labels
                        .for_each(|node| {
                            if discovered.insert(node.clone()) {
                                stack.push_back(node.clone())
                            }
                        })
                }
                None => {}
            }
        }
        discovered
    }
}

#[cfg(test)]
mod digraph_test {
    use super::test_traits::*;
    use super::*;
    use std::{collections::hash_set::HashSet, rc::Rc};

    fn setup_graph_with_edges() -> DiGraph<i32, i32> {
        let mut graph = DiGraph::new();
        graph.add_edge(1, 2, 1);
        graph.add_edge(1, 3, 1);
        graph.add_edge(2, 6, 1);
        graph.add_edge(3, 4, 1);
        graph.add_edge(3, 5, 1);
        graph.add_edge(3, 6, 1);
        graph.add_edge(4, 5, 1);
        graph.add_edge(5, 7, 1);
        graph.add_edge(6, 7, 1);
        graph
    }

    #[test]
    fn test_nodes_from_edges() {
        let graph = setup_graph_with_edges();
        let expected_nodes = [1, 2, 3, 4, 5, 6, 7];
        expected_nodes.iter().for_each(|node| {
            assert!(graph.nodes.contains(node));
            assert_eq!(graph.nodes.contains(node), graph.contains_node(node));
        });
    }

    #[test]
    fn test_nodes() {
        let mut graph: DiGraph<i32, ()> = DiGraph::new();
        let expected_nodes = [1, 2, 3, 4, 5, 6, 7];
        expected_nodes.iter().for_each(|node| {
            graph.add_node(*node);
        });
        expected_nodes.iter().for_each(|node| {
            assert!(graph.nodes.contains(node));
            assert_eq!(graph.nodes.contains(node), graph.contains_node(node));
        });
    }

    #[test]
    fn test_neighbors() {
        let graph = setup_graph_with_edges();
        let expected_neighbors_five: HashSet<i32> = [3, 4, 7].into_hash_set();
        let neighbors_five: HashSet<i32> = graph.neighbors(&5).unwrap().map(|e| *e.node).collect();
        assert_eq!(expected_neighbors_five, neighbors_five);
    }

    #[test]
    fn test_neighbors_incoming() {
        let graph = setup_graph_with_edges();
        let expected_neighbors_five: HashSet<i32> = [3, 4].into_hash_set();
        let neighbors_five: HashSet<i32> = graph
            .neighbors_incoming(&5)
            .unwrap()
            .map(|e| *e.node)
            .collect();
        assert_eq!(expected_neighbors_five, neighbors_five);
    }

    #[test]
    fn test_neighbors_outgoing() {
        let graph = setup_graph_with_edges();
        let expected_neighbors_five: HashSet<i32> = [7].into_hash_set();
        let neighbors_five: HashSet<i32> = graph
            .neighbors_outgoing(&5)
            .unwrap()
            .map(|e| *e.node)
            .collect();
        assert_eq!(expected_neighbors_five, neighbors_five);
    }

    #[test]
    fn test_reachable() {
        let graph = setup_graph_with_edges();
        // `3` is included in the expected because it can "loop" back
        let expected_reachable_three: HashSet<i32> = [1, 2, 3, 4, 5, 6, 7].into_hash_set();
        let reachable_three: HashSet<i32> = graph
            .reachable(Rc::new(3))
            .iter()
            .map(|rc_node| -> i32 { *rc_node.to_owned() }) // maybe this is kind weird
            .collect();
        assert_eq!(expected_reachable_three, reachable_three);
    }

    #[test]
    fn test_reachable_incoming() {
        let graph = setup_graph_with_edges();
        let expected_reachable_five: HashSet<i32> = [1, 3, 4].into_hash_set();
        let reachable_five: HashSet<i32> = graph
            .reachable_incoming(Rc::new(5))
            .iter()
            .map(|rc_node| -> i32 { *rc_node.to_owned() }) // maybe this is kind weird
            .collect();
        assert_eq!(expected_reachable_five, reachable_five);
    }

    #[test]
    fn test_reachable_outgoing() {
        let graph = setup_graph_with_edges();
        let expected_reachable_three: HashSet<i32> = [4, 5, 6, 7].into_hash_set();
        let reachable_three: HashSet<i32> = graph
            .reachable_outgoing(Rc::new(3))
            .iter()
            .map(|rc_node| *rc_node.to_owned()) // maybe this is kind weird
            .collect();
        assert_eq!(expected_reachable_three, reachable_three);
    }
}

/// Alias for the `DeterministicFiniteAutomata` type.
pub type DFA<State, Transition> = DeterministicFiniteAutomata<State, Transition>;

/// A deterministic finitie automata representation.
///
/// The automata itself is implemented on top of `petgraph::graphmap::DiGraphMap`.
pub struct DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash,
    Transition: Eq + Hash,
{
    /// The set of all initial states.
    pub initial_states: HashSet<Rc<State>>,
    /// The set of all final states.
    pub final_states: HashSet<Rc<State>>,
    /// Automata graph.
    pub automata: DiGraph<State, Transition>,
}

impl<State, Transition> DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash,
    Transition: Eq + Hash,
{
    /// Construct a new DFA.
    pub fn new() -> Self {
        Self {
            initial_states: HashSet::new(),
            final_states: HashSet::new(),
            automata: DiGraph::new(),
        }
    }

    /// Add a regular state to the DFA.
    pub fn add_state(&mut self, state: State) {
        self.automata.add_node(state);
    }

    /// Add an initial state to the DFA.
    ///
    /// If the state existed before, it is simply added to the initial states set.
    pub fn add_initial_state(&mut self, state: State) {
        let state = self.automata.add_node(state);
        self.initial_states.insert(state);
    }

    /// Add an final state to the DFA.
    ///
    /// If the state existed before, it is simply added to the final states set.
    pub fn add_final_state(&mut self, state: State) {
        let state = self.automata.add_node(state);
        self.final_states.insert(state);
    }

    /// Add a transition to the DFA.
    pub fn add_transition(&mut self, src: State, dst: State, transition: Transition) {
        self.automata.add_edge(src, dst, transition);
    }

    /// Compute the productive states.
    ///
    /// The taken approach starts by doing a BFS from all the final states,
    /// following the incoming transitions, and gathering them in a data structure.
    pub fn compute_productive(&self) -> HashSet<Rc<State>> {
        let mut productive = HashSet::new();
        self.final_states.iter().for_each(|state| {
            productive.extend(self.automata.reachable_incoming(state.to_owned()))
        });
        productive
    }

    /// Compute the non-productive states.
    ///
    /// This is done by calling [DeterministicFiniteAutomata::compute_productive].
    // TODO add tests
    pub fn compute_non_productive(&self) -> HashSet<Rc<State>> {
        let productive = self.compute_productive();
        self.automata
            .nodes
            .difference(&productive)
            .map(|s| s.to_owned())
            .collect()
    }

    /// Extract the useful states from a given set of productive states.
    ///
    /// Currently the complexity on this is *bad*.
    // TODO fix the node iteration redundancy
    // To fix it, we need to write a specialized iterator for the graph
    // The iterator should start in a given node, iterate the immediate neighbors and upon a condition either:
    // - Mark them and other nodes in their path as visited according to a condition
    // - Ignore them and not iterate them further
    // TODO add tests
    pub fn extract_useful(&self, productive: &HashSet<Rc<State>>) -> HashSet<Rc<State>> {
        self.initial_states
            .iter()
            .flat_map(|initial| self.automata.reachable_outgoing(Rc::clone(initial)))
            .filter(|state| productive.contains(state))
            .collect::<HashSet<Rc<State>>>()
    }
}

#[cfg(test)]
mod dfa_test {
    use super::test_traits::*;
    use super::*;

    fn setup_automata() -> DFA<i32, i32> {
        let mut dfa = DFA::new();
        dfa.add_transition(1, 2, 1);
        dfa.add_transition(1, 3, 1);
        dfa.add_transition(2, 6, 1);
        dfa.add_transition(3, 4, 1);
        dfa.add_transition(3, 5, 1);
        dfa.add_transition(3, 6, 1);
        dfa.add_transition(4, 5, 1);
        dfa.add_transition(5, 7, 1);
        dfa.add_transition(6, 7, 1);
        dfa
    }

    #[test]
    fn test_add_state() {
        let mut dfa: DFA<_, ()> = DFA::new();
        let expected_states: HashSet<i32> = [1, 2, 3, 4, 5].into_hash_set();
        expected_states.iter().for_each(|i| dfa.add_state(*i));
        let result_states: HashSet<i32> =
            dfa.automata.nodes.iter().map(|i| *i.to_owned()).collect();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_add_initial_state() {
        let mut dfa: DFA<_, ()> = DFA::new();
        let expected_states: HashSet<i32> = [1, 2, 3, 4, 5].into_hash_set();
        expected_states
            .iter()
            .for_each(|i| dfa.add_initial_state(*i));
        let result_states: HashSet<i32> =
            dfa.automata.nodes.iter().map(|i| *i.to_owned()).collect();
        let initial_result_states: HashSet<i32> =
            dfa.initial_states.iter().map(|i| *i.to_owned()).collect();
        assert_eq!(expected_states, result_states);
        assert_eq!(expected_states, initial_result_states);
    }

    #[test]
    fn test_add_existing_initial_state() {
        let mut dfa: DFA<_, ()> = DFA::new();
        let expected_states: HashSet<i32> = [1, 2, 3, 4, 5].into_hash_set();
        expected_states.iter().for_each(|i| dfa.add_state(*i));
        expected_states
            .iter()
            .for_each(|i| dfa.add_initial_state(*i));
        let result_states: HashSet<i32> =
            dfa.automata.nodes.iter().map(|i| *i.to_owned()).collect();
        let initial_result_states: HashSet<i32> =
            dfa.initial_states.iter().map(|i| *i.to_owned()).collect();
        assert_eq!(expected_states, result_states);
        assert_eq!(expected_states, initial_result_states);
    }

    #[test]
    fn test_add_final_state() {
        let mut dfa: DFA<_, ()> = DFA::new();
        let expected_states: HashSet<i32> = [1, 2, 3, 4, 5].into_hash_set();
        expected_states.iter().for_each(|i| dfa.add_final_state(*i));
        let result_states: HashSet<i32> =
            dfa.automata.nodes.iter().map(|i| *i.to_owned()).collect();
        let final_result_states: HashSet<i32> =
            dfa.final_states.iter().map(|i| *i.to_owned()).collect();
        assert_eq!(expected_states, result_states);
        assert_eq!(expected_states, final_result_states);
    }

    #[test]
    fn test_add_existing_final_state() {
        let mut dfa: DFA<_, ()> = DFA::new();
        let expected_states: HashSet<i32> = [1, 2, 3, 4, 5].into_hash_set();
        expected_states.iter().for_each(|i| dfa.add_state(*i));
        expected_states.iter().for_each(|i| dfa.add_final_state(*i));
        let result_states: HashSet<i32> =
            dfa.automata.nodes.iter().map(|i| *i.to_owned()).collect();
        let final_result_states: HashSet<i32> =
            dfa.final_states.iter().map(|i| *i.to_owned()).collect();
        assert_eq!(expected_states, result_states);
        assert_eq!(expected_states, final_result_states);
    }

    #[test]
    fn test_add_transition() {
        let dfa = setup_automata();
        let expected_states: HashSet<i32> = [1, 2, 3, 4, 5, 6, 7].into_hash_set();
        let result_states: HashSet<i32> =
            dfa.automata.nodes.iter().map(|i| *i.to_owned()).collect();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_compute_productive_empty_final() {
        let dfa = setup_automata();
        let empty_set = HashSet::new();
        assert_eq!(empty_set, dfa.compute_productive());
    }

    #[test]
    fn test_compute_productive() {
        let mut dfa = setup_automata();
        dfa.add_final_state(7);
        let expected_states: HashSet<i32> = [1, 2, 3, 4, 5, 6].into_hash_set();
        let result_states: HashSet<i32> = dfa.compute_productive().into_hash_set();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_compute_productive_multiple_states() {
        let mut dfa = setup_automata();
        dfa.add_final_state(3);
        dfa.add_final_state(5);
        let expected_states: HashSet<i32> = [1, 3, 4].into_hash_set();
        let result_states: HashSet<i32> = dfa.compute_productive().into_hash_set();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_compute_non_productive_no_final() {
        let dfa = setup_automata();
        let expected_states = [1, 2, 3, 4, 5, 6, 7].into_hash_set();
        assert_eq!(
            expected_states,
            dfa.compute_non_productive().into_hash_set()
        );
    }

    #[test]
    fn test_compute_non_productive() {
        let mut dfa = setup_automata();
        dfa.add_final_state(7);
        let expected_states: HashSet<i32> = [7].into_hash_set();
        let result_states: HashSet<i32> = dfa.compute_non_productive().into_hash_set();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_compute_non_productive_multiple_states() {
        let mut dfa = setup_automata();
        dfa.add_final_state(3);
        dfa.add_final_state(5);
        let expected_states: HashSet<i32> = [2, 5, 6, 7].into_hash_set();
        let result_states: HashSet<i32> = dfa.compute_non_productive().into_hash_set();
        assert_eq!(expected_states, result_states);
    }
}

#[cfg(test)] // should only be available for tests
mod test_traits {
    use std::{collections::HashSet, hash::Hash, rc::Rc};

    /// A conversion to HashSet<T> trait.
    pub(crate) trait IntoHashSet<T>
    where
        T: Eq + Hash,
    {
        fn into_hash_set(self) -> HashSet<T>;
    }

    /// Implementation of [IntoHashSet<T>] for a slice of `i32` integers.
    impl<const N: usize> IntoHashSet<i32> for [i32; N] {
        fn into_hash_set(self) -> HashSet<i32> {
            self.iter().map(|t| t.to_owned()).collect()
        }
    }

    /// Implementation of [IntoHashSet<T>] for an `HashSet<Rc<i32>>`.
    impl IntoHashSet<i32> for HashSet<Rc<i32>> {
        fn into_hash_set(self) -> HashSet<i32> {
            self.iter().map(|i| *i.to_owned()).collect()
        }
    }
}
