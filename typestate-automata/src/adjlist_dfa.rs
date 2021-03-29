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
    nodes: HashSet<Rc<Node>>,
    edges: HashMap<Rc<Node>, HashSet<DiEdge<Node, Edge>>>,
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
}
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
