use crate::{DFA, NFA};
use std::{fmt::Display, fs::File, hash::Hash, io::Write, path::Path};

struct DotEdge<Node, Edge>
where
    Node: Display,
    Edge: Display,
{
    source: Node,
    edge: Edge,
    destination: Node,
}

impl<Node, Edge> DotEdge<Node, Edge>
where
    Node: Display,
    Edge: Display,
{
    fn new(source: Node, edge: Edge, destination: Node) -> Self {
        Self {
            source,
            edge,
            destination,
        }
    }
}

impl<Node, Edge> Display for DotEdge<Node, Edge>
where
    Node: Display,
    Edge: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} -> {} [label={}];\n",
            self.source, self.destination, self.edge
        ))
    }
}

pub struct Dot<Node, Edge>
where
    Node: Display,
    Edge: Display,
{
    edges: Vec<DotEdge<Node, Edge>>,
}

impl<Node, Edge> Dot<Node, Edge>
where
    Node: Display,
    Edge: Display,
{
    fn new() -> Self {
        Self { edges: vec![] }
    }
}

impl<Node, Edge> Display for Dot<Node, Edge>
where
    Node: Display,
    Edge: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph Automata {{")?;
        for edge in self.edges.iter() {
            f.write_fmt(format_args!("\t{}", edge))?;
        }
        writeln!(f, "}}")
    }
}

impl<Node, Edge> From<DFA<Node, Edge>> for Dot<Node, Edge>
where
    Node: Eq + Hash + Clone + Display,
    Edge: Eq + Hash + Clone + Display,
{
    fn from(dfa: DFA<Node, Edge>) -> Self {
        let mut dot = Dot::new();
        for (source, transitions) in dfa.delta {
            for (edge, destination) in transitions {
                dot.edges
                    .push(DotEdge::new(source.clone(), edge, destination))
            }
        }
        dot
    }
}

impl<Node, Edge> From<NFA<Node, Edge>> for Dot<Node, Edge>
where
    Node: Eq + Hash + Clone + Display,
    Edge: Eq + Hash + Clone + Display,
{
    fn from(_: NFA<Node, Edge>) -> Self {
        todo!()
    }
}

pub trait TryIntoFile {
    fn try_into_file<P: AsRef<Path>>(self, path: P) -> std::io::Result<File>;
}

impl<Node, Edge> TryIntoFile for Dot<Node, Edge>
where
    Node: Display,
    Edge: Display,
{
    fn try_into_file<P: AsRef<Path>>(self, path: P) -> std::io::Result<File> {
        let mut file = File::create(path)?;
        file.write(self.to_string().as_bytes())?;
        Ok(file)
    }
}
