use crate::{DFA, NFA};
use std::{fmt::Display, fs::File, hash::Hash, io::Write, path::Path};

/// A labeled directed edge in a DOT graph.
///
/// It is the same as writing `Source -> Destination [label=Label]`.
struct DotEdge<Node, Label>
where
    Node: Display,
    Label: Display,
{
    /// Edge source node.
    source: Node,
    /// Edge label.
    label: Label,
    /// Edge destination node.
    destination: Node,
}

impl<Node, Label> DotEdge<Node, Label>
where
    Node: Display,
    Label: Display,
{
    /// Construct a new labeled DOT edge.
    fn new(source: Node, label: Label, destination: Node) -> Self {
        Self {
            source,
            label,
            destination,
        }
    }
}

impl<Node, Label> Display for DotEdge<Node, Label>
where
    Node: Display,
    Label: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} -> {} [label={}];\n",
            self.source, self.destination, self.label
        ))
    }
}

/// The list of directed edges in the DOT graph.
pub struct Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    /// List of [DotEdge].
    edges: Vec<DotEdge<Node, Label>>,
    /// List of initial state nodes.
    initial_states: Vec<Node>,
    /// List of final state nodes.
    final_states: Vec<Node>,
}

impl<Node, Label> Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    fn new() -> Self {
        Self {
            edges: vec![],
            initial_states: vec![],
            final_states: vec![],
        }
    }
}

impl<Node, Label> Display for Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph Automata {{")?;
        for (i, node) in self.initial_states.iter().enumerate() {
            f.write_fmt(format_args!(
                "\t_initial_{} [label=\"\", shape=\"plaintext\"];\n",
                i
            ))?;
            f.write_fmt(format_args!("\t_initial_{} -> {};\n", i, node))?;
        }
        for node in self.final_states.iter() {
            f.write_fmt(format_args!("\t{} [style=\"bold\"];\n", node))?;
        }
        for edge in self.edges.iter() {
            f.write_fmt(format_args!("\t{}", edge))?;
        }
        writeln!(f, "}}")
    }
}

impl<Node, Label> From<DFA<Node, Label>> for Dot<Node, Label>
where
    Node: Eq + Hash + Clone + Display,
    Label: Eq + Hash + Clone + Display,
{
    fn from(dfa: DFA<Node, Label>) -> Self {
        let mut dot = Dot::new();
        for node in dfa.initial_states {
            dot.initial_states.push(node)
        }
        for node in dfa.final_states {
            dot.final_states.push(node)
        }
        for (source, transitions) in dfa.delta {
            for (label, destination) in transitions {
                dot.edges
                    .push(DotEdge::new(source.clone(), label, destination))
            }
        }
        dot
    }
}

impl<Node, Label> From<NFA<Node, Label>> for Dot<Node, Label>
where
    Node: Eq + Hash + Clone + Display,
    Label: Eq + Hash + Clone + Display,
{
    fn from(nfa: NFA<Node, Label>) -> Self {
        let mut dot = Dot::new();
        for (source, transitions) in nfa.delta {
            for (label, destinations) in transitions {
                for destination in destinations {
                    dot.edges
                        .push(DotEdge::new(source.clone(), label.clone(), destination))
                }
            }
        }
        dot
    }
}

/// Write to file operation.
pub trait TryWriteFile {
    /// Try to write `self` to the file in `path`.
    /// This operation uses the `Display` representation for its output.
    /// If successful, returns the written [File], otherwise, an [std::io::Error] is returned.
    fn try_write_file<P: AsRef<Path>>(self, path: P) -> std::io::Result<File>;
}

impl<Node, Label> TryWriteFile for Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    fn try_write_file<P: AsRef<Path>>(self, path: P) -> std::io::Result<File> {
        let mut file = File::create(path)?;
        file.write(self.to_string().as_bytes())?;
        Ok(file)
    }
}
