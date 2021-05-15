use crate::{Dfa, Nfa, TryWriteFile};
use std::{fmt::Display, fs::File, hash::Hash, io::Write, path::Path};

// TODO make this have defaults in case std is not present
fn var_or_default(var_name: &str, var_default: &str) -> String {
    ::std::env::var_os(var_name)
        .and_then(|s| s.into_string().ok())
        .unwrap_or_else(|| var_default.to_string())
}

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
    initial_states: Vec<(Node, Label)>,
    /// List of final state nodes.
    final_states: Vec<(Node, Label)>,
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
        f.write_fmt(format_args!(
            "graph [pad=\"{}\", nodesep=\"{}\", ranksep=\"{}\"];",
            var_or_default("DOT_PAD", "0.25"),
            var_or_default("DOT_NODESEP", "0.75"),
            var_or_default("DOT_RANKSEP", "1"),
        ))?;
        for (i, (node, label)) in self.initial_states.iter().enumerate() {
            f.write_fmt(format_args!(
                "\t_initial_{} [label=\"\", shape=\"plaintext\"];\n",
                i
            ))?;
            f.write_fmt(format_args!(
                "\t_initial_{} -> {} [label=\"{}\"];\n",
                i, node, label
            ))?;
        }
        for (node, label) in self.final_states.iter() {
            f.write_fmt(format_args!("\t{} [style=\"bold\"];\n", node))?;
            f.write_fmt(format_args!(
                "\t{} -> {} [label=\"{}\", style=dashed];\n",
                node, node, label
            ))?;
        }
        for edge in self.edges.iter() {
            f.write_fmt(format_args!("\t{}", edge))?;
        }
        writeln!(f, "}}")
    }
}

impl<Node, Label> From<Dfa<Node, Label>> for Dot<Node, Label>
where
    Node: Eq + Hash + Clone + Display,
    Label: Eq + Hash + Clone + Display,
{
    fn from(dfa: Dfa<Node, Label>) -> Self {
        let mut dot = Dot::new();
        for (node, transitions) in dfa.initial_states {
            transitions
                .into_iter()
                .for_each(|t| dot.initial_states.push((node.clone(), t)));
        }
        for (node, transitions) in dfa.final_states {
            transitions
                .into_iter()
                .for_each(|t| dot.final_states.push((node.clone(), t)));
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

impl<Node, Label> From<Nfa<Node, Label>> for Dot<Node, Label>
where
    Node: Eq + Hash + Clone + Display,
    Label: Eq + Hash + Clone + Display,
{
    fn from(nfa: Nfa<Node, Label>) -> Self {
        let mut dot = Dot::new();
        for (node, transitions) in nfa.initial_states {
            transitions
                .into_iter()
                .for_each(|t| dot.initial_states.push((node.clone(), t)));
        }
        for (node, transitions) in nfa.final_states {
            transitions
                .into_iter()
                .for_each(|t| dot.final_states.push((node.clone(), t)));
        }
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

impl<Node, Label> TryWriteFile for Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    fn try_write_file<P: AsRef<Path>>(self, path: P) -> std::io::Result<File> {
        let mut file = File::create(path)?;
        file.write_all(self.to_string().as_bytes())?;
        Ok(file)
    }
}
