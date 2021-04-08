use crate::{DFA, NFA};
use std::{fmt::Display, fs::File, hash::Hash, io::Write, path::Path};

struct DotEdge<Node, Label>
where
    Node: Display,
    Label: Display,
{
    source: Node,
    label: Label,
    destination: Node,
}

impl<Node, Label> DotEdge<Node, Label>
where
    Node: Display,
    Label: Display,
{
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

pub struct Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    edges: Vec<DotEdge<Node, Label>>,
}

impl<Node, Label> Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    fn new() -> Self {
        Self { edges: vec![] }
    }
}

impl<Node, Label> Display for Dot<Node, Label>
where
    Node: Display,
    Label: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph Automata {{")?;
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

pub trait TryWriteFile {
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
