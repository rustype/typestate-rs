use darling::FromMeta;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

#[derive(Debug, Clone)]
pub struct StateNode<S> {
    state: Option<S>,
    metadata: Metadata,
}

impl<S> StateNode<S> {
    pub fn new(state: Option<S>) -> Self {
        Self {
            state,
            metadata: Metadata::empty(),
        }
    }

    pub fn update_metadata(&mut self, metadata: Metadata) {
        self.metadata = metadata;
    }
}

#[derive(Debug, Clone)]
pub enum Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    State(StateNode<S>),
    Decision(Vec<StateNode<S>>),
}

impl<S> From<S> for Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    fn from(s: S) -> Self {
        Node::State(StateNode::new(Some(s)))
    }
}

impl<S> From<Option<S>> for Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    fn from(s: Option<S>) -> Self {
        Node::State(StateNode::new(s))
    }
}

impl<S> From<Vec<S>> for Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    fn from(s: Vec<S>) -> Self {
        Node::Decision(s.into_iter().map(|s| StateNode::new(Some(s))).collect())
    }
}

impl<S> From<Vec<StateNode<S>>> for Node<S>
where
    S: Hash + Eq + Debug + Clone,
{
    fn from(s: Vec<StateNode<S>>) -> Self {
        Node::Decision(s)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    transition: T,
}

impl<T> Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    pub fn new(transition: T) -> Self {
        Self { transition }
    }
}

impl<T> Display for Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.transition))
    }
}

impl<T> From<T> for Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, FromMeta)]
pub struct Metadata {
    #[darling(rename="label")]
    transition_label: Option<String>,
}

impl Metadata {
    fn empty() -> Self {
        Self {
            transition_label: None,
        }
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Debug, Clone)]
pub struct IntermediateAutomaton<S, T>
where
    // State type parameter.
    S: Hash + Eq + Debug + Clone,
    // Transition type parameter.
    T: Hash + Eq + Debug + Clone + Display,
{
    states: HashSet<S>,
    choices: HashSet<S>,
    delta: HashMap<Option<S>, HashMap<Transition<T>, Node<S>>>,
}

impl<S, T> IntermediateAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone,
    T: Hash + Eq + Debug + Clone + Display,
{
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            choices: HashSet::new(),
            delta: HashMap::new(),
        }
    }

    pub fn add_state(&mut self, state: S) -> bool {
        self.states.insert(state)
    }

    pub fn add_choice(&mut self, choice: S) -> bool {
        self.choices.insert(choice)
    }

    pub fn add_transition(
        &mut self,
        source: Option<S>,
        transition: Transition<T>,
        destinations: Node<S>,
    ) {
        if let Some(source_value) = self.delta.get_mut(&source) {
            // NOTE: multi-valued transitions are disallowed because Rust does not support overloading,
            // thus, one cannot write function `f` for the same `Self` type with different signatures.
            source_value.insert(transition, destinations);
        } else {
            let mut transitions = HashMap::new();
            transitions.insert(transition, destinations);
            self.delta.insert(source, transitions);
        }
    }
}

impl<S, T> Default for IntermediateAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone,
    T: Hash + Eq + Debug + Clone + Display,
{
    fn default() -> Self {
        Self::new()
    }
}

/// Type alias for `()` or [`std::error::Error`].
type Result = std::result::Result<(), Box<dyn std::error::Error>>;

/// Blanket trait for [`Export`] implementations.
pub trait Format {}

/// Used to declare output formats for the [`IntermediateAutomaton`].
pub trait Export<F: Format> {
    /// Export the implementing type as format `F` to the output stream `w`.
    fn export<W: std::io::Write>(self, w: &mut W, format: F) -> Result;
}

#[cfg(feature = "mermaid")]
pub mod mermaid {
    use super::{Export, IntermediateAutomaton, Node, Result, Transition};
    use std::{
        fmt::{Debug, Display},
        hash::Hash,
    };

    /// The mermaid format struct.
    #[derive(Clone, Copy)]
    pub struct Mermaid;

    /// Blanket implementation for the [`Mermaid`] format.
    impl super::Format for Mermaid {}

    impl<S, T> Export<Mermaid> for IntermediateAutomaton<S, T>
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(self, w: &mut W, f: Mermaid) -> Result {
            writeln!(w, "stateDiagram-v2")?;
            for s in &self.choices {
                writeln!(w, "state {} <<choice>>", s)?
            }
            for s in &self.states {
                writeln!(w, "state {}", s)?
            }
            for (src, v) in &self.delta {
                for (t, dst) in v {
                    (src, t, dst).export(w, f)?
                }
            }
            Ok(())
        }
    }

    impl<S, T> Export<Mermaid> for (&Option<S>, &Transition<T>, &Node<S>)
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(self, w: &mut W, _: Mermaid) -> Result {
            let src = self.0;
            let t = &self.1.transition;
            let dst = self.2;

            if let Some(src) = src {
                match dst {
                    Node::State(state) => match &state.state {
                        None => writeln!(w, "{} --> [*] : {}", src, t)?,
                        Some(s) => {
                            // if there is a transition label, use that instead of the existing label
                            if let Some(label) = &state.metadata.transition_label {
                                writeln!(w, "{} --> {} : {}", src, label, t)?
                            } else {
                                writeln!(w, "{} --> {} : {}", src, s, t)?
                            }
                        }
                    },
                    Node::Decision(decision) => {
                        for s in decision {
                            if let Some(state) = &s.state {
                                if let Some(label) = &s.metadata.transition_label {
                                    writeln!(w, "{} --> {} : {}", src, state, label)?
                                } else {
                                    writeln!(w, "{} --> {}", src, state)?
                                }
                            } else {
                                if let Some(label) = &s.metadata.transition_label {
                                    writeln!(w, "{} --> [*] : {}", src, label)?
                                } else {
                                    writeln!(w, "{} --> [*]", src)?
                                }
                            }
                        }
                    }
                }
            } else {
                match dst {
                    Node::State(state) => match &state.state {
                        None => unreachable!("invalid transition: None -> None"),
                        Some(s) => {
                            // if there is a transition label, use that instead of the existing label
                            if let Some(label) = &state.metadata.transition_label {
                                writeln!(w, "[*] --> {} : {}", label, t)?
                            } else {
                                writeln!(w, "[*] --> {} : {}", s, t)?
                            }
                        }
                    },
                    Node::Decision(_) => {
                        // NOTE: unsure about this
                        unreachable!("invalid transition: None -> Decision")
                    }
                }
            }

            Ok(())
        }
    }
}
#[cfg(feature = "plantuml")]
pub mod plantuml {
    use super::{Export, IntermediateAutomaton, Node, Result, Transition};
    use std::{
        fmt::{Debug, Display},
        hash::Hash,
    };

    #[derive(Clone, Copy)]
    pub struct PlantUml;

    impl super::Format for PlantUml {}

    impl<S, T> Export<PlantUml> for IntermediateAutomaton<S, T>
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(self, w: &mut W, f: PlantUml) -> Result {
            writeln!(w, "@startuml")?;
            // TODO add settings
            for s in &self.choices {
                writeln!(w, "state {} <<choice>>", s)?
            }
            for s in &self.states {
                writeln!(w, "state {}", s)?
            }
            for (src, v) in &self.delta {
                for (t, dst) in v {
                    (src, t, dst).export(w, f)?
                }
            }
            writeln!(w, "@end")?;
            Ok(())
        }
    }

    impl<S, T> Export<PlantUml> for (&Option<S>, &Transition<T>, &Node<S>)
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(self, w: &mut W, _: PlantUml) -> Result {
            let src = self.0;
            let t = &self.1.transition;
            let dst = self.2;

            if let Some(src) = src {
                match dst {
                    Node::State(state) => match &state.state {
                        None => writeln!(w, "{} --> [*] : {}", src, t)?,
                        Some(s) => {
                            // if there is a transition label, use that instead of the existing label
                            if let Some(label) = &state.metadata.transition_label {
                                writeln!(w, "{} --> {} : {}", src, label, t)?
                            } else {
                                writeln!(w, "{} --> {} : {}", src, s, t)?
                            }
                        }
                    },
                    Node::Decision(decision) => {
                        for s in decision {
                            if let Some(state) = &s.state {
                                if let Some(label) = &s.metadata.transition_label {
                                    writeln!(w, "{} --> {} : {}", src, state, label)?
                                } else {
                                    writeln!(w, "{} --> {}", src, state)?
                                }
                            } else {
                                if let Some(label) = &s.metadata.transition_label {
                                    writeln!(w, "{} --> [*] : {}", src, label)?
                                } else {
                                    writeln!(w, "{} --> [*]", src)?
                                }
                            }
                        }
                    }
                }
            } else {
                match dst {
                    Node::State(state) => match &state.state {
                        None => unreachable!("invalid transition: None -> None"),
                        Some(s) => {
                            // if there is a transition label, use that instead of the existing label
                            if let Some(label) = &state.metadata.transition_label {
                                writeln!(w, "[*] --> {} : {}", label, t)?
                            } else {
                                writeln!(w, "[*] --> {} : {}", s, t)?
                            }
                        }
                    },
                    Node::Decision(_) => {
                        // NOTE: unsure about this
                        unreachable!("invalid transition: None -> Decision")
                    }
                }
            }

            Ok(())
        }
    }
}

#[cfg(feature = "dot")]
pub mod dot {
    use super::{Export, IntermediateAutomaton, Node, Result, Transition};
    use std::{
        fmt::{Debug, Display},
        hash::Hash,
    };

    #[derive(Clone, Copy)]
    pub struct Dot;

    impl super::Format for Dot {}

    const DOT_SPECIAL_NODE: &str =
        r#"label="", fillcolor=black, fixedsize=true, height=0.25, style=filled"#;

    impl<S, T> Export<Dot> for IntermediateAutomaton<S, T>
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(self, w: &mut W, f: Dot) -> Result {
            write!(w, "digraph Automata {{\n")?;
            // TODO add settings

            write!(w, "  _initial_ [{}, shape=circle];\n", DOT_SPECIAL_NODE)?;
            write!(w, "  _final_ [{}, shape=doublecircle];\n", DOT_SPECIAL_NODE)?;

            for s in &self.choices {
                write!(w, "  {} [shape=diamond];\n", s)?
            }
            for (src, v) in &self.delta {
                for (t, dst) in v {
                    (src, t, dst).export(w, f)?
                }
            }

            write!(w, "}}")?;
            Ok(())
        }
    }

    impl<S, T> Export<Dot> for (&Option<S>, &Transition<T>, &Node<S>)
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(self, w: &mut W, _: Dot) -> Result {
            let src = self.0;
            let t = &self.1.transition;
            let dst = self.2;

            if let Some(src) = src {
                match dst {
                    Node::State(state) => match &state.state {
                        None => write!(w, "  {} -> _final_ [label={}];\n", src, t)?,
                        Some(s) => {
                            // if there is a transition label, use that instead of the existing label
                            if let Some(label) = &state.metadata.transition_label {
                                write!(w, "  {} -> {} [label={}];\n", src, label, t)?
                            } else {
                                write!(w, "  {} -> {} [label={}];\n", src, s, t)?
                            }
                        }
                    },
                    Node::Decision(decision) => {
                        for s in decision {
                            if let Some(state) = &s.state {
                                if let Some(label) = &s.metadata.transition_label {
                                    write!(w, "  {} -> {} [label={}];\n", src, state, label)?
                                } else {
                                    write!(w, "  {} -> {};\n", src, state)?
                                }
                            } else {
                                if let Some(label) = &s.metadata.transition_label {
                                    write!(w, "  {} -> _final_ [label={}];\n", src, label)?
                                } else {
                                    write!(w, "  {} -> _final_;\n", src)?
                                }
                            }
                        }
                    }
                }
            } else {
                match dst {
                    Node::State(state) => match &state.state {
                        None => unreachable!("invalid transition: None -> None"),
                        Some(s) => {
                            // if there is a transition label, use that instead of the existing label
                            if let Some(label) = &state.metadata.transition_label {
                                write!(w, "  _initial_ -> {} [label={}];\n", label, t)?
                            } else {
                                write!(w, "  _initial_ -> {} [label={}];\n", s, t)?
                            }
                        }
                    },
                    Node::Decision(_) => {
                        // NOTE: unsure about this
                        unreachable!("invalid transition: None -> Decision")
                    }
                }
            }

            Ok(())
        }
    }
}
