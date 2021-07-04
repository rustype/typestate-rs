use darling::FromMeta;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display, Write},
    hash::Hash,
};

#[derive(Debug, Clone)]
pub struct StateNode<S> {
    state: Option<S>,
    metadata: Metadata,
}

impl<S> StateNode<S> {
    fn new(state: Option<S>) -> Self {
        Self {
            state,
            metadata: Metadata::empty(),
        }
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

// TODO: consider whether `Hash`, `PartialEq` & `Eq` should only take `transition` into account.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    transition: T,
    // metadata: Option<Metadata>,
}

impl<T> Transition<T>
where
    T: Hash + Eq + Debug + Clone + Display,
{
    pub fn new(transition: T) -> Self {
        Self {
            transition,
            // metadata: None,
        }
    }

    pub fn _with_metadata(transition: T, metadata: Metadata) -> Self {
        Self {
            transition,
            // metadata: metadata.into(),
        }
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
    transition_label: Option<String>,
}

impl Metadata {
    fn empty() -> Self {
        Self {
            transition_label: None,
        }
    }

    fn new(label: String) -> Self {
        Self {
            transition_label: Some(label),
        }
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Self::new(String::new())
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


type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub trait DisplayMermaid {
    fn into_mermaid(self) -> Result<String>;
}

impl<S, T> DisplayMermaid for IntermediateAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    fn into_mermaid(self) -> Result<String> {
        let mut res = String::new();
        writeln!(&mut res, "stateDiagram-v2")?;
        for s in &self.choices {
            writeln!(&mut res, "state {} <<choice>>", s)?
        }
        for s in &self.states {
            writeln!(&mut res, "state {}", s)?
        }
        for (src, v) in &self.delta {
            for (t, dst) in v {
                writeln!(&mut res, "{}", (src, t, dst).into_mermaid()?)?
            }
        }
        Ok(res)
    }
}

impl<S, T> DisplayMermaid for (&Option<S>, &Transition<T>, &Node<S>)
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    fn into_mermaid(self) -> Result<String> {
        let src = self.0;
        let t = &self.1.transition;
        let dst = self.2;
        let mut res = String::new();

        if let Some(src) = src {
            match dst {
                Node::State(state) => match &state.state {
                    None => writeln!(&mut res, "{} --> [*] : {}", src, t)?,
                    Some(s) => {
                        // if there is a transition label, use that instead of the existing label
                        if let Some(label) = &state.metadata.transition_label {
                            writeln!(&mut res, "{} --> {} : {}", src, label, t)?
                        } else {
                            writeln!(&mut res, "{} --> {} : {}", src, s, t)?
                        }
                    }
                },
                Node::Decision(decision) => {
                    for s in decision {
                        if let Some(state) = &s.state {
                            if let Some(label) = &s.metadata.transition_label {
                                writeln!(&mut res, "{} --> {} : {}", src, state, label)?
                            } else {
                                writeln!(&mut res, "{} --> {}", src, state)?
                            }
                        } else {
                            if let Some(label) = &s.metadata.transition_label {
                                writeln!(&mut res, "{} --> [*] : {}", src, label)?
                            } else {
                                writeln!(&mut res, "{} --> [*]", src)?
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
                            writeln!(&mut res, "[*] --> {} : {}", label, t)?
                        } else {
                            writeln!(&mut res, "[*] --> {} : {}", s, t)?
                        }
                    }
                },
                Node::Decision(decision) => {
                    // NOTE: unsure about this
                    unreachable!("invalid transition: None -> Decision")
                }
            }
        }

        Ok(res)
    }
}
