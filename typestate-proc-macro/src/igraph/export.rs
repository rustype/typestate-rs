// TODO: document module

/// Type alias for `()` or [`std::error::Error`].
#[allow(dead_code)]
type Result = std::result::Result<(), Box<dyn std::error::Error>>;

/// Blanket trait for [`Export`] implementations.
pub trait Format {}

/// Used to declare output formats for the [`IntermediateAutomaton`].
pub trait Export<F: Format> {
    /// Export the implementing type as format `F` to the output stream `w`.
    // TODO: this can be &self
    fn export<W: std::io::Write>(&self, w: &mut W, _: F) -> Result;
}

/// The Mermaid format module, containing the marker type and implementation for the respective export trait.
#[cfg(feature = "mermaid")]
pub mod mermaid {
    use super::{Export, Result};
    use crate::igraph::{IntermediateAutomaton, Node, Transition};
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
        fn export<W: std::io::Write>(&self, w: &mut W, f: Mermaid) -> Result {
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
        fn export<W: std::io::Write>(&self, w: &mut W, _: Mermaid) -> Result {
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
                            } else if let Some(label) = &s.metadata.transition_label {
                                writeln!(w, "{} --> [*] : {}", src, label)?
                            } else {
                                writeln!(w, "{} --> [*]", src)?
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

/// The PlantUML format module, containing the marker type and implementation for the respective export trait.
#[cfg(feature = "plantuml")]
pub mod plantuml {
    use super::{Export, Result};
    use crate::igraph::{IntermediateAutomaton, Node, Transition};
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
        fn export<W: std::io::Write>(&self, w: &mut W, f: PlantUml) -> Result {
            writeln!(w, "@startuml")?;
            if let Some(s) = ::std::env::var_os("PLANTUML_NODESEP") {
                w.write_fmt(format_args!(
                    "skinparam nodesep {}\n",
                    s.into_string().unwrap_or_else(|_| "30".to_string())
                ))?;
            }
            if let Some(s) = ::std::env::var_os("PLANTUML_RANKSEP") {
                w.write_fmt(format_args!(
                    "skinparam ranksep {}\n",
                    s.into_string().unwrap_or_else(|_| "30".to_string())
                ))?;
            }
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
        fn export<W: std::io::Write>(&self, w: &mut W, _: PlantUml) -> Result {
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
                            } else if let Some(label) = &s.metadata.transition_label {
                                writeln!(w, "{} --> [*] : {}", src, label)?
                            } else {
                                writeln!(w, "{} --> [*]", src)?
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

/// The DOT format module, containing the marker type and implementation for the respective export trait.
#[cfg(feature = "dot")]
pub mod dot {
    use super::{Export, Result};
    use crate::igraph::{IntermediateAutomaton, Node, Transition};
    use std::{
        fmt::{Debug, Display},
        hash::Hash,
    };

    fn var_or_default(var_name: &str, var_default: &str) -> String {
        ::std::env::var_os(var_name)
            .and_then(|s| s.into_string().ok())
            .unwrap_or_else(|| var_default.to_string())
    }

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
        fn export<W: std::io::Write>(&self, w: &mut W, f: Dot) -> Result {
            writeln!(w, "digraph Automata {{")?;

            w.write_fmt(format_args!(
                "  graph [pad=\"{}\", nodesep=\"{}\", ranksep=\"{}\"];\n",
                var_or_default("DOT_PAD", "0.25"),
                var_or_default("DOT_NODESEP", "0.75"),
                var_or_default("DOT_RANKSEP", "1"),
            ))?;

            writeln!(w, "  _initial_ [{}, shape=circle];", DOT_SPECIAL_NODE)?;
            writeln!(w, "  _final_ [{}, shape=doublecircle];", DOT_SPECIAL_NODE)?;

            for s in &self.choices {
                writeln!(w, "  {} [shape=diamond];", s)?
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
        fn export<W: std::io::Write>(&self, w: &mut W, _: Dot) -> Result {
            let src = self.0;
            let t = &self.1.transition;
            let dst = self.2;

            if let Some(src) = src {
                match dst {
                    Node::State(state) => match &state.state {
                        None => writeln!(w, "  {} -> _final_ [label=\"{}\"];", src, t)?,
                        Some(s) => {
                            // if there is a transition label, use that instead of the existing label
                            if let Some(label) = &state.metadata.transition_label {
                                writeln!(w, "  {} -> {} [label=\"{}\"];", src, label, t)?
                            } else {
                                writeln!(w, "  {} -> {} [label=\"{}\"];", src, s, t)?
                            }
                        }
                    },
                    Node::Decision(decision) => {
                        for s in decision {
                            if let Some(state) = &s.state {
                                if let Some(label) = &s.metadata.transition_label {
                                    writeln!(w, "  {} -> {} [label=\"{}\"];", src, state, label)?
                                } else {
                                    writeln!(w, "  {} -> {};", src, state)?
                                }
                            } else if let Some(label) = &s.metadata.transition_label {
                                writeln!(w, "  {} -> _final_ [label=\"{}\"];", src, label)?
                            } else {
                                writeln!(w, "  {} -> _final_;", src)?
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
                                writeln!(w, "  _initial_ -> {} [label=\"{}\"];", label, t)?
                            } else {
                                writeln!(w, "  _initial_ -> {} [label=\"{}\"];", s, t)?
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
