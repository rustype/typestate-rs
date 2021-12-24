//! The `export` module contains the code which "compiles"
//! typestate graphs into formats other tools understand.
//!
//! Currently, the following formats are supported:
//! - `DOT`
//! - `PlantUML`
//! - `MermaidJS`
//!
//! To add a new format you will need to create a new `struct`
//! representing the format, implement `Format` for it and then
//! implement `Export` for `IntermediateGraph` where the generic type
//! `Export` takes, is your format.
//!
//! ```rust,ignore
//! impl Export<YourFormat> for IntermediateGraph<S, T> { ... }
//! ```

/// Type alias for `()` or [`std::error::Error`].
#[allow(dead_code)]
type Result = std::result::Result<(), Box<dyn std::error::Error>>;

/// Blanket trait for [`Export`] implementations.
pub trait Format {
    /// The current format file extension.
    fn file_extension<'a>() -> &'a str;
}

/// Used to declare output formats for the [`IntermediateAutomaton`].
pub trait Export<F: Format> {
    /// Export the implementing type as format `F` to the output stream `w`.
    fn export<W: std::io::Write>(&self, w: &mut W, _: F) -> Result;
}

/// The Mermaid format module, containing the marker type and implementation for the respective export trait.
#[cfg(feature = "docs-mermaid")]
pub mod mermaid {
    use super::{Export, Result};
    use crate::igraph::{IntermediateGraph, Node, Transition};
    use std::{
        fmt::{Debug, Display},
        hash::Hash,
    };

    /// The mermaid format struct.
    #[derive(Clone, Copy)]
    pub struct Mermaid;

    /// Blanket implementation for the [`Mermaid`] format.
    impl super::Format for Mermaid {
        fn file_extension<'a>() -> &'a str {
            ".mermaid"
        }
    }

    impl<S, T> Export<Mermaid> for IntermediateGraph<S, T>
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(&self, w: &mut W, f: Mermaid) -> Result {
            writeln!(w, "stateDiagram-v2")?;

            if let Some(v) = self.delta.get(&None) {
                for (t, dst) in v {
                    (t, dst).export(w, f)?
                }
            }

            for choice in &self.choices {
                writeln!(w, "state {} <<choice>>", choice)?;
            }

            for (src, v) in &self.delta {
                if let Some(src) = src {
                    for (t, dst) in v {
                        (src, t, dst).export(w, f)?
                    }
                }
            }

            Ok(())
        }
    }

    impl<S, T> Export<Mermaid> for (&Transition<T>, &Node<S>)
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(&self, w: &mut W, _: Mermaid) -> Result {
            let t = &self.0.transition;
            let dst = self.1;
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
            Ok(())
        }
    }

    impl<S, T> Export<Mermaid> for (&S, &Transition<T>, &Node<S>)
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(&self, w: &mut W, _: Mermaid) -> Result {
            let src = self.0;
            let t = &self.1.transition;
            let dst = self.2;

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

            Ok(())
        }
    }
}

/// The PlantUML format module, containing the marker type and implementation for the respective export trait.
#[cfg(feature = "export-plantuml")]
pub mod plantuml {
    use super::{Export, Result};
    use crate::igraph::{IntermediateGraph, Node, Transition};
    use std::{
        fmt::{Debug, Display},
        hash::Hash,
    };

    #[derive(Clone, Copy)]
    pub struct PlantUml;

    impl super::Format for PlantUml {
        fn file_extension<'a>() -> &'a str {
            ".uml"
        }
    }

    impl<S, T> Export<PlantUml> for IntermediateGraph<S, T>
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(&self, w: &mut W, f: PlantUml) -> Result {
            writeln!(w, "@startuml")?;
            writeln!(w, "hide empty description")?;

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

            if let Some(v) = self.delta.get(&None) {
                for (t, dst) in v {
                    (t, dst).export(w, f)?
                }
            }

            for choice in &self.choices {
                writeln!(w, "state {} <<choice>>", choice)?;
            }

            for (src, v) in &self.delta {
                if let Some(src) = src {
                    for (t, dst) in v {
                        (src, t, dst).export(w, f)?
                    }
                }
            }

            writeln!(w, "@end")?;

            Ok(())
        }
    }

    impl<S, T> Export<PlantUml> for (&Transition<T>, &Node<S>)
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(&self, w: &mut W, _: PlantUml) -> Result {
            let t = &self.0.transition;
            let dst = self.1;

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

            Ok(())
        }
    }

    impl<S, T> Export<PlantUml> for (&S, &Transition<T>, &Node<S>)
    where
        S: Hash + Eq + Debug + Clone + Display,
        T: Hash + Eq + Debug + Clone + Display,
    {
        fn export<W: std::io::Write>(&self, w: &mut W, _: PlantUml) -> Result {
            let src = self.0;
            let t = &self.1.transition;
            let dst = self.2;

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

            Ok(())
        }
    }
}

/// The DOT format module, containing the marker type and implementation for the respective export trait.
#[cfg(feature = "export-dot")]
pub mod dot {
    use super::{Export, Result};
    use crate::igraph::{IntermediateGraph, Node, Transition};
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

    impl super::Format for Dot {
        fn file_extension<'a>() -> &'a str {
            ".dot"
        }
    }

    const DOT_SPECIAL_NODE: &str =
        r#"label="", fillcolor=black, fixedsize=true, height=0.25, style=filled"#;

    impl<S, T> Export<Dot> for IntermediateGraph<S, T>
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

#[cfg(any(feature = "export-dot", feature = "export-plantuml"))]
use {super::IntermediateGraph, std::fs::File, syn::Ident};

#[cfg(any(feature = "export-dot", feature = "export-plantuml"))]
pub(crate) fn export<F: Format>(
    file_name: &str,
    igraph: &IntermediateGraph<Ident, Ident>,
    format: F,
) -> Result
where
    IntermediateGraph<Ident, Ident>: Export<F>,
{
    let folder_path = ::std::env::var_os("EXPORT_FOLDER")
        .and_then(|s| s.into_string().ok())
        .unwrap_or_else(|| "./".to_string());
    let mut f = File::create(format!(
        "{}{}{}",
        folder_path,
        file_name,
        F::file_extension(),
    ))?;

    igraph.export(&mut f, format)?;
    Ok(())
}
