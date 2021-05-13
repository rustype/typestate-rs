extern crate typestate_proc_macro;

pub use ::typestate_proc_macro::{typestate, generated};

#[doc(hidden)]
#[cfg(feature = "debug_mermaid")]
extern crate aquamarine;
