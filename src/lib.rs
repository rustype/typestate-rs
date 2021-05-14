extern crate typestate_proc_macro;

pub use ::typestate_proc_macro::{generated, typestate};

#[doc(hidden)]
#[cfg(feature = "debug_mermaid")]
pub use ::aquamarine::*;
