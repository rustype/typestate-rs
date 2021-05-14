//! [<img alt="github" src="https://img.shields.io/badge/github-rustype/typestate-8da0cb?style=flat-square&logo=github">](https://github.com/rustype/typestate-rs)
//! [<img alt="docs" src="https://img.shields.io/badge/docs.rs-typestate-success?style=flat-square">](https://docs.rs/typestate)
//! [<img alt="crates" src="https://img.shields.io/crates/v/typestate?style=flat-square">](https://crates.io/crates/typestate)
//!
//! Are you frustrated with `IllegalStateException`s in Java?
//!
//! Typestates allow you to define *safe* usage protocols for your objects.
//! The compiler will help you on your journey and disallow errors on given states.
//! You will no longer be able to try and read from closed streams.
//!
//! `#[typestate]` builds on ideas from the [`state_machine_future`](https://github.com/fitzgen/state_machine_future) crate.
//! If typestates are so useful, why not use them with limit them to `Future`s?
//!
//! ### Typestates in Rust
//!
//! Typestates are not a new concept to Rust.
//! There are several blog posts on the subject
//! [[1](https://yoric.github.io/post/rust-typestate/),
//! [2](http://cliffle.com/blog/rust-typestate/),
//! [3](https://rustype.github.io/notes/notes/rust-typestate-series/rust-typestate-index)]
//! as well as a [chapter](https://docs.rust-embedded.org/book/static-guarantees/typestate-programming.html) in *The Embedded Rust Book*.
//!
//! In short, we can write typestates by hand, we add some generics here and there,
//! declare them as a "*state*" and in the end we can keep living our lives with our new state machine.
//!
//! This approach however is *error-prone* and *verbose* (especially with bigger automata).
//! It also provides *no* guarantees about the automata, unless of course, you designed and tested the design previously.
//!
//! As programmers, we want to automate this cumbersome job and to do so, we use Rust's powerful procedural macros!
//!
//! ## Basic Guide
//!
//! Consider we are tasked with building the firmware for a traffic light,
//! we can turn it on and off and cycle between Green, Yellow and Red.
//!
//! We first declare a module with the `#[typestate]` macro attached to it.
//! ```rust,ignore
//! #[typestate]
//! mod traffic_light {}
//! ```
//!
//! This of course does nothing, in fact it will provide you an error,
//! saying that we haven't declared an *automata*.
//!
//! And so, our next task is to do that.
//! Inside our `traffic_light` module we declare a structure annotated with `#[automata]`.
//! ```rust,ignore
//! #[automata]
//! pub struct TrafficLight;
//! ```
//!
//! Our next step is to declare the states.
//! We declare three empty structures annotated with `"[state]`.
//! ```rust,ignore
//! #[state] pub struct Green;
//! #[state] pub struct Yellow;
//! #[state] pub struct Red;
//! ```
//!
//! So far so good, however some errors should appear, regarding the lack of initial and final states.
//!
//! To declare initial and final states we need to see them as describable by transitions.
//! Whenever an object is created, the method that created leaves the object in the *initial* state.
//! Equally, whenever a method consumes an object and does not return it (or a similar version of it),
//! it made the object reach the *final* state.
//!
//! With this in mind we can lay down the following rules:
//! - Functions that *do not* take a valid state (i.e. `self`) and return a valid state, describe an initial state.
//! - Functions that take a valid state (i.e. `self`) and *do not* return a valid state, describe a final state.
//!
//! So we write the following function signatures:
//! ```rust,ignore
//! fn turn_on() -> Red;
//! fn turn_off(self);
//! ```
//!
//! However, these are *free* functions, meaning that `self` relates to nothing.
//! To attach them to a state we wrap them around a `trait` with the name of the state they are supposed to be attached to.
//! So our previous example becomes:
//! ```rust,ignore
//! trait Red {
//!     fn turn_on() -> Red;
//!     fn turn_off(self);
//! }
//! ```
//!
//! *Before we go further, a quick review:*
//! > - The module is annotated with `#[typestate]` enabling the DSL.
//! > - To declare the main automaton we attach `#[automata]` to a structure.
//! > - The states are declared by attaching `#[state]`.
//! > - State functions are declared through traits that share the same name.
//! > - Initial and final states are declared by functions with a "special" signature.
//!
//! Finally, we need to address how states transition between each other.
//! An astute reader might have inferred that we can consume one state and return another,
//! such reader would be 100% correct.
//!
//! For example, to transition between the `Red` state and the `Green` we do:
//! ```rust,ignore
//! trait Red {
//!     fn to_green(self) -> Green;
//! }
//! ```
//!
//! Building on this we can finish the other states:
//! ```rust,ignore
//! pub trait Green {
//!     fn to_yellow(self) -> Yellow;
//! }
//!
//! pub trait Yellow {
//!     fn to_red(self) -> Red;
//! }
//!
//! pub trait Red {
//!     fn to_green(self) -> Green;
//!     fn turn_on() -> Red;
//!     fn turn_off(self);
//! }
//! ```
//!
//! And the full code becomes:
//!
//! ```rust,ignore
//! #[typestate]
//! mod traffic_light {
//!     #[automata]
//!     pub struct TrafficLight {
//!         pub cycles: u64,
//!     }
//!
//!     #[state] pub struct Green;
//!     #[state] pub struct Yellow;
//!     #[state] pub struct Red;
//!
//!     pub trait Green {
//!         fn to_yellow(self) -> Yellow;
//!     }
//!
//!     pub trait Yellow {
//!         fn to_red(self) -> Red;
//!     }
//!
//!     pub trait Red {
//!         fn to_green(self) -> Green;
//!         fn turn_on() -> Red;
//!         fn turn_off(self);
//!     }
//! }
//! ```
//!
//! The code above will generate:
//! - Expand the main structure with a `state: State` field.
//! - A sealed trait which disallows states from being added *externally*.
//! - Traits for each state, providing the described functions.
//!
//! ## Advanced Guide
//!
//! There are some features which may be helpful when describing a typestate.
//! There are two main features that weren't discussed yet.
//!
//! ### Self-transitioning functions
//! Putting it simply, states may require to mutate themselves without transitioning, or maybe we require a simple getter.
//! To declare methods for that purpose, we can use functions that take references (mutable or not) to `self`.
//!
//! Consider the following example where we have a flag that can be up or not.
//! We have two functions, one checks if the flag is up, the other, sets the flag up.
//!
//! ```rust,ignore
//! #[state] struct Flag {
//!     up: bool
//! }
//!
//! impl Flag {
//!     fn is_up(&self) -> bool;
//!     fn set_up(&mut self);
//! }
//! ```
//!
//! As these functions do not change the typestate state,
//! they transition back to the current state.
//!
//! ### Non-deterministic transitions
//! Consider that a typestate relies on an external component that can fail, to model that, one would use `Result<T>`.
//! However, we need our typestate to transition between known states, so we declare two things:
//! - An `Error` state along with the other states.
//! - An `enum` to represent the bifurcation of states.
//!
//! ```rust,ignore
//! #[state] struct Error {
//!     message: String
//! }
//!
//! enum OperationResult {
//!     State, Error
//! }
//! ```
//!
//! Inside the enumeration there can only be other valid states and only `Unit` style variants are supported.
//!
//! ## Attributes
//!
//! This is the list of attributes that can be used along `#[typestate]`:
//! - `#[typestate]`: the main attribute macro, without attribute parameters.
//! - `#[typestate(enumerate = "...")]`: this option makes the macro generate an additional `enum`,
//!   the `enum` enables working with variables and structures "generic" to the state.
//!   - The parameter can be declared *with* or *without* a string literal, if declared with the string,
//!     that string will be used as identifier to the `enum`.
//!   - If the parameter is used with an *empty string* or *without* a string, the default behavior is to prepend an `E` to the
//! - `#[typestate(state_constructors = "...")`: this option generates basic constructors for states with fields.
//!
//! ## Features
//! The cargo features you can enable:
//! - `debug_dot` will generate a `.dot` file of your state machine.
//!   - This feature can be customized through the following environment variables (taken from the [DOT documentation](https://graphviz.org/doc/info/attrs.html)):
//!     - `DOT_PAD` - Specifies how much, in inches, to extend the drawing area around the minimal area needed to draw the graph.
//!     - `DOT_NODESEP` - In `dot`, `nodesep` specifies the minimum space between two adjacent nodes in the same rank, in inches.
//!     - `DOT_RANKSEP` - In `dot`, sets the desired rank separation, in inches.
//! - `debug_plantuml` will generate a PlantUML state diagram (`.uml` file) of your state machine.

extern crate typestate_proc_macro;

pub use ::typestate_proc_macro::{generated, typestate};

#[doc(hidden)]
#[cfg(feature = "debug_mermaid")]
pub use ::aquamarine::*;
