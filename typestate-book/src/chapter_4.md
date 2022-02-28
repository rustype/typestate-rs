# Advanced Guide

There are some features which may be helpful when describing a typestate.
There are two main features that weren't discussed yet.

## Self-transitioning functions
Putting it simply, states may require to mutate themselves without transitioning, or maybe we require a simple getter.
To declare methods for that purpose, we can use functions that take references (mutable or not) to `self`.

Consider the following example where we have a flag that can be up or not.
We have two functions, one checks if the flag is up, the other, sets the flag up.

```rust,noplaypen
#[state] struct Flag {
    up: bool
}

impl Flag {
    fn is_up(&self) -> bool;
    fn set_up(&mut self);
}
```

As these functions do not change the typestate state,
they transition back to the current state.

## Non-deterministic transitions
Consider that a typestate relies on an external component that can fail, to model that, one would use `Result<T>`.
However, we need our typestate to transition between known states, so we declare two things:
- An `Error` state along with the other states.
- An `enum` to represent the bifurcation of states.

```rust,noplaypen
#[state] struct Error {
    message: String
}

enum OperationResult {
    State, Error
}
```

Inside the enumeration there can only be other valid states and only `Unit` style variants are supported.
