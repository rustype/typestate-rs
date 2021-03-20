# `#[typestate]`

This crate provides a single attribute macro: `#[typestate]`.

The macro is attached to a `mod` and allows for the usage of a pure-Rust macro DSL.

## Defining automata and states

To define automata and states the macro uses attributes.
The attributes are parsed by the macro enabling typestates to be described with minimal effort.

### The `#[automata]` attribute

This attribute defines the main automata structure.
It adds a single generic type parameter, the `State` parameter.
The type parameter will be bounded by a sealed trait, disabling external manual extensions of the acceptable type sets.

### The `#[state]` attribute

This attribute defines a possible state the automata can be in.
It implements the sealed trait which bounds the automata state.

The defined state can contain data, which will be only available when the state is "on".

## Special states

Along with regular states, we can have special cases; the initial and end states.

The key idea behind the declaration of these states is that each one can be thought as a constructor or destructor (I am aware Rust uses `Drop`).
Since each one is a function we can simply write functions and infer which states should be starting states and which ones should be end states.

### Defining initial states

The rule to infer an initial state is as follows:
> If a function does not take a `self` state, but returns a valid state; the returned state is considered to be an initial state.

The following function signatures declare `A` and `B` as initial states.
```rust
fn new_a() -> A;
fn new_b(_: u64) -> B;
```

### Defining end states

Just like initial states, end states are inferred, the rule is:
> If a function takes ownership of a `self` state, but does not return a valid state; the `self` state is considered to be an end state.

The following function signatures declare `C` and `D` as end states.
```rust
fn end_c(self: C);
fn end_d(self: D) -> u64;
```

## Transitions

Transitions are defined and inferred through functions.
Their inference rule is:
> A function is inferred as a transition if it takes ownership of a `self` state, *and* returns a valid state.

```rust
fn a_to_b(self: A) -> B;
```

## Extra functions

Our state machine would be incomplete if we were not able to define functions for a certain state.
The inference rule is similar to that of transitions:
> A function is considered to be part of a certain state if `self` is typed with a valid state.

Remember that this function must not take ownership of the current state.
To cope with that restriction we use references, immutable or mutable.

```rust
fn check_a(self: &A) -> bool;
fn mutate_b_field(self: &mut B);
```

## Code Example

Consider the traffic light example in <https://github.com/rustype/traffic-light>.

Using the `#[typestate]` macro we can write the following code:

```rust
#[typestate]
mod traffic_light {
    #[automata] struct TrafficLight { cycles: u64 }
    #[state] struct Green;
    #[state] struct Yellow;
    #[state] struct Red;

    trait Green { fn to_yellow(self: Green) -> Yellow; }
    trait Yellow { fn to_red(self: Yellow) -> Red; }
    trait Red {
        fn turn_on() -> Red;
        fn turn_off(self: Red);
        fn to_green(self: Red) -> Green;
    }
}
```

Which gets expanded into:

```rust
pub struct TrafficLight<State>
where
    State: TrafficLightState,
{
    cycles: u64,
    state: State,
}

pub struct Green;
pub struct Yellow;
pub struct Red;

mod private {
    use crate::{Green, Red, Yellow};
    pub trait Private {}
    impl Private for Green {}
    impl Private for Yellow {}
    impl Private for Red {}
}

pub trait TrafficLightState: private::Private {}

impl TrafficLightState for Green {}
impl TrafficLightState for Yellow {}
impl TrafficLightState for Red {}

pub trait GreenState {
    fn to_yellow(self) -> TrafficLight<Yellow>;
}

pub trait YellowState {
    fn to_red(self) -> TrafficLight<Red>;
}

pub trait RedState {
    fn to_green(self) -> TrafficLight<Green>;
    fn turn_on() -> TrafficLight<Red>;
    fn turn_off(self);
}
```