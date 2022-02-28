# Macro Attributes

The `#[typestate]` macro exposes some extra features through attribute parameters.
This chapter introduces them and provides simple examples.

## `enumerate`

The `enumerate` parameter will generate an additional `enum` containing all states;
this is useful when dealing with anything that requires a more "general" concept of state.

Consider the file [`examples/light_bulb.rs`](../examples/light_bulb.rs):

```rust
#[typestate(enumerate = "LightBulbStates")]
mod light_bulb {
    #[state] struct On;
    #[state] struct Off;
    // ...
}
```

Using the `enumerate` attribute will add the following `enum` to the expansion:

```rust
pub enum LightBulbStates {
    Off(LightBulb<Off>),
    On(LightBulb<On>),
}
```

## `state_constructors`

The `state_constructors` parameter will generate additional constructors
for each state *with fields*; this is useful when declaring states inside the automaton.

Consider the following example state:

```rust
#[typestate(state_constructors = "new_state")]
mod light_bulb {
    #[state] struct On {
        color: [u8; 3]
    }
    // ...
}
```

When compiled, the following constructor is generated:

```rust
impl On {
    pub fn new_state(color: [u8; 3]) -> Self {
        Self { color }
    }
}
```
