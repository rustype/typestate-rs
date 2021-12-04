# Basic Guide to Typestates

Consider we are tasked with building the firmware for a traffic light,
we can turn it on and off and cycle between Green, Yellow and Red.

We first declare a module with the `#[typestate]` macro attached to it.
```rust,noplaypen
#[typestate]
mod traffic_light {}
```

This of course does nothing, in fact it will provide you an error,
saying that we haven't declared an *automaton*.

And so, our next task is to do that.
Inside our `traffic_light` module we declare a structure annotated with `#[automaton]`.
```rust,noplaypen
#[automaton]
pub struct TrafficLight;
```

Our next step is to declare the states.
We declare three empty structures annotated with `"[state]`.
```rust,noplaypen
#[state] pub struct Green;
#[state] pub struct Yellow;
#[state] pub struct Red;
```

So far so good, however some errors should appear, regarding the lack of initial and final states.

To declare initial and final states we need to see them as describable by transitions.
Whenever an object is created, the method that created leaves the object in the *initial* state.
Equally, whenever a method consumes an object and does not return it (or a similar version of it),
it made the object reach the *final* state.

With this in mind we can lay down the following rules:
- Functions that *do not* take a valid state (i.e. `self`) and return a valid state, describe an initial state.
- Functions that take a valid state (i.e. `self`) and *do not* return a valid state, describe a final state.

So we write the following function signatures:
```rust,noplaypen
fn turn_on() -> Red;
fn turn_off(self);
```

However, these are *free* functions, meaning that `self` relates to nothing.
To attach them to a state we wrap them around a `trait` with the name of the state they are supposed to be attached to.
So our previous example becomes:
```rust,noplaypen
trait Red {
    fn turn_on() -> Red;
    fn turn_off(self);
}
```

*Before we go further, a quick review:*
> - The module is annotated with `#[typestate]` enabling the DSL.
> - To declare the main automaton we attach `#[automaton]` to a structure.
> - The states are declared by attaching `#[state]`.
> - State functions are declared through traits that share the same name.
> - Initial and final states are declared by functions with a "special" signature.

Finally, we need to address how states transition between each other.
An astute reader might have inferred that we can consume one state and return another,
such reader would be 100% correct.

For example, to transition between the `Red` state and the `Green` we do:
```rust,noplaypen
trait Red {
    fn to_green(self) -> Green;
}
```

Building on this we can finish the other states:
```rust,noplaypen
pub trait Green {
    fn to_yellow(self) -> Yellow;
}

pub trait Yellow {
    fn to_red(self) -> Red;
}

pub trait Red {
    fn to_green(self) -> Green;
    fn turn_on() -> Red;
    fn turn_off(self);
}
```

And the full code becomes:

```rust,noplaypen
#[typestate]
mod traffic_light {
    #[automaton]
    pub struct TrafficLight {
        pub cycles: u64,
    }

    #[state] pub struct Green;
    #[state] pub struct Yellow;
    #[state] pub struct Red;

    pub trait Green {
        fn to_yellow(self) -> Yellow;
    }

    pub trait Yellow {
        fn to_red(self) -> Red;
    }

    pub trait Red {
        fn to_green(self) -> Green;
        fn turn_on() -> Red;
        fn turn_off(self);
    }
}
```

The code above will generate:
- Expand the main structure with a `state: State` field.
- A sealed trait which disallows states from being added *externally*.
- Traits for each state, providing the described functions.