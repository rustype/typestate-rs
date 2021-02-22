# Macro Syntax

## Function-like Macro

```rust
// Both approaches compile to something like this
// <https://github.com/rustype/drone/blob/main/src/drone.rs>

// Function-like Macro
typestate! { Drone {
    struct Grounded { location: Coordinates }
    struct Hovering { location: Coordinates }
    struct Flying {
        location:    Coordinates,
        destination: Coordinates,
    }
    enum Landed {
        Success(Grounded),  // houston, touchdown!!
        Error,              // crashed
    }
    fn get_location(&self: Grounded) -> &Coordinates;
    fn correct_coordinates(&mut self: Grounded);
    fn take_off(self: Grounded) -> Hovering;
    fn fly_to(self: Hovering, dst: Coordinates) -> Hovering;
    fn land(self: Hovering) -> Landed;
    fn new() -> Grounded;   // defines the start state
    fn end(self: Grounded); // defines the final state
} }
```


## Attribute Macro

```rust
// Attribute Macro
// The key insights into the following syntax are:
// - attr macros replace the input stream
// - the input stream just needs to be similar enough to rust
// - we can use inner attr macros to mark structs, states and friends for further processing
#[typestate]
mod M {
    struct Drone {
        location: Coordinates
    }
    #[state] struct Grounded;
    #[state] struct Hovering;
    #[state] struct Flying {
        destination: Coordinates
    }
    #[state] enum Landed {
        Success(Grounded),
        Error
    }

    // either gets inferred or can be enhanced with more attr macros
    fn get_location(self: &Grounded) -> &Coordinates;
    fn correct_coordinates(self: &mut Grounded);
    fn take_off(self: Grounded) -> Hovering;
    fn fly_to(self: Hovering, dst: Coordinates) -> Hovering;
    fn land(self: Hovering) -> Landed;
    fn new() -> Grounded;   // defines the start state
    fn end(self: Grounded); // defines the final state
}
```