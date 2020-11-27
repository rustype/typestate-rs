# Typestate

Currently, this crate exports a single macro: `typestate!`.

The macro defines a small DSL to generate typestate-ready structures.

## Syntax

The macro is defined by the following (pseudo) syntax:

```
(limited|strict)? $visibility? $struct_name
$(<$state_name $(:$state_trait_bound)?>)?
($($strict_mod_name::)? $strict_mod_trait)?
[$($typestate),+]
{$($struct_field),+}
```

## Usage Examples

Example of unconstrained typestate:
```rust
typestate!(
    Drone [Idle, Hovering, Flying] {
        x: f32,
        y: f32
    }
);
```

Example of a limited typestate:
```rust
typestate!(
    limited Drone [Idle, Hovering, Flying] {
        x: f32,
        y: f32
    }
);
```

Example of a strict typestate:
```rust
typestate!(
    strict Drone [Idle, Hovering, Flying] {
        x: f32,
        y: f32
    }
);
```