/**
The expansion of this macro generates a typestated `struct` with a `__state` field.
*/
macro_rules! __gen_struct {
    ($vis:vis $struct_name:ident <$state_name:ident> {$($field:ident:$field_type:ty),*}) => {
        $vis struct $struct_name<$state_name> {
            __state: std::marker::PhantomData<$state_name>,
            $($field:$field_type,)*
        }
    };
    ($vis:vis $struct_name:ident <$state_name:ident:$state_trait:ident> {$($field:ident:$field_type:ty),*}) => {
        $vis struct $struct_name<$state_name>
        where
            $state_name: $state_trait,
        {
            __state: std::marker::PhantomData<$state_name>,
            $($field:$field_type,)*
        }
    };
}

/**
Generates the empty `struct`s which represent state on the typesystem.
*/
macro_rules! __gen_typestate_structs {
    ($vis:vis $($typestate:ident),+) => {
        $($vis struct $typestate;)+
    }
}

/**
Generates the sealed trait implementation.
*/
macro_rules! __gen_sealed {
    ($mod:ident::$trait:ident [$($typestate:ident),+]) => {
        mod $mod {
            pub trait $trait {}
            $(impl $trait for super::$typestate {})+
        }
    };
    ($trait:ident [$($typestate:ident),+]) => {
        __gen_sealed!(sealed::$trait [$($typestate),+]);
    };
    ([$($typestate:ident),+]) => {
        __gen_sealed!(sealed::Sealed [$($typestate),+]);
    };
}

/**
Generates the traits for the `limited` and `strict` typestate variants.
*/
macro_rules! __gen_state_trait {
    ($vis:vis $trait_name:ident [$($typestate:ident),+]) => {
        $vis trait $trait_name {}
        $(impl $trait_name for $typestate {})+
    };
    (strict $vis:vis $trait_name:ident ($sealed_mod:ident::$sealed_trait:ident) [$($typestate:ident),+]) => {
        __gen_sealed!($sealed_mod::$sealed_trait [$($typestate),+]);
        $vis trait $trait_name: $sealed_mod::$sealed_trait {}
        $(impl $trait_name for $typestate {})+
    };
}

/**
Describe a new `struct` and its possible typestates.

Typestate descriptions can be prepended with the `limited` or `strict` keywords,
without them the resulting expansion can be extended by a user without bounds.

The `limited` keyword limits the possible state types with a trait bound,
the resulting expasion will look similar to:

```rust
trait Limit {}
struct S<State> where S: Limit { /**/ }
```

The `limited` keyword generates the new trait to avoid users from writing an implementation for any type.
To add a new state to the `State` set, the new `struct` must implement the trait bound.

```rust
struct NewState;
impl Limit for NewState {}
impl S<NewState> {} // now valid
```

The `strict` keyword implements the [sealed trait pattern](https://rust-lang.github.io/api-guidelines/future-proofing.html).
Making external users are unable to extend the `State` set.

Syntax:
```
(limited|strict)? $visibility? $struct_name
$(<$state_name $(:$state_trait_bound)?>)?
($($strict_mod_name::)? $strict_mod_trait?)?
[$($typestate),+]
{$($struct_field),+}
```

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
*/
#[macro_export(local_inner_macros)]
macro_rules! typestate {
    ($vis:vis $struct_name:ident <$state_name:ident> [$($typestate:ident),+] {$($field:ident:$field_ty:ty),*}) => {
        __gen_typestate_structs!($vis $($typestate),+);
        __gen_struct!($vis $struct_name<$state_name> {$($field:$field_ty),*});
    };
    (limited
        $vis:vis
        $struct_name:ident
        <$state_name:ident:$state_trait:ident>
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        __gen_typestate_structs!($vis $($typestate),+);
        __gen_state_trait!($vis $state_trait [$($typestate),+]);
        __gen_struct!($vis $struct_name<$state_name:$state_trait> {$($field:$field_ty),*});
    };
    (limited
        $vis:vis
        $struct_name:ident
        <$state_name:ident>
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        typestate!(
            limited
            $vis
            $struct_name
            <$state_name:Limited>
            [$($typestate),+]
            {$($field:$field_ty),*}
        );
    };
    (limited
        $vis:vis
        $struct_name:ident
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        typestate!(
            limited
            $vis
            $struct_name
            <State:Limited>
            [$($typestate),+]
            {$($field:$field_ty),*}
        );
    };
    (strict $vis:vis $struct_name:ident
        <$state_name:ident:$state_trait:ident>
        ($sealed_mod:ident::$sealed_trait:ident)
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        __gen_typestate_structs!($vis $($typestate),+);
        __gen_state_trait!(strict $vis $state_trait ($sealed_mod::$sealed_trait) [$($typestate),+]);
        __gen_struct!($vis $struct_name<$state_name:$state_trait> {$($field:$field_ty),*});
    };
    (strict
        $vis:vis
        $struct_name:ident
        <$state_name:ident:$state_trait:ident>
        ($sealed_trait:ident)
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        typestate!(strict $vis $struct_name <$state_name:$state_trait> (sealed::$sealed_trait) [$($typestate),+] {$($field:$field_ty),*});
    };
    (strict
        $vis:vis
        $struct_name:ident
        <$state_name:ident:$state_trait:ident>
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        typestate!(strict $vis $struct_name <$state_name:$state_trait> (Sealed) [$($typestate),+] {$($field:$field_ty),*});
    };
    (strict
        $vis:vis
        $struct_name:ident
        <$state_name:ident>
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        typestate!(strict $vis $struct_name <$state_name:Limited> [$($typestate),+] {$($field:$field_ty),*});
    };
    (strict
        $vis:vis
        $struct_name:ident
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        typestate!(strict $vis $struct_name <State> [$($typestate),+] {$($field:$field_ty),*});
    };
}