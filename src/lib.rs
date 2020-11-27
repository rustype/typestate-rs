macro_rules! gen_struct {
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

macro_rules! gen_typestate_structs {
    ($vis:vis $($typestate:ident),+) => {
        $($vis struct $typestate;)+
    }
}

macro_rules! gen_sealed {
    ($mod:ident::$trait:ident [$($typestate:ident),+]) => {
        mod $mod {
            pub trait $trait {}
            $(impl $trait for super::$typestate {})+
        }
    };
    ($trait:ident [$($typestate:ident),+]) => {
        gen_sealed!(sealed::$trait [$($typestate),+]);
    };
    ([$($typestate:ident),+]) => {
        gen_sealed!(sealed::Sealed [$($typestate),+]);
    };
}

macro_rules! gen_state_trait {
    ($vis:vis $trait_name:ident [$($typestate:ident),+]) => {
        $vis trait $trait_name {}
        $(impl $trait_name for $typestate {})+
    };
    (strict $vis:vis $trait_name:ident ($sealed_mod:ident::$sealed_trait:ident) [$($typestate:ident),+]) => {
        gen_sealed!($sealed_mod::$sealed_trait [$($typestate),+]);
        $vis trait $trait_name: $sealed_mod::$sealed_trait {}
        $(impl $trait_name for $typestate {})+
    };
}

#[macro_export]
macro_rules! typestate {
    ($vis:vis $struct_name:ident <$state_name:ident> [$($typestate:ident),+] {$($field:ident:$field_ty:ty),*}) => {
        gen_typestate_structs!($vis $($typestate),+);
        gen_struct!($vis $struct_name<$state_name> {$($field:$field_ty),*});
    };
    (limited
        $vis:vis
        $struct_name:ident
        <$state_name:ident:$state_trait:ident>
        [$($typestate:ident),+]
        {$($field:ident:$field_ty:ty),*}
    ) => {
        gen_typestate_structs!($vis $($typestate),+);
        gen_state_trait!($vis $state_trait [$($typestate),+]);
        gen_struct!($vis $struct_name<$state_name:$state_trait> {$($field:$field_ty),*});
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
        gen_typestate_structs!($vis $($typestate),+);
        gen_state_trait!(strict $vis $state_trait ($sealed_mod::$sealed_trait) [$($typestate),+]);
        gen_struct!($vis $struct_name<$state_name:$state_trait> {$($field:$field_ty),*});
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