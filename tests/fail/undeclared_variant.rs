use typestate_proc_macro::typestate;

fn main() {}

#[typestate]
mod undeclared_variant {
    #[automaton]
    pub struct A;

    #[state]
    pub struct First;
    pub trait First {
        fn new() -> First;
        fn end(self);
    }

    enum E {
        First,
        Second
    }
}