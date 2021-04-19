use typestate::typestate;

fn main() {}

#[typestate]
mod undeclared_variant {
    #[automata]
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