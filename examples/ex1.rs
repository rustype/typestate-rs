use typestate::typestate;

#[typestate]
mod m {
    #[automata]
    struct S {}

    #[state]
    struct A {}

    trait A {
        fn start() -> A;
        fn end(self);
    }
}

fn main() {}