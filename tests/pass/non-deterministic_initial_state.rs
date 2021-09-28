use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[automaton]
    struct S {}

    #[state]
    struct A {}

    #[state]
    struct B {}

    #[state]
    struct Error {}

    trait A {
        fn start() -> Fallible;
        fn next(self) -> B;
    }

    trait B {
        fn end(self);
    }

    trait Error {
        fn consume(self);
    }

    enum Fallible {
        A,
        Error,
    }
}

fn main() {}
