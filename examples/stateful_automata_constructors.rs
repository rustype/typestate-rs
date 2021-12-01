use typestate_proc_macro::typestate;

#[typestate(state_constructors = "new")]
mod m {
    #[automaton]
    struct S {}

    #[state]
    struct A {}

    trait A {
        fn start() -> A;
        fn end(self);
    }
}

fn main() {}
