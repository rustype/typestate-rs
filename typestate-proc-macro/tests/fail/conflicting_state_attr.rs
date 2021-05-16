use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[automaton]
    struct S {}

    #[automaton]
    #[state]
    struct A {}
}

fn main() {}