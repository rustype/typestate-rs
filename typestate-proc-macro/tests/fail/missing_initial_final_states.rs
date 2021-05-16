use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[automaton]
    struct S {}

    #[state]
    struct A {}
}

fn main() {}