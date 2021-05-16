use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[state]
    #[automaton] // this should error because it is the second one
    struct S {}
}

fn main() {}