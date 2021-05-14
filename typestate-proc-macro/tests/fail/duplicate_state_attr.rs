use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[automata]
    struct S {}

    #[state]
    #[state]
    struct A {}
}

fn main() {}