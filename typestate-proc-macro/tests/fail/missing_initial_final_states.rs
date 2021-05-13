use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[automata]
    struct S {}

    #[state]
    struct A {}
}

fn main() {}