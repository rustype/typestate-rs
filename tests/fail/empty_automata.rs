use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[automaton]
    struct S {}
}

fn main() {}