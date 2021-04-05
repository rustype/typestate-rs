use typestate::typestate;

#[typestate]
mod m {
    #[automata]
    struct S {}

    #[automata]
    #[state]
    struct A {}
}

fn main() {}