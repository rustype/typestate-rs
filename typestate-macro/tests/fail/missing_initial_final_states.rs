use typestate::typestate;

#[typestate]
mod m {
    #[automata]
    struct S {}

    #[state]
    struct A {}
}

fn main() {}