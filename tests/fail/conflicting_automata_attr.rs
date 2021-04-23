use typestate::typestate;

#[typestate]
mod m {
    #[state]
    #[automata] // this should error because it is the second one
    struct S {}
}

fn main() {}