use typestate::typestate;

#[typestate]
mod m {
    #[automata]
    struct S {}
}

fn main() {}