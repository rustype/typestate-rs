use typestate::typestate;

#[typestate]
mod m {
    #[automata]
    pub struct TrafficLight {
        cycles: u64
    }
    #[state]
    pub struct Green;
    #[state]
    pub struct Yellow;
    #[state]
    pub struct Red;
}

#[typestate]
mod error_mod {
    #[automata]
    pub struct T;
}

fn main() {}