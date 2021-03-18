use typestate::typestate;

#[typestate]
mod m {
    #[automata]
    pub struct TrafficLight {
        cycles: u64,
    }
    #[state]
    pub struct Green;
    #[state]
    pub struct Yellow;
    #[state]
    pub struct Red;
    // #[transition]
    trait Green {
        fn to_yellow(self) -> Yellow;
    }
    trait Yellow {
        fn to_red(self) -> Red;
    }
    trait Red {
        fn to_green(self) -> Green;
    }
}

#[typestate]
mod error_mod {
    #[automata]
    pub struct T;
}

fn main() {}
