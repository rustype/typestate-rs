use typestate::typestate;

#[typestate]
mod traffic_light {
    #[automata]
    pub struct TrafficLight {
        pub cycles: u64,
    }

    #[state]
    pub struct Green;

    #[state]
    pub struct Yellow;

    #[state]
    pub struct Red;

    #[state]
    pub struct RedA;

    pub trait Green {
        fn to_yellow(self) -> Yellow;
    }

    pub trait Yellow {
        fn to_red(self) -> Red;
    }

    pub trait Red {
        fn to_green(self) -> Green;
        fn turn_on() -> Red;
        fn turn_off(self);
    }
}

fn main() {}
