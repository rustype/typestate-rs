use typestate_proc_macro::typestate;

#[typestate(enumerate)]
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

    // #[transition]
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
        fn to_either(self) -> Either;
    }
    pub enum A {}

    pub enum Either {
        Yellow,
        Red,
        A
    }
}

fn main() {}