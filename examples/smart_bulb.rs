use typestate::typestate;

#[typestate]
mod smart_bulb {
    #[automaton]
    struct SmartBulb {
        cycles: u64,
    }

    #[state]
    struct Off;
    trait Off {
        fn screw() -> Off;
        fn unscrew(self);
        fn turn_on(self) -> On; // Off => On transition
    }

    #[state]
    struct On;
    trait On {
        fn turn_off(self) -> Off;
        fn get_color(&self);
        fn set_color(self, color: (u8, u8, u8)) -> Unknown;
    }

    enum Unknown {
        #[metadata(label = "bulb changed color successfully")]
        On,
        #[metadata(label = "bulb failed and turned off")]
        Off,
    }
}

fn main() {}
