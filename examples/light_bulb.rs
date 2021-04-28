use typestate::typestate;

#[typestate]
mod light_bulb {
    #[automata]
    struct LightBulb;

    #[state] struct Off;
    trait Off {
        fn screw() -> Off;
        fn unscrew(self);
        fn turn_on(self) -> On; // Off => On transition
    }

    #[state] struct On;
    trait On {
        fn turn_off(self) -> Off;
    }
}

fn main() {}