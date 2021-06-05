use light_bulb::*;
use typestate::typestate;

#[typestate]
mod light_bulb {
    #[automaton]
    pub struct LightBulb;

    #[state]
    pub struct Off;
    pub trait Off {
        fn screw() -> Off;
        fn unscrew(self);
        fn turn_on(self) -> On; // Off => On transition
    }

    #[state]
    pub struct On;
    pub trait On {
        fn turn_off(self) -> Off;
    }
}

impl OffState for LightBulb<Off> {
    fn screw() -> LightBulb<Off> {
        Self { state: Off }
    }
    fn unscrew(self) {}
    fn turn_on(self) -> LightBulb<On> {
        LightBulb::<On> { state: On }
    }
}

impl OnState for LightBulb<On> {
    fn turn_off(self) -> LightBulb<Off> {
        LightBulb::<Off> { state: Off }
    }
}

fn main() {
    let bulb = LightBulb::<Off>::screw();
    let bulb = bulb.turn_on();
    let bulb = bulb.turn_off();
    let _ = bulb.turn_off();
}
