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
        fn turn_on(self) -> AfterOn;
    }

    #[state]
    pub struct On;
    pub trait On {
        fn turn_off(self) -> Off;
    }

    #[state]
    pub struct Dead;
    pub trait Dead {
        fn turn_off(self) -> Off;
    }

    pub enum AfterOn {
        #[metadata(label="'Bulb is On'")]
        On,
        #[metadata(label="'Bulb is Dead'")]
        Dead
    }
}


fn main() {

}
