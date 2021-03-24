use traffic_light::*;
use typestate::typestate;


const N_CYCLES_MAINTENANCE: u64 = 1000;

fn main() {
    let red_light = TrafficLight::<Red>::turn_on();
    let mut green_light = red_light.to_green();
    if green_light.requires_maintenance() {
        green_light.reset_cycles()
    }
    let yellow_light = green_light.to_yellow();
    let red_light = yellow_light.to_red();
    red_light.turn_off();
}

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
    }

}

impl GreenState for TrafficLight<Green> {
    fn to_yellow(self) -> TrafficLight<Yellow> {
        println!("Green -> Yellow");
        TrafficLight::<Yellow> {
            cycles: self.cycles,
            state: Yellow,
        }
    }
}

impl YellowState for TrafficLight<Yellow> {
    fn to_red(self) -> TrafficLight<Red> {
        println!("Yellow -> Red");
        TrafficLight::<Red> {
            // increment the cycle
            cycles: self.cycles + 1,
            state: Red,
        }
    }
}

impl RedState for TrafficLight<Red> {
    fn to_green(self) -> TrafficLight<Green> {
        println!("Red -> Green");
        TrafficLight::<Green> {
            cycles: self.cycles,
            state: Green,
        }
    }

    fn turn_on() -> TrafficLight<Red> {
        println!("Turning on...");
        TrafficLight::<Red> {
            cycles: 0,
            state: Red,
        }
    }

    fn turn_off(self) {
        println!("Turning off...");
        // ... consume
    }
}

impl<State> TrafficLight<State>
where
    State: TrafficLightState,
{
    fn requires_maintenance(&self) -> bool {
        self.cycles > N_CYCLES_MAINTENANCE
    }

    fn reset_cycles(&mut self) {
        self.cycles = 0;
    }
}
