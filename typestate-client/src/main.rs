use typestate::typestate;

#[typestate]
mod m {
    #[automata]
    struct Drone {
        location: (i32, i32)
    }

    #[automata]
    struct DroneB {
        location: (i32, i32)
    }

    #[state]
    struct Grounded;
}

fn main() {}