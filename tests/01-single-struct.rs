use typestate::typestate;

#[typestate]
mod M {
    struct Drone {
        location: (i32, i32)
    }
}