use typestate_lite::automaton;
use typestate_lite::state;

#[automaton]
struct LightBulb {}

#[state(LightBulb)]
struct Off;

#[state(LightBulb)]
struct On {
    intensity: f32,
}

#[state(LightBulb)]
enum Failed {
    On,
    Off,
}

impl LightBulb<Off> {
    fn new() -> Self {
        Self { state: Off }
    }
}
