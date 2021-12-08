use typestate::typestate;

#[typestate]
mod m {
    #[automaton]
    struct Player {}

    #[state]
    struct Alive<'name> {
        name: &'name str
    }

    #[state]
    struct Dead<'name> {
        name: &'name str
    }

    enum PlayerLifeState<'name> {
        Alive,
        Dead,
    }

    trait Alive<'name> {
        fn start(name: &str) -> Alive;
        fn damage(self) -> PlayerLifeState;
        fn suicide(self) -> Dead<'name>;
    }

    trait Dead {
        fn end(self);
    }
}

fn main() {}