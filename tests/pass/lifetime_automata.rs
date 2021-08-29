use typestate_proc_macro::typestate;

#[typestate]
mod m {
    #[automaton]
    struct Game {}

    #[state]
    struct Player<'name> {
        name: &'name str
    }

    trait Player {
        fn start(name: &str) -> Player;
        fn end(self);
    }
}

fn main() {}