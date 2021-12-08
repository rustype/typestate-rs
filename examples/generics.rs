use typestate::typestate;

#[typestate]
mod my_state {
    #[automaton]
    pub struct MyState;

    #[state]
    pub struct State1<T> {
        data: T,
    }

    trait State1 {
        fn new<T>() -> State1<T>;

        fn done(self);
    }
}

fn main() {}