use typestate_proc_macro::typestate;
use m::*;

#[typestate(enumerate="ES")]
mod m {
    #[automaton]
    pub struct S;

    #[state]
    pub struct A;

    #[state]
    pub struct B;

    #[state]
    pub struct Error;

    pub trait A {
        fn start() -> A;
        fn next(self) -> Fallible;
    }

    pub trait B {
        fn end(self);
    }

    pub trait Error {
        fn consume(self);
    }

    pub enum Fallible {
        B,
        Error,
    }
}

impl AState for S<A> {
    fn start() -> Self {
        Self { state: A }
    }

    fn next(self) -> Fallible {
        Fallible::B(S { state: B })
    }
}

fn main() {
    let s = S::<A>::start();
    let f = s.next();
    ES::from(f);
}
