macro_rules! fail {
    ($tests:ident [$($head:ident),* $(,)?]) => {
        $( $tests.compile_fail(format!("tests/{}.rs", stringify!($head))); )*
    };
}

macro_rules! pass {
    ($tests:ident [$($head:ident),* $(,)?]) => {
        $( $tests.pass(format!("tests/{}.rs", stringify!($head))); )*
    };
}

#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    fail!(t[
        empty_module,
        duplicate_automata_attr,
        duplicate_state_attr,
        conflicting_automata_attr,
        conflicting_state_attr,
        unproductive_state,
        useless_state,
    ]);
    pass!(t[
        empty_automata,
        stateful_automata,
    ]);
}
