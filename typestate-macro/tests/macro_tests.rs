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
    // TODO add tests for:
    // duplicate state attributes
    // conflicting state attributes
    // unproductive states
    // useless states
    let t = trybuild::TestCases::new();
    fail!(t[
        empty_module,
        duplicate_automata_attr,
        conflicting_automata_attr,
    ]);
    pass!(t[empty_automata]);
}
