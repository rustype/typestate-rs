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
fn compile_fail() {
    let t = trybuild::TestCases::new();
    fail!(t[
        empty_module,
        empty_automata,
        duplicate_automata_attr,
        duplicate_state_attr,
        conflicting_automata_attr,
        conflicting_state_attr,
        missing_initial_final_states,
        complex_unreachable_state,
    ]);
    pass!(t[
        stateful_automata,
    ]);
}

// #[test]
// fn compile_pass() {
//     let t = trybuild::TestCases::new();
//     pass!(t[
//         stateful_automata,
//     ]);
// }