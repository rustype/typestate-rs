#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-single-struct.rs");
    t.compile_fail("tests/02-no-mod.rs");
}
