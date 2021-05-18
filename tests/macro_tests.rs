// this stable means like "the latest stable"
// minimum ensured so far is 1.51
#[rustversion::stable]
#[test]
fn compile() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/*.rs");
    t.pass("tests/pass/*.rs");
}