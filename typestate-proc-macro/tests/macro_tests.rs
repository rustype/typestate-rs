// 1.51 because that is around when the crate's development started
#[rustversion::stable(1.51)]
#[test]
fn compile() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/*.rs");
    t.pass("tests/pass/*.rs");
}