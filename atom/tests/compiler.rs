use atom::compiler::Context;
use test_case::test_case;

mod common;

#[test_case("unknown_var", Err("CompileError: unknown name 'x'"); "unknown variable")]
#[test_case("unused_var", Err("CompileError: name 'name' is not used"); "unused variable")]
#[test_case("name_used", Err("CompileError: name 'bar' is already defined"); "name used")]
#[cfg_attr(miri, ignore)]
fn compile(name: &str, expected: Result<(), &'static str>) {
    let mut ctx = Context::default();
    let filename = format!("compiler/{name}.atom");
    let result = common::compile(&mut ctx, &filename);

    assert_eq!(
        result.map(|_| ()).map_err(|e| e.to_string()),
        expected.map_err(ToOwned::to_owned)
    );
}
