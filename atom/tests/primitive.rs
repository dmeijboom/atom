use atom::{backend::GlobalContext, runtime::Gc};
use test_case::test_case;

mod common;

#[test_case("basic", Ok(42); "basic integer")]
#[test_case("negative", Ok(-42); "negative integer")]
#[test_case("min", Ok(-9_223_372_036_854_775_808); "minimum integer")]
#[test_case("max", Ok(9_223_372_036_854_775_807i64); "maximum integer")]
#[test_case("double-sign", Err("ParseError: invalid expr '-'"); "double sign not allowed")]
#[cfg_attr(miri, ignore)]
fn int(name: &str, expected: Result<i64, &'static str>) {
    let mut gc = Gc::default();
    let ctx = GlobalContext::default();
    let return_value = common::run(&mut gc, ctx, &format!("primitive/int/{name}.atom"));

    assert_eq!(
        return_value
            .map(|r| r.map(|value| value.as_bigint().as_i64()))
            .map_err(|e| e.to_string()),
        expected.map(Some).map_err(ToOwned::to_owned)
    );
}

#[test_case("basic", Ok(0.42); "basic float")]
#[test_case("min", Ok(-1.7976931348623157E+308f64); "minimum float")]
#[test_case("max", Ok(1.7976931348623157E+308f64); "maximum float")]
#[test_case("double-dot", Err("ParseError: unexpected token '.', expected: ')'"); "double dot not allowed")]
#[cfg_attr(miri, ignore)]
fn float(name: &str, expected: Result<f64, &'static str>) {
    let mut gc = Gc::default();
    let ctx = GlobalContext::default();
    let return_value = common::run(&mut gc, ctx, &format!("primitive/float/{name}.atom"));

    assert_eq!(
        return_value
            .map(|r| r.map(|r| r.as_float()))
            .map_err(|e| e.to_string()),
        expected.map(Some).map_err(ToOwned::to_owned)
    );
}
