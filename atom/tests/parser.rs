use std::{env, fs};

use ron::ser::PrettyConfig;
use test_case::test_case;

mod common;

#[test_case("literal/bool"; "bool literal")]
#[test_case("literal/int"; "int literal")]
#[test_case("literal/float"; "float literal")]
#[test_case("literal/nil"; "nil literal")]
#[test_case("literal/string"; "string literal")]
#[test_case("assign/simple"; "simple assignment")]
#[test_case("assign/add"; "addition assignment")]
#[test_case("assign/sub"; "subtract assignment")]
#[test_case("assign/mul"; "multiply assignment")]
#[test_case("assign/div"; "division assignment")]
#[test_case("binary/add"; "add")]
#[test_case("binary/sub"; "subtract")]
#[test_case("binary/div"; "division")]
#[test_case("binary/rem"; "remainder")]
#[test_case("binary/mul"; "muliply")]
#[test_case("binary/bitand"; "bitwise and")]
#[test_case("binary/bitor"; "bitwise or")]
#[test_case("binary/bitxor"; "bitwise xor")]
#[test_case("binary/shiftleft"; "bitwise shift left")]
#[test_case("binary/shiftright"; "bitwise shift right")]
#[test_case("binary/lt"; "less than")]
#[test_case("binary/lte"; "less than or equal")]
#[test_case("binary/gt"; "greater than")]
#[test_case("binary/gte"; "greater than or equal")]
#[test_case("binary/eq"; "equal")]
#[test_case("binary/ne"; "not equal")]
#[test_case("binary/or"; "or")]
#[test_case("binary/and"; "and")]
#[test_case("binary/complex1"; "complex binary operation 1")]
#[test_case("binary/complex2"; "complex binary operation 2")]
#[test_case("ident"; "identifier")]
#[test_case("array"; "array")]
#[test_case("unary"; "not")]
#[test_case("member/name"; "member on ident")]
#[test_case("member/expr"; "member on expr")]
#[test_case("call/no_args"; "call without args")]
#[test_case("call/single"; "call with a single arg")]
#[test_case("call/multiple"; "call with multiple args")]
#[test_case("call/recursive"; "recursive call")]
#[test_case("comp_member/name"; "computed member on ident")]
#[test_case("comp_member/expr"; "computed member on expr")]
#[test_case("range/all"; "range")]
#[test_case("range/from"; "range from")]
#[test_case("range/to"; "range to")]
#[test_case("range/full"; "full range")]
fn expr(name: &str) {
    let config = PrettyConfig::default().struct_names(true);
    let actual = common::parse(&format!("parser/expr/{name}.atom")).unwrap();
    let actual_string = ron::ser::to_string_pretty(&actual, config.clone()).unwrap();
    let dest = format!("tests/results/parser/expr/{name}.ron");

    if env::var("GENERATE_RESULT").is_ok() {
        fs::write(&dest, &actual_string).unwrap();
    }

    let expected = fs::read_to_string(&dest).unwrap();

    assert_eq!(actual_string, expected);
}
