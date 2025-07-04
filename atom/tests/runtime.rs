use atom::{
    compiler::GlobalContext,
    gc::Gc,
    runtime::{value::Type, IntoAtom, Value},
};
use test_case::test_case;

mod common;

fn equals(lhs: &Value, rhs: &Value) -> bool {
    if lhs == rhs {
        return true;
    }

    if lhs.ty() != rhs.ty() {
        return false;
    }

    match lhs.ty() {
        Type::Int => lhs.as_bigint() == rhs.as_bigint(),
        Type::Float => lhs.as_float() == rhs.as_float(),
        Type::Str => lhs.as_str() == rhs.as_str(),
        Type::Atom => lhs.as_atom() == rhs.as_atom(),
        Type::Array => lhs
            .as_array()
            .iter()
            .zip(rhs.as_array().iter())
            .all(|(l, r)| equals(l, r)),
        _ => false,
    }
}

#[test_case("call_basic", 4i64; "basic function call")]
#[test_case("call_nested", 200i64; "nested function call")]
#[test_case("call_var_fn", 30i64; "call function variable")]
#[test_case("call_var_method", 30i64; "call method variable")]
#[test_case("call_assign_arg", 30i64; "assign to argument in call")]
#[test_case("class_member", 10i64; "class member")]
#[test_case("class_init", "Hello World".to_string(); "class initialization")]
#[test_case("class_composition", 10i64; "class composition")]
#[test_case("loop", vec![0i64, 1i64, 2i64, 3i64, 4i64, 5i64]; "basic loop")]
#[test_case("loop_nested", vec![1i64, 2i64, 3i64, 2i64, 4i64, 6i64]; "nested loop")]
#[test_case("array_index", 200i64; "array index")]
#[test_case("array_slice", vec![200i64, 300i64]; "array slice")]
#[test_case("array_slice_bounds", Vec::<i64>::default(); "array slice out of bounds")]
#[test_case("array_elem_assign", vec![100i64, 400i64, 300i64]; "array assign element")]
#[cfg_attr(miri, ignore)]
fn runtime(name: &str, expected: impl for<'gc> IntoAtom<'gc>) {
    let mut gc = Gc::default();
    let ctx = GlobalContext::default();
    let actual = common::run(&mut gc, ctx, &format!("runtime/{name}.atom"));
    let actual = actual
        .expect("runtime error")
        .expect("return value not found");
    let expected = expected.into_atom(&mut gc).unwrap();

    assert!(
        equals(&actual, &expected),
        "expected: {}, got: {}",
        expected,
        actual,
    );
}
