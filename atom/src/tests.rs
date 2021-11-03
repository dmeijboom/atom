#[cfg(test)]
mod tests {
    use test_case::test_case;

    use atom_ir::{Code, IR};
    use atom_runtime::{AtomRef, Int, RuntimeError, Value};

    use crate::utils::{parse_and_compile, Error};
    use crate::vm::VM;

    fn run_code(source: &str) -> Result<Option<Value>, Error> {
        let modules = parse_and_compile(
            source,
            vec!["./src/std/atom".to_string(), "./examples".to_string()],
        )?;
        let module = modules
            .iter()
            .find(|module| module.name == "main")
            .ok_or_else(|| {
                Error::Runtime(RuntimeError::new("main module not found".to_string()))
            })?;

        let mut vm = VM::new()?;

        if let Some(id) = module.funcs.get_index_of("main") {
            for module in modules {
                vm.register_module(module)?;
            }

            vm.eval(
                "main",
                IR::with_codes(vec![Code::LoadFn(id), Code::Call(0)]),
            )?;

            return Ok(vm.result());
        }

        Err(Error::Runtime(RuntimeError::new(
            "function 'main' was not found in the module".to_string(),
        )))
    }

    #[test_case(include_str!("../examples/int_ops.atom"), Value::Array(AtomRef::new(vec![
        Value::Int(Int::Uint64(11562461410742939137)),
        Value::Int(Int::Uint64(11562461410679940144)),
        Value::Bool(false),
    ])); "integer operations")]
    #[test_case(include_str!("../examples/calls.atom"), Value::Int(Int::Int32(0)); "calls")]
    #[test_case(include_str!("../examples/loops.atom"), Value::Int(Int::Int32(75)); "loops")]
    #[test_case(include_str!("../examples/casts.atom"), Value::Int(Int::Uint64(34)); "casts")]
    #[test_case(include_str!("../examples/references.atom"), Value::Bool(true); "references")]
    #[test_case(include_str!("../examples/if_else.atom"), Value::Int(Int::Int32(25)); "if else")]
    #[test_case(include_str!("../examples/call_arg_order.atom"), Value::Bool(true); "call arg order")]
    #[test_case(include_str!("../examples/range_iter.atom"), Value::Int(Int::Int32(100)); "range iter")]
    #[test_case(include_str!("../examples/init_class.atom"), Value::Int(Int::Int32(200)); "init class")]
    #[test_case(include_str!("../examples/simple_return.atom"), Value::Int(Int::Int32(10)); "simple return")]
    #[test_case(include_str!("../examples/class_methods.atom"), Value::Int(Int::Int32(100)); "class methods")]
    #[test_case(include_str!("../examples/wrap_reference_type.atom"), Value::Bool(true); "wrap reference type")]
    #[test_case(include_str!("../examples/type_assertions.atom"), Value::Int(Int::Int32(7)); "type assertions")]
    #[test_case(include_str!("../examples/int_overflow.atom"), Value::Int(Int::Int32(100000)); "integer overflow")]
    #[test_case(include_str!("../examples/operator_precedence.atom"), Value::Int(Int::Int32(17)); "operator precedence")]
    #[test_case(include_str!("../examples/self_referencing_closure.atom"), Value::Bool(true); "self referencing closure")]
    #[test_case(include_str!("../examples/locals.atom"), Value::String(AtomRef::new("item3item6".to_string())); "locals")]
    #[test_case(include_str!("../examples/map_value_arithmetic.atom"), Value::Int(Int::Int32(20)); "map value arithmetic")]
    #[test_case(include_str!("../examples/map_basics.atom"), Value::String(AtomRef::new("atom".to_string())); "map basics")]
    #[test_case(include_str!("../examples/closures.atom"), Value::String(AtomRef::new("hello world!".to_string())); "closures")]
    #[test_case(include_str!("../examples/store_var_in_loop.atom"), Value::Int(Int::Int32(28)); "store variable in infinite loop")]
    #[test_case(include_str!("../examples/class_fields.atom"), Value::String(AtomRef::new("hello world".to_string())); "class fields")]
    #[test_case(include_str!("../examples/use_class_fn_before_init.atom"), Value::Bool(true); "use classes/functions before initialization")]
    #[test_case(include_str!("../examples/template_string.atom"), Value::String(AtomRef::new("Hello { World 120".to_string())); "template string")]
    #[test_case(include_str!("../examples/heap_copy.atom"), Value::Array(AtomRef::new(vec![Value::Int(Int::Int32(20)), Value::Int(Int::Int32(30)), Value::Int(Int::Int32(40))])); "heap copy")]
    fn code_success(source: &str, value: Value) {
        let result = run_code(source);

        assert_eq!(result, Ok(Some(value)));
    }

    #[test_case(include_str!("../examples/invalid/call_int.atom"), "type 'Int' is not callable"; "function call on an integer")]
    #[test_case(include_str!("../examples/invalid/duplicate_class.atom"), "unable to redefine class: Test"; "declaring a class more than once")]
    #[test_case(include_str!("../examples/invalid/fn_signature.atom"), "invalid argument count for target: main.test(...) (expected 3, not 2)"; "invalid Fn signature")]
    #[test_case(include_str!("../examples/invalid/method_not_found.atom"), "no such field or method 'test_example' for: std.map.Map"; "method not found on a map type")]
    #[test_case(include_str!("../examples/invalid/init_class_missing_fields.atom"), "unable to create instance of 'main.Test' without field: three"; "initialize class with missing fields")]
    #[test_case(include_str!("../examples/invalid/private_class.atom"), "failed to import unknown name 'exampleClass' from: invalid.private_module"; "import and use a private class")]
    #[test_case(include_str!("../examples/invalid/init_class_with_args.atom"), "unable to create instance of 'main.Test' with non-keyword arguments"; "initialize class with non-keyword arguments")]
    fn code_fail(source: &str, message: &str) {
        let result = run_code(source);

        assert_eq!(
            Some(message.to_string()),
            result.err().map(|e| match e {
                Error::Compile(e) => e.message,
                Error::Runtime(e) => e.message,
                Error::ParseError(_) => "unexpected parse error".to_string(),
            })
        );
    }
}
