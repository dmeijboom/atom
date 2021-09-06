#[cfg(test)]
mod tests {
    use test_case::test_case;

    use atom_ir::{Code, Location, IR};
    use atom_runtime::{AtomRef, RuntimeError, Value};

    use crate::utils::{parse_and_compile, Error};
    use crate::vm::VM;

    fn run_code(source: &str) -> Result<Option<Value>, Error> {
        let module = parse_and_compile(
            source,
            vec!["./src/std/atom".to_string(), "./examples".to_string()],
        )?;
        let mut vm = VM::new()?;

        if let Some(id) = module.funcs.iter().position(|func| func.name == "main") {
            vm.register_module(module, None)?;
            vm.eval(
                "main",
                vec![
                    IR::new(Code::LoadFn(id), Location::default()),
                    IR::new(Code::Call(0), Location::default()),
                ],
            )?;

            return Ok(vm.result());
        }

        Err(Error::Runtime(RuntimeError::new(
            "function 'main' was not found in the module".to_string(),
        )))
    }

    #[test_case(include_str!("../examples/calls.atom"), Value::Int(0); "calls")]
    #[test_case(include_str!("../examples/loops.atom"), Value::Int(75); "loops")]
    #[test_case(include_str!("../examples/casts.atom"), Value::Int(34); "casts")]
    #[test_case(include_str!("../examples/if_else.atom"), Value::Int(25); "if else")]
    #[test_case(include_str!("../examples/range_iter.atom"), Value::Int(100); "range iter")]
    #[test_case(include_str!("../examples/init_class.atom"), Value::Int(200); "init class")]
    #[test_case(include_str!("../examples/references.atom"), Value::Bool(true); "references")]
    #[test_case(include_str!("../examples/simple_return.atom"), Value::Int(10); "simple return")]
    #[test_case(include_str!("../examples/class_methods.atom"), Value::Int(100); "class methods")]
    #[test_case(include_str!("../examples/type_assertions.atom"), Value::Int(7); "type assertions")]
    #[test_case(include_str!("../examples/call_arg_order.atom"), Value::Bool(true); "call arg order")]
    #[test_case(include_str!("../examples/operator_precedence.atom"), Value::Int(17); "operator precedence")]
    #[test_case(include_str!("../examples/map_value_arithmetic.atom"), Value::Int(20); "map value arithmetic")]
    #[test_case(include_str!("../examples/wrap_reference_type.atom"), Value::Bool(true); "wrap reference type")]
    #[test_case(include_str!("../examples/store_var_in_loop.atom"), Value::Int(28); "store variable in infinite loop")]
    #[test_case(include_str!("../examples/self_referencing_closure.atom"), Value::Bool(true); "self referencing closure")]
    #[test_case(include_str!("../examples/locals.atom"), Value::String(AtomRef::new("item3item6".to_string())); "locals")]
    #[test_case(include_str!("../examples/map_basics.atom"), Value::String(AtomRef::new("atom".to_string())); "map basics")]
    #[test_case(include_str!("../examples/closures.atom"), Value::String(AtomRef::new("hello world!".to_string())); "closures")]
    #[test_case(include_str!("../examples/class_fields.atom"), Value::String(AtomRef::new("hello world".to_string())); "class fields")]
    #[test_case(include_str!("../examples/use_class_fn_before_init.atom"), Value::Bool(true); "use classes/functions before initialization")]
    #[test_case(include_str!("../examples/template_string.atom"), Value::String(AtomRef::new("Hello { World 120".to_string())); "template string")]
    #[test_case(include_str!("../examples/heap_copy.atom"), Value::Array(AtomRef::new(vec![Value::Int(20), Value::Int(30), Value::Int(40)])); "heap copy")]
    fn code_success(source: &str, value: Value) {
        let result = run_code(source);

        assert_eq!(result, Ok(Some(value)));
    }

    #[test_case(include_str!("../examples/invalid/call_int.atom"), "type 'Int' is not callable"; "function call on an integer")]
    #[test_case(include_str!("../examples/invalid/duplicate_class.atom"), "unable to redefine class: Test"; "declaring a class more than once")]
    #[test_case(include_str!("../examples/invalid/private_class.atom"), "unable to import private class: exampleClass"; "import and use a private class")]
    #[test_case(include_str!("../examples/invalid/method_not_found.atom"), "no such field or method 'test_example' for: std.core.Map"; "method not found on a map type")]
    #[test_case(include_str!("../examples/invalid/fn_signature.atom"), "invalid argument count for target: main.test(...) (expected 3, not 2)"; "invalid Fn signature")]
    #[test_case(include_str!("../examples/invalid/init_class_with_args.atom"), "unable to initialize 'main.Test' with non-keyword arguments"; "initialize class with non-keyword arguments")]
    #[test_case(include_str!("../examples/invalid/init_class_missing_fields.atom"), "unable to initialize 'main.Test' without field: three"; "initialize class with missing fields")]
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
