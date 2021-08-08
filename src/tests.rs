#[cfg(test)]
mod tests {
    use test_case::test_case;

    use crate::compiler::{Code, LocalId, IR};
    use crate::runtime::Value;
    use crate::utils::{parse_and_compile, Error};
    use crate::vm::VM;

    fn run_code(source: &str) -> Result<Option<Value>, Error> {
        let module = parse_and_compile(source, None)?;
        let mut vm = VM::new()?;

        vm.register_module(module)?;
        vm.eval(
            "main",
            vec![
                IR::new(Code::Load(LocalId::new("main".to_string())), 0..0),
                IR::new(Code::Call(0), 0..0),
            ],
        )?;

        Ok(vm.result())
    }

    #[test_case(include_str!("../examples/loops.atom"), Value::Int(75); "loops")]
    #[test_case(include_str!("../examples/casts.atom"), Value::Int(34); "casts")]
    #[test_case(include_str!("../examples/if_else.atom"), Value::Int(25); "if else")]
    #[test_case(include_str!("../examples/simple_return.atom"), Value::Int(10); "simple return")]
    #[test_case(include_str!("../examples/class_methods.atom"), Value::Int(100); "class methods")]
    #[test_case(include_str!("../examples/operator_precedence.atom"), Value::Int(17); "operator precedence")]
    #[test_case(include_str!("../examples/class_fields.atom"), Value::String("hello world".to_string()); "class fields")]
    #[test_case(include_str!("../examples/stack_copy.atom"), Value::Array(vec![Value::Int(20), Value::Int(20), Value::Int(20)]); "stack copy")]
    fn code_success(source: &str, value: Value) {
        let result = run_code(source);

        assert_eq!(result, Ok(Some(value)));
    }

    #[test_case(include_str!("../examples/invalid/index_out_of_bounds.atom"), "index out of bounds: 3"; "index out of bounds")]
    #[test_case(include_str!("../examples/invalid/call_int.atom"), "type 'Int' is not callable"; "function call on an integer")]
    #[test_case(include_str!("../examples/invalid/fn_signature.atom"), "invalid argument count for Fn: main.test(...) (expected 3, not 2)"; "invalid Fn signature")]
    #[test_case(include_str!("../examples/invalid/init_class_with_args.atom"), "unable to initialize main.Test with non-keyword arguments"; "initialize class with non-keyword arguments")]
    #[test_case(include_str!("../examples/invalid/init_class_missing_fields.atom"), "unable to initialize main.Test with missing fields: three"; "initialize class with missing fields")]
    fn code_fail(source: &str, message: &str) {
        let result = run_code(source);

        assert_eq!(
            Some(message.to_string()),
            result.err().and_then(|e| match e {
                Error::Compile(e) => Some(e.message),
                Error::Runtime(e) => Some(e.message),
                Error::ParseError(_) => Some("unexpected parse error".to_string()),
            })
        );
    }
}
