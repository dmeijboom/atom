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

    #[test_case(include_str!("../examples/simple_return.atom"), Value::Int(10); "simple return")]
    #[test_case(include_str!("../examples/if_else.atom"), Value::Int(25); "if else")]
    #[test_case(include_str!("../examples/loops.atom"), Value::Int(75); "loops")]
    #[test_case(include_str!("../examples/operator_precedence.atom"), Value::Int(17); "operator precedence")]
    #[test_case(include_str!("../examples/class_fields.atom"), Value::String("hello world".to_string()); "class fields")]
    #[test_case(include_str!("../examples/class_methods.atom"), Value::Int(100); "class methods")]
    #[test_case(include_str!("../examples/stack_copy.atom"), Value::Array(vec![Value::Int(20), Value::Int(20), Value::Int(20)]); "stack copy")]
    fn code(source: &str, value: Value) {
        let result = run_code(source);

        assert_eq!(result, Ok(Some(value)));
    }
}
