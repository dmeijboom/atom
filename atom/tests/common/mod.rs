#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use std::{cell::RefCell, fs, rc::Rc};

    use atom::{
        ast::Stmt,
        runtime::{Api, Runtime},
        Compiler, Error, Lexer, Module, Parser, Value, Vm, VmError, FFI,
    };

    const PRELUDE_SOURCE: &str = include_str!("../../std/prelude.atom");

    pub struct TestRuntime<F: FFI> {
        fallback: F,
        return_value: Rc<RefCell<Option<Value>>>,
    }

    impl<L: FFI> FFI for TestRuntime<L> {
        fn call(&self, name: &str, api: Api, args: Vec<Value>) -> Result<Value, VmError> {
            if name == "assert" {
                let return_value = Rc::clone(&self.return_value);

                let value = args.get(0).copied();
                *return_value.borrow_mut() = Some(value.unwrap_or(Value::NIL));
                Ok(Value::NIL)
            } else {
                self.fallback.call(name, api, args)
            }
        }
    }

    impl<F: FFI> TestRuntime<F> {
        fn new(fallback: F, return_value: Rc<RefCell<Option<Value>>>) -> Self {
            Self {
                fallback,
                return_value,
            }
        }
    }

    fn _parse(source: &str) -> Result<Vec<Stmt>, Error> {
        let chars = source.chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&chars);
        let tokens = lexer.lex()?;
        let parser = Parser::new(tokens);

        Ok(parser.parse()?)
    }

    pub fn parse(name: &str) -> Result<Vec<Stmt>, Error> {
        let source = fs::read_to_string(format!("tests/source/{name}"))?;
        let program = _parse(&source)?;
        Ok(program)
    }

    pub fn compile(name: &str) -> Result<Module, Error> {
        let mut program = _parse(&[PRELUDE_SOURCE, "\nextern fn assert(value);"].concat())?;
        let source = fs::read_to_string(format!("tests/source/{name}"))?;
        program.extend(_parse(&source)?);
        let compiler = Compiler::default();

        Ok(compiler.compile(program)?)
    }

    pub type TestVm = Vm<TestRuntime<Runtime>, 1000, 1000>;

    pub fn run(name: &str) -> Result<(TestVm, Option<Value>), Error> {
        let module = compile(name)?;
        let return_value = Rc::new(RefCell::new(None));
        let linker = TestRuntime::new(Runtime::default(), Rc::clone(&return_value));
        let mut vm = TestVm::with(module, linker)?;
        vm.run()?;

        let return_value = return_value.borrow_mut().take();
        Ok((vm, return_value))
    }
}

#[cfg(test)]
pub use tests::*;
