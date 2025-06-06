#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use std::{fs, sync::mpsc::Sender};

    use atom::{
        ast::Stmt, runtime::Runtime, Compiler, Error, Ffi, Gc, Lexer, Module, Parser, Trace, Value,
        Vm, VmError,
    };

    const PRELUDE_SOURCE: &str = include_str!("../../std/prelude.atom");

    pub struct TestRuntime<F: Ffi> {
        fallback: F,
        sender: Sender<Value>,
    }

    impl<L: Ffi> Ffi for TestRuntime<L> {
        fn call(&mut self, name: &str, gc: &mut Gc, args: Vec<Value>) -> Result<Value, VmError> {
            if name == "ret" {
                gc.disable();
                let _ = self.sender.send(args[0]);
                Ok(Value::NIL)
            } else {
                self.fallback.call(name, gc, args)
            }
        }
    }

    impl<F: Ffi> TestRuntime<F> {
        fn new(fallback: F, sender: Sender<Value>) -> Self {
            Self { fallback, sender }
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
        let mut program = _parse(&[PRELUDE_SOURCE, "\nextern fn ret(value);"].concat())?;
        let source = fs::read_to_string(format!("tests/source/{name}"))?;
        program.extend(_parse(&source)?);
        let compiler = Compiler::default();

        Ok(compiler.compile(program)?)
    }

    pub type TestVm = Vm<TestRuntime<Runtime>, 1000, 1000>;

    pub fn run(name: &str) -> Result<Option<Value>, Error> {
        let module = compile(name)?;
        let (sender, recv) = std::sync::mpsc::channel();
        let runtime = TestRuntime::new(Runtime::default(), sender);
        let mut vm = TestVm::with(module, runtime)?;
        vm.run()?;

        Ok(recv.recv().ok())
    }
}

#[cfg(test)]
pub use tests::*;
