#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use std::{fs, sync::mpsc::Sender};

    use atom::{
        ast::Stmt,
        compiler::{Compiler, Package},
        error::Error,
        gc::Gc,
        lexer::Lexer,
        parser::Parser,
        runtime::{value::Value, Runtime},
        vm::{Error as VmError, Ffi, Vm},
    };

    pub struct TestRuntime<'gc, F: Ffi<'gc>> {
        fallback: F,
        sender: Sender<Value<'gc>>,
    }

    impl<'gc, F: Ffi<'gc>> Ffi<'gc> for TestRuntime<'gc, F> {
        fn call(
            &mut self,
            name: &str,
            gc: &mut Gc<'gc>,
            args: Vec<Value<'gc>>,
        ) -> Result<Value<'gc>, VmError> {
            if name == "ret" {
                gc.disable();
                let _ = self.sender.send(Value::clone(&args[0]));
                Ok(Value::default())
            } else {
                self.fallback.call(name, gc, args)
            }
        }
    }

    impl<'gc, F: Ffi<'gc>> TestRuntime<'gc, F> {
        fn new(fallback: F, sender: Sender<Value<'gc>>) -> Self {
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

    pub fn compile(name: &str) -> Result<Package, Error> {
        let mut program = _parse("\nextern fn ret(value);")?;
        let source = fs::read_to_string(format!("tests/source/{name}"))?;
        program.extend(_parse(&source)?);
        let compiler = Compiler::default();

        Ok(compiler.compile(program)?)
    }

    pub type TestVm<'gc> = Vm<'gc, TestRuntime<'gc, Runtime>, 1000>;

    pub fn run<'gc>(gc: &mut Gc<'gc>, name: &str) -> Result<Option<Value<'gc>>, Error> {
        let module = compile(name)?;
        let (sender, recv) = std::sync::mpsc::channel();
        let runtime = TestRuntime::new(Runtime::default(), sender);
        let mut vm = TestVm::new(gc, "".into(), module, runtime)?;

        vm.run(gc)?;

        Ok(recv.recv().ok())
    }
}

#[cfg(test)]
pub use tests::*;
