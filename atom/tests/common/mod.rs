#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use std::{fs, sync::mpsc::Sender};

    use atom::{
        ast::Stmt,
        builtins::BuiltinFunction,
        compiler::{Compiler, Context, Package},
        error::Error,
        gc::Gc,
        lexer::Lexer,
        parser::Parser,
        runtime::{error::RuntimeError, value::Value, Runtime},
        vm::{Builtins, Vm},
    };

    pub struct TestRuntime<'gc> {
        sender: Sender<Value<'gc>>,
    }

    impl<'gc> TestRuntime<'gc> {
        fn new(sender: Sender<Value<'gc>>) -> Self {
            Self { sender }
        }

        fn ret(&mut self, gc: &mut Gc<'gc>, arg: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
            gc.disable();
            let _ = self.sender.send(Value::clone(&arg));
            Ok(Value::default())
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

    pub fn compile(ctx: &mut Context, name: &str) -> Result<Package, Error> {
        let source = fs::read_to_string(format!("tests/source/{name}"))?;
        let program = _parse(&source)?;
        let compiler = Compiler::default();

        Ok(compiler.compile(ctx, program)?)
    }

    pub type TestVm<'gc> = Vm<'gc, 1000>;

    struct RetBuiltin {
        sender: Sender<u64>,
    }

    impl<'a> BuiltinFunction for RetBuiltin {
        fn call<'gc>(
            &mut self,
            gc: &mut Gc<'gc>,
            _rt: &dyn Runtime,
            mut args: Vec<Value<'gc>>,
        ) -> atom::runtime::Result<Value<'gc>> {
            gc.disable();
            let _ = self.sender.send(args.remove(0).into_bits());
            Ok(Value::default())
        }
    }

    pub fn run<'gc>(
        gc: &mut Gc<'gc>,
        mut ctx: Context,
        name: &str,
    ) -> Result<Option<Value<'gc>>, Error> {
        let module = compile(&mut ctx, name)?;
        let (sender, recv) = std::sync::mpsc::channel();

        let mut builtins = Builtins::default();
        builtins.register("ret", Box::new(RetBuiltin { sender }));

        let mut vm = TestVm::new(gc, ctx, "".into(), module)?;
        vm.run(gc, &mut builtins)?;

        Ok(recv.recv().ok().map(Value::from_bits))
    }
}

#[cfg(test)]
pub use tests::*;
