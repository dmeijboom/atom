use std::{cell::RefCell, fs, rc::Rc};

use atom::{
    ast::Stmt, runtime, BoxedFn, Compiler, DynamicLinker, Error, Lexer, Module, Parser, Value, Vm,
};

const PRELUDE_SOURCE: &str = include_str!("../../std/prelude.atom");

struct TestLinker<L: DynamicLinker> {
    linker: L,
    return_value: Rc<RefCell<Option<Value>>>,
}

impl<L: DynamicLinker> DynamicLinker for TestLinker<L> {
    fn resolve(&self, name: &str) -> Option<BoxedFn> {
        if name == "assert" {
            let return_value = Rc::clone(&self.return_value);

            Some(Rc::new(Box::new(move |_ctx, args| {
                let value = args.get(0).copied();
                *return_value.borrow_mut() = Some(value.unwrap_or(Value::NIL));
                Ok(Value::NIL)
            })))
        } else {
            self.linker.resolve(name)
        }
    }
}

impl<L: DynamicLinker> TestLinker<L> {
    fn new(linker: L, return_value: Rc<RefCell<Option<Value>>>) -> Self {
        Self {
            linker,
            return_value,
        }
    }
}

fn parse(source: &str) -> Result<Vec<Stmt>, Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;
    let parser = Parser::new(tokens);

    Ok(parser.parse()?)
}

pub fn compile(name: &str) -> Result<Module, Error> {
    let mut program = parse(&[PRELUDE_SOURCE, "\nextern fn assert(value);"].concat())?;
    let source = fs::read_to_string(format!("tests/source/{name}"))?;
    program.extend(parse(&source)?);
    let compiler = Compiler::default();

    Ok(compiler.compile(program)?)
}

//pub fn must_compile(name: &str) -> Module {
//    compile(name).expect("failed to compile module")
//}

pub fn run(name: &str) -> Result<Option<Value>, Error> {
    let module = compile(name)?;
    let return_value = Rc::new(RefCell::new(None));
    let linker = TestLinker::new(runtime::linker(), Rc::clone(&return_value));
    let mut vm = Vm::new(module, linker)?;
    vm.run()?;

    let return_value = return_value.borrow_mut().take();
    Ok(return_value)
}

pub fn must_run(name: &str) -> Option<Value> {
    run(name).expect("failed to run module")
}
