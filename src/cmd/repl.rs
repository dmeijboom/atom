use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::ast::{ClassDeclStmt, Expr, ExprStmt, FnDeclStmt, ModuleStmt, ReturnStmt, Stmt};
use crate::compiler::{Code, Compiler, IR};
use crate::parser;
use crate::runtime::{RuntimeError, Value};
use crate::utils::{display_error, Error};
use crate::vm::{ClassDesc, FuncDesc, FuncSource, MethodDesc, Module, VM};

enum Action {
    Evaluate(Expr),
    Import(String),
    SetVariable((String, Expr)),
    CreateClass(ClassDeclStmt),
    CreateFn(FnDeclStmt),
}

fn parse_action(input: &str) -> Result<Action, Error> {
    let mut tree = match parser::parse(input) {
        Ok(tree) => Ok(tree),
        Err(e) => {
            // Maybe an expression?
            if let Ok(expr) = parser::parse_expr(input) {
                Ok(vec![Stmt::Expr(ExprStmt {
                    pos: expr.pos(),
                    expr,
                })])
            } else {
                Err(e)
            }
        }
    }?;
    let stmt = tree.pop();

    if !tree.is_empty() {
        return Err(
            RuntimeError::new("unable to parse more than one statement".to_string()).into(),
        );
    }

    match stmt {
        Some(Stmt::Let(let_stmt)) => {
            Ok(Action::SetVariable((let_stmt.name.clone(), let_stmt.value)))
        }
        Some(Stmt::Expr(expr_stmt)) => Ok(Action::Evaluate(expr_stmt.expr)),
        Some(Stmt::Import(import_stmt)) => Ok(Action::Import(import_stmt.name)),
        Some(Stmt::ClassDecl(class_decl)) => Ok(Action::CreateClass(class_decl)),
        Some(Stmt::FnDecl(fn_decl)) => Ok(Action::CreateFn(fn_decl)),
        _ => Err(RuntimeError::new("invalid statement".to_string()).into()),
    }
}

struct AtomEngine {
    imports: Vec<String>,
    module_paths: Vec<PathBuf>,
    vars: HashMap<String, Value>,
    classes: HashMap<String, ClassDesc>,
    functions: HashMap<String, FuncDesc>,
}

impl AtomEngine {
    fn new(module_paths: Vec<PathBuf>) -> Self {
        Self {
            imports: vec![],
            module_paths,
            vars: HashMap::new(),
            classes: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn setup(&self, module: &mut Module) {
        for (key, value) in self.vars.iter() {
            module.globals.insert(key.clone(), value.clone());
        }

        for (name, func) in self.functions.iter() {
            module.func_map.insert(
                name.clone(),
                FuncDesc {
                    public: func.public,
                    source: match &func.source {
                        FuncSource::Native(instructions) => {
                            FuncSource::Native(Rc::clone(instructions))
                        }
                        FuncSource::External(external_fn) => FuncSource::External(*external_fn),
                    },
                    args: func.args.clone(),
                    pos: func.pos.clone(),
                },
            );
        }

        for (name, class) in self.classes.iter() {
            module.class_map.insert(
                name.clone(),
                ClassDesc {
                    public: class.public,
                    methods: class
                        .methods
                        .iter()
                        .map(|(key, method)| {
                            (
                                key.clone(),
                                MethodDesc {
                                    func: FuncDesc {
                                        pos: method.func.pos.clone(),
                                        public: method.func.public,
                                        source: match &method.func.source {
                                            FuncSource::Native(instructions) => {
                                                FuncSource::Native(Rc::clone(instructions))
                                            }
                                            FuncSource::External(external_fn) => {
                                                FuncSource::External(*external_fn)
                                            }
                                        },
                                        args: method.func.args.clone(),
                                    },
                                    class_name: name.clone(),
                                },
                            )
                        })
                        .collect(),
                    fields: class.fields.clone(),
                },
            );
        }
    }

    fn create_vm(&self) -> Result<VM, RuntimeError> {
        let mut vm = VM::new()?;

        for module_path in self.module_paths.iter() {
            vm.add_module_lookup_path(module_path);
        }

        Ok(vm)
    }

    fn create_module(&self, stmt: Stmt) -> Result<Module, Error> {
        let compiler = Compiler::new(vec![stmt]);
        let compiled_module = compiler.compile()?;

        let mut module = Module::new(compiled_module, Some("stdin".into()));

        self.setup(&mut module);

        Ok(module)
    }

    fn eval(&self, expr: Expr) -> Result<VM, Error> {
        let module = self.create_module(Stmt::FnDecl(FnDeclStmt {
            name: "main".to_string(),
            public: false,
            args: vec![],
            pos: expr.pos(),
            body: vec![Stmt::Return(ReturnStmt {
                pos: expr.pos(),
                expr,
            })],
        }))?;

        let mut vm = self.create_vm()?;

        vm.register_module(module)?;
        vm.eval(
            "main",
            vec![
                IR::new(Code::LoadName("main".to_string()), 0..0),
                IR::new(Code::Call(0), 0..0),
            ],
        )?;

        Ok(vm)
    }
}

fn handle_input(engine: &mut AtomEngine, line: &str) -> Result<(), Error> {
    match parse_action(line)? {
        Action::Evaluate(expr) => {
            let mut vm = engine.eval(expr)?;

            if let Some(value) = vm.result() {
                println!("{}", vm.fmt_value(&value));
            } else {
                println!("nil");
            }

            Ok(())
        }
        Action::Import(name) => {
            let mut module = engine.create_module(Stmt::Module(ModuleStmt {
                name: "main".to_string(),
                pos: 0..0,
            }))?;

            module.imports.push(name.clone());

            let mut vm = engine.create_vm()?;

            vm.register_module(module)?;

            engine.imports.push(name);

            Ok(())
        }
        Action::SetVariable((name, expr)) => {
            let mut vm = engine.eval(expr)?;

            if let Some(value) = vm.result() {
                engine.vars.insert(name, value);

                return Ok(());
            }

            Err(
                RuntimeError::new(format!("unable to set variable '{}' without value", name))
                    .into(),
            )
        }
        Action::CreateClass(class_decl_stmt) => {
            let class_name = class_decl_stmt.name.clone();
            let mut module = engine.create_module(Stmt::ClassDecl(class_decl_stmt))?;

            engine.classes.insert(
                class_name.clone(),
                module.class_map.remove(&class_name).unwrap(),
            );

            Ok(())
        }
        Action::CreateFn(fn_decl_stmt) => {
            let function_name = fn_decl_stmt.name.clone();
            let mut module = engine.create_module(Stmt::FnDecl(fn_decl_stmt))?;

            engine.functions.insert(
                function_name.clone(),
                module.func_map.remove(&function_name).unwrap(),
            );

            Ok(())
        }
    }
}

pub fn command(module_paths: Vec<PathBuf>) -> Result<(), Error> {
    let mut rl = Editor::<()>::new();
    let mut engine = AtomEngine::new(module_paths);

    loop {
        let readline = rl.readline("$ ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                if let Err(e) = handle_input(&mut engine, &line) {
                    display_error(&line, e);
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) | Err(_) => break,
        }
    }

    Ok(())
}
