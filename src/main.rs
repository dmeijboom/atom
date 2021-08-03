use ::std::collections::HashMap;
use ::std::fs;
use ::std::path::PathBuf;

use clap::Clap;
use peg::error::ParseError;
use peg::str::LineCol;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::ast::{ClassDeclStmt, Expr, ExprStmt, FnDeclStmt, ModuleStmt, ReturnStmt, Stmt};
use crate::compiler::{Code, CompileError, Compiler, LocalId, IR};
use crate::runtime::{RuntimeError, Value};
use crate::utils::{parse_line_column, Error};
use crate::vm::{ClassDesc, FuncDesc, Module, VM};

mod ast;
mod compiler;
mod parser;
mod runtime;
mod std;
mod tests;
mod utils;
mod vm;

#[derive(Clap)]
struct Opts {
    #[clap(long)]
    module_path: Vec<PathBuf>,
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Clap)]
enum Cmd {
    Run(RunOpts),
    Repl,
}

#[derive(Clap)]
struct RunOpts {
    filename: PathBuf,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    show_ir: bool,
}

fn run(module_paths: &Vec<PathBuf>, opts: RunOpts, contents: &str) -> Result<(), Error> {
    let tree = parser::parse(contents)?;

    if opts.show_ast {
        println!("{:#?}", tree);
    }

    let compiler = Compiler::new(tree);
    let compiled_module = compiler.compile()?;

    if opts.show_ir {
        println!("Classes:");
        println!("{:#?}", compiled_module.classes);

        println!("\nFunctions:");
        println!("{:#?}", compiled_module.funcs);
    }

    let module = Module::new(compiled_module, Some(opts.filename));
    let mut vm = VM::new()?;

    vm.register_module(module)?;

    for module_path in module_paths {
        vm.add_module_path(module_path);
    }

    vm.eval(
        "main",
        vec![
            IR::new(Code::Load(LocalId::new("main".to_string())), 0..0),
            IR::new(Code::Call(0), 0..0),
        ],
    )?;

    Ok(())
}

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
                    pos: expr.pos().clone(),
                    expr,
                })])
            } else {
                Err(e)
            }
        }
    }?;
    let stmt = tree.pop();

    if tree.len() > 0 {
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
            module.func_map.insert(name.clone(), func.clone());
        }

        for (name, class) in self.classes.iter() {
            module.class_map.insert(name.clone(), class.clone());
        }
    }

    fn create_vm(&self) -> Result<VM, RuntimeError> {
        let mut vm = VM::new()?;

        for module_path in self.module_paths.iter() {
            vm.add_module_path(module_path);
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

    fn eval(&self, expr: Expr) -> Result<Option<Value>, Error> {
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
                IR::new(Code::Load(LocalId::new("main".to_string())), 0..0),
                IR::new(Code::Call(0), 0..0),
            ],
        )?;

        Ok(vm.result())
    }
}

fn handle_input(engine: &mut AtomEngine, line: &str) -> Result<(), Error> {
    match parse_action(line)? {
        Action::Evaluate(expr) => {
            if let Some(result) = engine.eval(expr)? {
                println!("({}) {}", result.get_type().name(), result);
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
            if let Some(result) = engine.eval(expr)? {
                engine.vars.insert(name, result);

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

fn repl(module_paths: Vec<PathBuf>) -> Result<(), Error> {
    let mut rl = Editor::<()>::new();
    let mut engine = AtomEngine::new(module_paths);

    loop {
        let readline = rl.readline("$ ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                if let Err(e) = handle_input(&mut engine, &line) {
                    handle_error(&line, e);
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) | Err(_) => break,
        }
    }

    Ok(())
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let source = fs::read_to_string(&run_opts.filename).expect("failed to read file");

            if let Err(e) = run(&opts.module_path, run_opts, &source) {
                handle_error(&source, e);
            }
        }
        Cmd::Repl => {
            if let Err(e) = repl(opts.module_path) {
                handle_error("", e);
            }
        }
    }
}

fn handle_parse_error(e: ParseError<LineCol>) {
    let common_errors = vec![
        (")", "Did you forget the closing parenthesis ')'?"),
        ("}", "Did you forget to end the block with '}'?"),
        (";", "Did you forget to end the statement with ';'?"),
    ];

    eprintln!(
        "ParseError: {} on line {} at column {}",
        if e.expected.tokens().collect::<Vec<_>>().len() <= 3 {
            format!(
                "expected one of: {}",
                e.expected
                    .tokens()
                    .map(|token| match &token[1..token.len() - 1] {
                        "!=" => "not equals",
                        "==" => "equals",
                        ">=" => "greater than or equal",
                        ">" => "greater than",
                        "<" => "less than",
                        "<=" => "less than or equal",
                        "." => "dot",
                        "||" => "or",
                        "&&" => "and",
                        "+" => "addition",
                        "-" => "subtraction",
                        "/" => "division",
                        "*" => "multiplication",
                        "|" => "bit or",
                        "&" => "bit and",
                        "'0' ..= '9'" => "number",
                        "(" => "parenthesis open",
                        ")" => "parenthesis close",
                        "[" => "square bracket open",
                        "]" => "square bracket close",
                        "," => "argument separator",
                        ".." => "range",
                        _ => token,
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        } else {
            "unexpected character".to_string()
        },
        e.location.line,
        e.location.column
    );

    for (search, message) in common_errors {
        if e.expected
            .tokens()
            .any(|token| &token[1..token.len() - 1] == search)
        {
            eprintln!("  | {}", message);
        }
    }
}

fn handle_compile_error(e: CompileError) {
    eprintln!("CompileError: {}", e);
}

fn handle_runtime_error(main_module: &str, contents: &str, e: RuntimeError) {
    let mut message = String::new();

    if !e.stack_trace.is_empty() {
        message.push_str("Stack trace:\n");
    }

    for trace in e.stack_trace {
        if trace.func.module != main_module {
            message.push_str(&format!(
                "  > in {}.{}(..) at {}..{}\n",
                trace.func.module, trace.func.name, trace.pos.start, trace.pos.end,
            ));

            continue;
        }

        let (line, column) = parse_line_column(&contents, &trace.pos);

        message.push_str(&format!(
            "  > in {}.{}(..) on line {} at column {}\n",
            trace.func.module, trace.func.name, line, column,
        ));
    }

    message.push_str("RuntimeError: ");
    message.push_str(&e.message);

    if let Some(pos) = e.pos {
        let (line, column) = parse_line_column(&contents, &pos);

        message.push_str(&format!(" on line {} at column {}", line, column));
    }

    eprintln!("{}", message);
}

fn handle_error(source: &str, e: Error) {
    match e {
        Error::Compile(e) => handle_compile_error(e),
        Error::Runtime(e) => handle_runtime_error("main", &source, e),
        Error::ParseError(e) => handle_parse_error(e),
    }
}
