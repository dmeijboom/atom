use ::std::fs;
use ::std::path::PathBuf;

use clap::Clap;
use peg::error::ParseError;
use peg::str::LineCol;

use crate::compiler::{Code, CompileError, Compiler, LocalId, IR};
use crate::runtime::RuntimeError;
use crate::utils::parse_line_column;
use crate::vm::{Module, VM};

mod ast;
mod compiler;
mod parser;
mod runtime;
mod std;
mod utils;
mod vm;

#[derive(Clap)]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Clap)]
enum Cmd {
    Run(RunOpts),
}

#[derive(Clap)]
struct RunOpts {
    filename: PathBuf,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    show_ir: bool,
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let contents = fs::read_to_string(&run_opts.filename).expect("unable to read file");
            let tree = match parser::parse(&contents) {
                Ok(tree) => tree,
                Err(e) => {
                    handle_parse_error(e);

                    return;
                }
            };

            if run_opts.show_ast {
                println!("{:#?}", tree);
            }

            let compiler = Compiler::new(tree);
            let module = match compiler.compile() {
                Ok(module) => module,
                Err(e) => {
                    handle_compile_error(e);

                    return;
                }
            };

            if run_opts.show_ir {
                println!("Classes:");
                println!("{:#?}", module.classes);

                println!("\nFunctions:");
                println!("{:#?}", module.funcs);
            }

            let main_module = Module::new(module, Some(run_opts.filename));
            let main_module_name = main_module.name.clone();

            let mut vm = VM::new().expect("failed to initialize VM");

            vm.register_module(main_module)
                .expect("failed to register main module");

            if let Err(e) = vm.eval(
                "main",
                vec![
                    IR::new(Code::Load(LocalId::new("main".to_string())), 0..0),
                    IR::new(Code::CallWithKeywords((vec![], 0)), 0..0),
                ],
            ) {
                handle_runtime_error(&main_module_name, &contents, e);
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
