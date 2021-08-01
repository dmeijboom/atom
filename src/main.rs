use ::std::fs;
use ::std::path::PathBuf;

use clap::Clap;

use crate::ast::Pos;
use crate::compiler::{Code, Compiler, LocalId, IR};
use crate::vm::{Module, VM};

mod ast;
mod compiler;
mod parser;
mod runtime;
mod std;
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

fn parse_line_column(source: &str, pos: &Pos) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;
    let mut chars = source.chars();

    for _ in 0..pos.start {
        match chars.next() {
            Some('\n') => {
                line += 1;
                column = 1;
            }
            _ => column += 1,
        }
    }

    (line, column)
}

fn setup_stdlib() -> Module {
    let contents = include_str!("std/core.atom");
    let tree = parser::parse(contents).expect("syntax error");
    let compiler = Compiler::new(tree);
    let module = compiler.compile().expect("compile error");

    Module::new(module, Some("std/core.atom".into()))
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let contents = fs::read_to_string(&run_opts.filename).expect("unable to read file");
            let tree = parser::parse(&contents).expect("syntax error");

            if run_opts.show_ast {
                println!("{:#?}", tree);
            }

            let compiler = Compiler::new(tree);
            let module = compiler.compile().expect("compile error");

            if run_opts.show_ir {
                println!("Classes:");
                println!("{:#?}", module.classes);

                println!("\nFunctions:");
                println!("{:#?}", module.funcs);
            }

            let mut stdlib = setup_stdlib();
            std::core::register(&mut stdlib).expect("std lib error");

            let main_module = Module::new(module, Some(run_opts.filename));

            let mut vm = VM::new();

            vm.register_module(stdlib);
            vm.register_module(main_module);

            if let Err(e) = vm.eval(
                "main",
                vec![
                    vec![
                        "println",
                        "Option",
                        "String",
                        "Int",
                        "Float",
                        "RangeIter",
                        "Range",
                        "ArrayIter",
                        "Array",
                        "Map",
                    ]
                    .into_iter()
                    .map(|name| IR::new(Code::Import(format!("std.core.{}", name)), 0..0))
                    .collect(),
                    vec![
                        IR::new(Code::Load(LocalId::new("main".to_string())), 0..0),
                        IR::new(Code::CallWithKeywords((vec![], 0)), 0..0),
                    ],
                ]
                .concat(),
            ) {
                let mut message = String::new();

                if !e.stack_trace.is_empty() {
                    message.push_str("Stack trace:\n");
                }

                for trace in e.stack_trace {
                    let (line, column) = parse_line_column(&contents, &trace.pos);

                    message.push_str(&format!(
                        "  > in {}.{}(..) on line {} at {}\n",
                        trace.func.module, trace.func.name, line, column,
                    ));
                }

                message.push_str("RuntimeError: ");
                message.push_str(&e.message);

                if let Some(pos) = e.pos {
                    let (line, column) = parse_line_column(&contents, &pos);

                    message.push_str(&format!(" on line {} at {}", line, column));
                }

                eprintln!("{}", message);
            }
        }
    }
}
