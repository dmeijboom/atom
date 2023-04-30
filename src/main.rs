use std::{fs, path::PathBuf, process::exit};

use anyhow::Result;
use ast::Location;
use clap::Parser as _;
use logos::Logos;

mod ast;
mod parser;
mod token;

use parser::{Error, Parser};
use token::Token;

#[derive(clap::Parser)]
struct Opts {
    source: PathBuf,
}

fn main() -> Result<()> {
    let opts = Opts::parse();
    let source = fs::read_to_string(opts.source)?;
    let lexer = Token::lexer(&source);
    let parser = Parser::new(lexer);

    match parser.parse() {
        Ok(tree) => println!("{:#?}", tree),
        Err(e) => {
            let message = format!("{}", e);

            match e {
                Error::Syntax { location } => {
                    eprintln!(
                        "ParseError: {} at {}:{}",
                        message, location.line, location.column
                    );
                    show_error(&source, location)
                }
                Error::UnexpectedToken { location, .. } => {
                    eprintln!(
                        "ParseError: {} at {}:{}",
                        message, location.line, location.column
                    );
                    show_error(&source, location)
                }
                _ => {}
            };

            exit(1);
        }
    }

    Ok(())
}

fn show_error(source: &str, location: Location) {
    let summary = source
        .lines()
        .enumerate()
        .filter(|(idx, _)| *idx + 1 >= location.line - 1 && *idx < location.line)
        .collect::<Vec<_>>();

    println!();

    for (line_no, line) in summary {
        let prefix = format!("{} | ", line_no + 1);

        eprintln!("{}{}", prefix, line);

        if line_no + 1 == location.line {
            eprintln!("{}^", " ".repeat(prefix.len() + location.column - 1));
        }
    }
}
