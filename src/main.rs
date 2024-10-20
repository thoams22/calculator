use std::io;

use lexer::LexerError;

use crate::{ast::Expression, parser::Parser};

mod ast;
mod diagnostic;
mod lexer;
mod parser;
mod solver;
mod utils;

// TODO refactor print_console()
// TODO do a proper factorise function in utils
// TODO add error handling and propagation
// TODO make decision tree for simplify to try with and without expanding multinomial expression
// TODO make simplify return a solution in mutiple form (like developped and undevelopped)
// TODO add step by step solution OR a proper error context to better find where the "bad comportement is caused"

fn main() -> Result<(), LexerError> {
    let mut expression = String::new();

    io::stdin()
        .read_line(&mut expression)
        .expect("Failed to read line");

    expression = expression.trim_end().to_string();

    while expression != *"exit" {
        match Parser::default().lex(&expression) {
            Ok(mut parser) => match parser.parse() {
                Ok(statement) => {
                    println!("Before");
                    statement.print_console();

                    let result = statement.solve();

                    match result {
                        Ok(solutions) => {
                            println!("\n");
                            println!("After:\n");

                            solutions.iter().for_each(|solution| {
                                solution.print_console();
                            })
                        }
                        Err(err) => {
                            eprintln!("Error:\n");
                            eprintln!("{}", err);
                        }
                    }
                }
                Err(errs) => {
                    eprintln!("Error:\n");
                    errs.iter().for_each(|err| {
                        eprintln!("{}", err);
                    });
                }
            },
            Err(err) => {
                eprintln!("Error:\n");
                eprintln!("{}", err)
            }
        }

        expression.clear();
        io::stdin()
            .read_line(&mut expression)
            .expect("Failed to read line");
        expression = expression.trim_end().to_string();
    }

    Ok(())
}
