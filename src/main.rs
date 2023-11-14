use std::io;

use crate::{ast::Expression, parser::Parser, solver::solve, utils::substitute};

mod ast;
mod diagnostic;
mod lexer;
mod parser;
mod solver;
mod utils;

fn main() {
    let mut expression = String::new();

    io::stdin()
        .read_line(&mut expression)
        .expect("Failed to read line");
    expression = expression.trim_end().to_string();

    while expression != *"exit" {
        let mut parser = Parser::new(&expression);

        if !parser.get_diagnostic_message().is_empty() {
            println!("\n***LEXER ERROR***\n");

            for diag in parser.get_diagnostic_message() {
                println!("{}", diag);
            }
            println!("\n***END OF ERROR***");
            println!();
        } else {
            let statement = parser.parse();

            if !parser.get_diagnostic_message().is_empty() {
                println!("\n*** PARSER ERROR***\n");

                for diag in parser.get_diagnostic_message() {
                    println!("{}", diag);
                }
                println!("\n***END OF ERROR***");
                println!();
            } else {

                println!("Before");
                statement.print_console();

                let result = statement.solve();

                println!("\n");
                println!("After:\n");
                result.iter().for_each(|solution| {
                    solution.print_console();
                });
            }
        }

        expression.clear();
        io::stdin()
            .read_line(&mut expression)
            .expect("Failed to read line");
        expression = expression.trim_end().to_string();
    }
}
