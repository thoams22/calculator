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

                let result = match statement {
                    ast::Statement::Simplify(expression) => vec![expression.simplify()],
                    ast::Statement::Solve(expression) => solve(expression, None),
                    ast::Statement::SolveFor(expression, variable) => solve(expression, Some(variable)),
                    ast::Statement::Replace(expression, equality) => solve(substitute(expression, &equality), None),
                    ast::Statement::Error => vec![Expression::Error],
                };

                println!("\n");
                println!("After:\n");
                result.iter().for_each(|solution| {
                    // solution.print_tree(None);
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
