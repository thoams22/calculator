// TODO next functionality

// add a isolator only for one variables
// add a sustitutor -> y^4 + 6y^2 + 2 = 0 -> x = y^2, x^2 + 6x + 2 = 0

// Systeme équation
// Polynome
// How to make derivate and integral
// Differential equations
// Complex numbers

// laplace fourier and Z transform
// statistics
// probability
// base conversion
// boolean algebra
// factorial => Gamma function
// matrix

use std::io;

use crate::{ast::Expression, parser::Parser, solver::solve};

mod ast;
mod diagnostic;
mod lexer;
mod parser;
mod solver;

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
            let result = parser.parse();

            // for statement in &result {
            //     println!("\n");
            //     statement.print_tree(None);
            // }

            if !parser.get_diagnostic_message().is_empty() {
                println!("\n*** PARSER ERROR***\n");

                for diag in parser.get_diagnostic_message() {
                    println!("{}", diag);
                }
                println!("\n***END OF ERROR***");
                println!();
            } else {
                let mut simplified_expression: Vec<Expression> = Vec::new();

                for statement in result {
                    simplified_expression.push(statement.simplify());
                }

                for statement in simplified_expression {
                    // statement.print_tree(None);
                    statement.print_console();

                    let solved = solve(statement, None);

                    println!("\n");
                    println!("Solved expression:\n");
                    solved.iter().for_each(|solution| {
                        // solution.print_tree(None);
                        println!("\n");
                        solution.print_console();
                    });
                }
            }
        }

        expression.clear();
        io::stdin()
            .read_line(&mut expression)
            .expect("Failed to read line");
        expression = expression.trim_end().to_string();
    }
}
