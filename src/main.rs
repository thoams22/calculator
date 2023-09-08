// TODO next functionality

// add a isolator only for one variables
// add a sustitutor -> y^4 + 6y^2 + 2 = 0 -> x = y^2, x^2 + 6x + 2 = 0

// Systeme équation
// Polynome
// Solve for a var
// How to make derivate and integral
// Differential equations
// Complex numbers

// laplace fourier and Z transform
// statistics
// probability
// base conversion
// boolean algebra
// factorial => Gamma function
use std::io;

use crate::{parser::Parser, ast::Expression};

mod diagnostic;
mod expression;
mod lexer;
mod parser;
mod tokenizer;
mod ast;

fn main() {
    let mut expression = String::new();

    io::stdin()
        .read_line(&mut expression)
        .expect("Failed to read line");
    expression = expression.trim_end().to_string();

    while expression != "exit".to_string() {

        let mut parser = Parser::new(&expression);

        println!("\nTokens Lexed\n");
        for token in parser.get_tokens() {
            print!("{token}");
        }
        println!();


        if !parser.get_diagnostic_message().is_empty() {
            println!("\n***LEXER ERROR***\n");

            for diag in parser.get_diagnostic_message() {
                println!("{}", diag);
            }
            println!("\n***END OF ERROR***");
            println!();
        } else {
            let result = parser.parse();

            for statement in &result {
                println!("\n");
                statement.print(None);
            }

            if !parser.get_diagnostic_message().is_empty() {
                println!("\n*** PARSER ERROR***\n");

                for diag in parser.get_diagnostic_message() {
                    println!("{}", diag);
                }
                println!("\n***END OF ERROR***");
                println!();
            } 

            else {

                let mut simplified_expression: Vec<Expression> = Vec::new();
                
                for statement in result {
                    simplified_expression.push(statement.simplify());
                }

                for statement in simplified_expression {
                    println!("\n");
                    statement.print(None);
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
