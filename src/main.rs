// TODO next functionality 

// add a factorisor/reducer -> ab + cb = (a+b)c, a^2 + 2ab + b^2 = (a+b)^2, (a-b)(a+b) = (a^2-b^2)
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

use crate::evaluator::evaluation;
use crate::tokenizer::tokenization;

mod evaluator;
mod tokenizer;
mod expression;

fn main() {
    println!("Please input the expression to evaluate");

    let mut expression = String::new();

    io::stdin()
        .read_line(&mut expression)
        .expect("Failed to read line");

    print!("Row Input: {expression}");

    println!("Parsed: {:?}", expression.chars());

    let tokenized = tokenization(&expression);

    match tokenized {
        (Ok(tokens), history) => {
            // println!("history of tokenization {:?}", history);
            println!("Evaluated form {:?}", tokens);
            let evaluated = evaluation(tokens);
            match evaluated {
                Ok(result) => println!("Result is : {}", result),
                Err(error) => println!("Error in Evaluation : {:?},\n history {:?}", error, history),
            }
        }
        (Err(error), history) => println!("Error in tokenization : {:?},\n history {:?}", error, history),
    }
}