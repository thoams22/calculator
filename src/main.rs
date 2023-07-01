// factorial => Gamma function
// complex numbers
// statistics
// probability
// base conversion
// boolean algebra
// how to make derivate and integral
// laplace fourier and Z transform
// differential equations

// find solution to expression
// display in latex style

use std::io;

use crate::evaluator::evaluation;
use crate::tokenizer::tokenization;

mod evaluator;
mod tokenizer;

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
            println!("history of tokenization {:?}", history);
            println!("Evaluated form {:?}", tokens);
            let evaluated = evaluation(tokens);
            match evaluated {
                Ok(result) => println!("Result is : {}", result),
                Err(error) => println!("Error in Evaluation : {:?}", error),
            }
        }
        (Err(error), history) => println!("Error in tokenization : {:?},\n history {:?}", error, history),
    }
}