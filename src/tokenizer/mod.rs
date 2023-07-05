#[derive(PartialEq, Clone, Debug)]
pub enum TokenError {
    TokenNotSupported(char),
    MissingParenthesis,
    BadMinusCase,
    BadFloatParsing,
    BadVariableParsing,
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Instruction {
    LeftParenthesis,
    Addition,
    Substraction,
    Multiplication,
    ImplicitMultiplication,
    Division,
    Exponentiation,
    Function(Function),
    RightParenthesis,
    Number(f64),
    Variable(char),
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Function {
    Ln,
    Log2,
    Log10,
    Log,
    Sqrt,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
}

// helper to respect left to right order of operation for operation of same precedence
fn precedence(token: &Instruction) -> u8 {
    match token {
        Instruction::Addition | Instruction::Substraction => 2,
        Instruction::Multiplication | Instruction::Division => 3,
        Instruction::Exponentiation => 4,
        _ => 0,
    }
}

// function supported in Lowercase if in uppercase will not be detected as function but as variable
fn is_function(token: &str) -> Option<Instruction> {
    match token {
        "ln(" => Some(Instruction::Function(Function::Ln)),
        "log2(" => Some(Instruction::Function(Function::Log2)),
        "log10(" => Some(Instruction::Function(Function::Log10)),
        "log(" => Some(Instruction::Function(Function::Log)),
        "sqrt(" => Some(Instruction::Function(Function::Sqrt)),
        "sin(" => Some(Instruction::Function(Function::Sin)),
        "cos(" => Some(Instruction::Function(Function::Cos)),
        "tan(" => Some(Instruction::Function(Function::Tan)),
        "asin(" => Some(Instruction::Function(Function::Asin)),
        "acos(" => Some(Instruction::Function(Function::Acos)),
        "atan(" => Some(Instruction::Function(Function::Atan)),
        _ => None,
    }
}

fn try_parse_f64(
    current_number: &mut String,
    instruction_stack: &mut Vec<Instruction>,
    history: &mut Vec<Instruction>,
) -> Result<Vec<Instruction>, TokenError> {
    match current_number.parse::<f64>() {
        Ok(number) => {
            let mut tokenized: Vec<Instruction> = vec![Instruction::Number(number)];
            if Some(&Instruction::ImplicitMultiplication) == history.last() {
                tokenized.push(instruction_stack.pop().unwrap());
            }
            history.push(Instruction::Number(number));
            current_number.clear();
            Ok(tokenized)
        }
        Err(_) => Err(TokenError::BadFloatParsing),
    }
}

fn try_parse_variable(
    current_function: &mut String,
    instruction_stack: &mut Vec<Instruction>,
    history: &mut Vec<Instruction>,
) -> Result<Vec<Instruction>, TokenError> {

    let mut tokenized: Vec<Instruction> = Vec::new();
    let binding = current_function.clone();

    for token in binding.chars() {
        // check if current_function is a function 
        if let Some(result) = is_function(current_function) {
                // check if implicit multiplication with last token
                if let Some(last_history) = history.last() {
                    match last_history {
                        Instruction::Number(_)
                        | Instruction::RightParenthesis
                        | Instruction::Variable(_) => {
                            // for laft to right order of operation
                            if let Some(last_instruction) = instruction_stack.last() {
                                if precedence(last_instruction) >= 3 {
                                    tokenized.push(instruction_stack.pop().unwrap());
                                }
                            }
                            instruction_stack.push(Instruction::Multiplication);
                            history.push(Instruction::ImplicitMultiplication);
                        }
                        _ => {}
                    }
                // push parenthesis and function
                instruction_stack.push(Instruction::LeftParenthesis);
                instruction_stack.push(result);
                history.push(result);
                history.push(Instruction::LeftParenthesis);
                // return because when funtion found we are a the end of current_function
                return Ok(tokenized);
            }
        }
        match token {
            '(' => {
                // check for implicit multiplication
                if let Some(Instruction::Variable(_)) = history.last() {
                    if let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 3 {
                            tokenized.push(instruction_stack.pop().unwrap());
                        }
                    }
                    instruction_stack.push(Instruction::Multiplication);
                    history.push(Instruction::ImplicitMultiplication);
                }
                // push parenthesis because we are a the end of current_function
                instruction_stack.push(Instruction::LeftParenthesis);
                history.push(Instruction::LeftParenthesis);
            }
            c if c.is_ascii_alphabetic() => {
                // check for implicit multiplication
                if let Some(last_history) = history.last() {
                    match last_history {
                        Instruction::Number(_)
                        | Instruction::RightParenthesis
                        | Instruction::Variable(_) => {
                            if let Some(last_instruction) = instruction_stack.last() {
                                if precedence(last_instruction) >= 3 {
                                    tokenized.push(instruction_stack.pop().unwrap());
                                }
                            }
                            instruction_stack.push(Instruction::Multiplication);
                            history.push(Instruction::ImplicitMultiplication);
                        }
                        _ => {}
                    }
                }
                // push the variable
                tokenized.push(Instruction::Variable(c));
                history.push(Instruction::Variable(c));
            }
            _ => return Err(TokenError::BadVariableParsing),
        }
        // remove the first char of current_function for the next is_function() check
        current_function.remove(0);
    }
    current_function.clear();
    Ok(tokenized)
}

pub fn tokenization(expression: &str) -> (Result<Vec<Instruction>, TokenError>, Vec<Instruction>) {
    // variable for the shunting-yard algorithm
    let mut tokenized: Vec<Instruction> = Vec::new();
    let mut instruction_stack: Vec<Instruction> = Vec::new();
    // history for debbuging and for multiple minus
    let mut history: Vec<Instruction> = Vec::new();
    // parenthesis count to trow error if not pair
    let mut parenthesis: i16 = 0;
    // storage to construct number and function
    let mut current_number = String::new();
    let mut current_function = String::new();

    for token in expression.chars() {
        // if token is a number
        if token.is_ascii_digit() || token == '.' {
            // check for implicit multiplication
            if let Some(Instruction::RightParenthesis) = history.last() {
                if let Some(last_instruction) = instruction_stack.last() {
                    if precedence(last_instruction) >= 3 {
                        tokenized.push(instruction_stack.pop().unwrap());
                    }
                }
                instruction_stack.push(Instruction::Multiplication);
                history.push(Instruction::ImplicitMultiplication);
            }
            // for detecting function like log2() and log10()
            else if !current_function.is_empty() {
                current_function.push(token);
                continue;
            }
            current_number.push(token);
        }
        // if token is a lowercase letter
        else if token.is_ascii_alphabetic() {
            // check if this was a number before and it is not yet push to tokenized
            if !current_number.is_empty() && current_number != "-" {
                match try_parse_f64(&mut current_number, &mut instruction_stack, &mut history) {
                    Ok(result) => {
                        tokenized.extend(result);
                    }
                    Err(error) => return (Err(error), history),
                }
            }
            current_function.push(token);
        } else {
            // check if this was a number before and it is not yet push to tokenized
            if !current_number.is_empty() && current_number != "-" {
                match try_parse_f64(&mut current_number, &mut instruction_stack, &mut history) {
                    Ok(result) => {
                        tokenized.extend(result);
                    }
                    Err(error) => return (Err(error), history),
                }
            }
            // check for variables
            if !current_function.is_empty() && token != '(' {
                match try_parse_variable(
                    &mut current_function,
                    &mut instruction_stack,
                    &mut history,
                ) {
                    Ok(result) => {
                        tokenized.extend(result);
                    }
                    Err(error) => return (Err(error), history),
                }
            }
            match token {
                '/' => {
                    if let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 3 {
                            tokenized.push(instruction_stack.pop().unwrap());
                        }
                    }
                    history.push(Instruction::Division);
                    instruction_stack.push(Instruction::Division);
                }
                '*' => {
                    if let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 3 {
                            tokenized.push(instruction_stack.pop().unwrap());
                        }
                    }
                    history.push(Instruction::Multiplication);
                    instruction_stack.push(Instruction::Multiplication);
                }
                '+' => {
                    if let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 2 {
                            tokenized.push(instruction_stack.pop().unwrap());
                        }
                    }
                    history.push(Instruction::Addition);
                    instruction_stack.push(Instruction::Addition);
                }
                '-' => {
                    // check to simplify for multiple minus
                    match (history.last(), instruction_stack.last()) {
                        (None, None) => {
                            current_number.push('-');
                        }
                        (Some(Instruction::Number(_)), _)
                        | (Some(Instruction::RightParenthesis), _) => {
                            if let Some(last_instruction) = instruction_stack.last() {
                                if precedence(last_instruction) >= 2 {
                                    tokenized.push(instruction_stack.pop().unwrap());
                                }
                            }
                            current_number.push('-');
                            instruction_stack.push(Instruction::Addition);
                        }
                        (Some(Instruction::Substraction), Some(Instruction::Substraction)) => {
                            instruction_stack.pop();
                            instruction_stack.push(Instruction::Addition);
                        }
                        (Some(Instruction::Substraction), Some(Instruction::Addition)) => {
                            instruction_stack.pop();
                            instruction_stack.push(Instruction::Substraction);
                        }
                        (Some(Instruction::Division), _)
                        | (Some(Instruction::LeftParenthesis), _)
                        | (Some(Instruction::Multiplication), _)
                        | (Some(Instruction::Addition), _)
                        | (Some(Instruction::Exponentiation), _) => {
                            current_number.push('-');
                        }
                        (Some(Instruction::Substraction), _) => {
                            if current_number.is_empty() {
                                current_number.push('-');
                            } else {
                                current_number.pop();
                            }
                        }
                        (_, _) => {
                            return (Err(TokenError::BadMinusCase), history);
                        }
                    };
                    history.push(Instruction::Substraction);
                }
                '^' => {
                    if let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) > 4 {
                            tokenized.push(instruction_stack.pop().unwrap());
                        }
                    }
                    history.push(Instruction::Exponentiation);
                    instruction_stack.push(Instruction::Exponentiation);
                }
                '(' => {
                    parenthesis += 1;
                    // if ( is preceded by a function
                    if !current_function.is_empty() {
                        // if function is preceded by minus
                        if current_number == "-" {
                            tokenized.push(Instruction::Number(-1.0));
                            if let Some(last_instruction) = instruction_stack.last() {
                                if precedence(last_instruction) >= 3 {
                                    tokenized.push(instruction_stack.pop().unwrap());
                                }
                            }
                            instruction_stack.push(Instruction::Multiplication);
                            current_number.clear();
                        }
                        // check if function is supported else trow error
                        current_function.push('(');
                        match try_parse_variable(
                            &mut current_function,
                            &mut instruction_stack,
                            &mut history,
                        ) {
                            Ok(result) => {
                                tokenized.extend(result);
                            }
                            Err(error) => return (Err(error), history),
                        }

                        current_function.clear();
                        continue;
                    }
                    // if ( is preceded by a number add implicit multiplication
                    else if let Some(last_history) = history.last() {
                        match last_history {
                            Instruction::Number(_) | Instruction::RightParenthesis => {
                                if let Some(last_instruction) = instruction_stack.last() {
                                    if precedence(last_instruction) >= 3 {
                                        tokenized.push(instruction_stack.pop().unwrap());
                                    }
                                }
                                instruction_stack.push(Instruction::Multiplication);
                                history.push(Instruction::ImplicitMultiplication);
                            }
                            _ => {}
                        }
                    }
                    instruction_stack.push(Instruction::LeftParenthesis);
                    history.push(Instruction::LeftParenthesis);
                }
                ')' => {
                    parenthesis -= 1;
                    history.push(Instruction::RightParenthesis);
                    loop {
                        // pop to tokenized until ( find or return error if ( missing and instruction_stack.last() == None
                        if let Some(last_instruction) = instruction_stack.last() {
                            if last_instruction != &Instruction::LeftParenthesis {
                                tokenized.push(instruction_stack.pop().unwrap());
                            } else {
                                instruction_stack.pop();
                                break;
                            }
                        } else {
                            return (Err(TokenError::MissingParenthesis), history);
                        }
                    }
                }
                // for log(x, y)
                ',' => {
                    // parse the number before , or Error
                    if !current_number.is_empty() && current_number != "-" {
                        match try_parse_f64(
                            &mut current_number,
                            &mut instruction_stack,
                            &mut history,
                        ) {
                            Ok(result) => {
                                tokenized.extend(result);
                            }
                            Err(error) => return (Err(error), history),
                        }
                    }
                }
                // when app run in cmdline '\r' '\n' appear at the end
                '\r' | '\n' => break,
                ' ' => continue, // ignore space
                // in case of a char like # that as no utility
                _ => return (Err(TokenError::TokenNotSupported(token)), history),
            }
        }
    }

    // if the last token is a number parse it or Error
    if !current_number.is_empty() {
        match try_parse_f64(&mut current_number, &mut instruction_stack, &mut history) {
            Ok(result) => {
                tokenized.extend(result);
            }
            Err(error) => return (Err(error), history),
        }
    }

    // check for variables
    if !current_function.is_empty() {
        match try_parse_variable(&mut current_function, &mut instruction_stack, &mut history) {
            Ok(result) => {
                tokenized.extend(result);
            }
            Err(error) => return (Err(error), history),
        }
    }

    // check if there was a pair number of parenthesis or Error
    if parenthesis != 0 {
        return (Err(TokenError::MissingParenthesis), history);
    }

    // pop instruction_stack in tokenized if ther was Operation left inside
    tokenized.extend(instruction_stack.drain(..).rev());

    (Ok(tokenized), history)
}

// test mostly for when i add a functionnality to see if it break something
#[cfg(test)]
mod tests {
    use crate::tokenizer::{tokenization, Function, Instruction, TokenError};

    #[test]
    fn blank() {
        let result: (Result<Vec<Instruction>, TokenError>, Vec<Instruction>) =
            tokenization(" 2* 3   / 4         +4 -9");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(3.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Division,
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Number(-9.0),
                Instruction::Addition,
            ]),
            result.0,
            "blank 1,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn negative_number() {
        let mut result = tokenization("-2+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 1,\n history = {:?}",
            result.1
        );
        result = tokenization("2-4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 2,\n history = {:?}",
            result.1
        );
        result = tokenization("-2-4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 3,\nhistory = {:?}",
            result.1
        );
        result = tokenization("-2--4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Substraction,
            ]),
            result.0,
            "negative_number 4,\n history = {:?}",
            result.1
        );
        result = tokenization("--2+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 5,\n history = {:?}",
            result.1
        );
        result = tokenization("---2+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 6,\n history = {:?}",
            result.1
        );
        result = tokenization("-2--4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Substraction,
            ]),
            result.0,
            "negative_number 7,\n history = {:?}",
            result.1
        );
        result = tokenization("-2---4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 8,\n history = {:?}",
            result.1
        );
        result = tokenization("-2/-4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Division,
            ]),
            result.0,
            "negative_number 9,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*-4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Multiplication,
            ]),
            result.0,
            "negative_number 10,\n history = {:?}",
            result.1
        );
        result = tokenization("-2+-4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 11,\n history = {:?}",
            result.1
        );
        result = tokenization("--2/--4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Division,
            ]),
            result.0,
            "negative_number 12,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*--4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
            ]),
            result.0,
            "negative_number 13,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*(--4/2-2)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Number(2.0),
                Instruction::Division,
                Instruction::Number(-2.0),
                Instruction::Addition,
                Instruction::Multiplication,
            ]),
            result.0,
            "negative_number 14,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*(--4/-2--2)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Number(-2.0),
                Instruction::Division,
                Instruction::Number(-2.0),
                Instruction::Substraction,
                Instruction::Multiplication,
            ]),
            result.0,
            "negative_number 15,\n history = {:?}",
            result.1
        );
        result = tokenization("--2^---4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-4.0),
                Instruction::Exponentiation,
            ]),
            result.0,
            "negative_number 16,\n history = {:?}",
            result.1
        );
        result = tokenization("2(-4)-8");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-4.0),
                Instruction::Multiplication,
                Instruction::Number(-8.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 17,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn unknown_token() {
        let mut result = tokenization("@");
        assert_eq!(
            Err(TokenError::TokenNotSupported('@')),
            result.0,
            "unknown_token 1,\n history = {:?}",
            result.1
        );
        result = tokenization("#");
        assert_eq!(
            Err(TokenError::TokenNotSupported('#')),
            result.0,
            "unknown_token 2,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn parenthesis() {
        let mut result = tokenization("2*(3+4)+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "parenthesis 1,\n history = {:?}",
            result.1
        );
        result = tokenization("2+((3*4)+4)*6");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Number(6.0),
                Instruction::Multiplication,
                Instruction::Addition,
            ]),
            result.0,
            "parenthesis 2,\n history = {:?}",
            result.1
        );
        result = tokenization("2+(4)*6");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Number(6.0),
                Instruction::Multiplication,
                Instruction::Addition,
            ]),
            result.0,
            "parenthesis 3,\n history = {:?}",
            result.1
        );
        result = tokenization("2*(4+6");
        assert_eq!(
            Err(TokenError::MissingParenthesis),
            result.0,
            "parenthesis 4,\n history = {:?}",
            result.1
        );
        result = tokenization("2*4+6)");
        assert_eq!(
            Err(TokenError::MissingParenthesis),
            result.0,
            "parenthesis 5,\n history = {:?}",
            result.1
        );

        result = tokenization("(2*4+6)(2*4+6)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(6.0),
                Instruction::Addition,
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(6.0),
                Instruction::Addition,
                Instruction::Multiplication,
            ]),
            result.0,
            "parenthesis 6,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn exponentiation() {
        let mut result = tokenization("2^4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Exponentiation,
            ]),
            result.0,
            "exponentiation 1,\n history = {:?}",
            result.1
        );
        result = tokenization("2^-4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-4.0),
                Instruction::Exponentiation,
            ]),
            result.0,
            "exponentiation 2,\n history = {:?}",
            result.1
        );
        result = tokenization("-2^--4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Exponentiation,
            ]),
            result.0,
            "exponentiation 3,\n history = {:?}",
            result.1
        );
        result = tokenization("-2^4^6");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Number(6.0),
                Instruction::Exponentiation,
                Instruction::Exponentiation,
            ]),
            result.0,
            "exponentiation 4,\n history = {:?}",
            result.1
        );
        result = tokenization("(-2^4)^6");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Exponentiation,
                Instruction::Number(6.0),
                Instruction::Exponentiation,
            ]),
            result.0,
            "exponentiation 5,\n history = {:?}",
            result.1
        );
        result = tokenization("(-2+4)^6");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Number(6.0),
                Instruction::Exponentiation,
            ]),
            result.0,
            "exponentiation 6,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn implicit_multiplication() {
        let mut result = tokenization("2(3+4)+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "implicit_multiplication 1,\n history = {:?}",
            result.1
        );
        result = tokenization("(3+4)4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Number(4.0),
                Instruction::Multiplication,
            ]),
            result.0,
            "implicit_multiplication 2,\n history = {:?}",
            result.1
        );
        result = tokenization("2*2(3(4^2)4)5");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(2.0),
                Instruction::Multiplication,
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Multiplication,
                Instruction::Number(5.0),
                Instruction::Multiplication
            ]),
            result.0,
            "implicit_multiplication 3,\n history = {:?}",
            result.1
        );
        result = tokenization("2(3+4)4+3");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(3.0),
                Instruction::Addition,
            ]),
            result.0,
            "implicit_multiplication 4,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn function() {
        let mut result = tokenization("ln(4-3)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(-3.0),
                Instruction::Addition,
                Instruction::Function(Function::Ln),
            ]),
            result.0,
            "function 1,\n history = {:?}",
            result.1
        );
        result = tokenization("4ln(4-3)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(4.0),
                Instruction::Number(-3.0),
                Instruction::Addition,
                Instruction::Function(Function::Ln),
                Instruction::Multiplication
            ]),
            result.0,
            "function 2,\n history = {:?}",
            result.1
        );
        result = tokenization("ln(4-3)4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(-3.0),
                Instruction::Addition,
                Instruction::Function(Function::Ln),
                Instruction::Number(4.0),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 3,\n history = {:?}",
            result.1
        );

        result = tokenization("ln(4-3)4ln(8-5)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(-3.0),
                Instruction::Addition,
                Instruction::Function(Function::Ln),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Number(-5.0),
                Instruction::Addition,
                Instruction::Function(Function::Ln),
                Instruction::Multiplication
            ]),
            result.0,
            "function 4,\n history = {:?}",
            result.1
        );
        result = tokenization("4ln(4-3)--8--7");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(4.0),
                Instruction::Number(-3.0),
                Instruction::Addition,
                Instruction::Function(Function::Ln),
                Instruction::Multiplication,
                Instruction::Number(-8.0),
                Instruction::Substraction,
                Instruction::Number(-7.0),
                Instruction::Substraction,
            ]),
            result.0,
            "function 5,\n history = {:?}",
            result.1
        );
        result = tokenization("log2(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Function(Function::Log2),
            ]),
            result.0,
            "function 6,\n history = {:?}",
            result.1
        );
        result = tokenization("log10(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Function(Function::Log10),
            ]),
            result.0,
            "function 7,\n history = {:?}",
            result.1
        );
        result = tokenization("log(3, 4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Function(Function::Log),
            ]),
            result.0,
            "function 8,\n history = {:?}",
            result.1
        );
        result = tokenization("sin(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Function::Sin),
            ]),
            result.0,
            "function 9,\n history = {:?}",
            result.1
        );
        result = tokenization("cos(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Function::Cos),
            ]),
            result.0,
            "function 10,\n history = {:?}",
            result.1
        );
        result = tokenization("tan(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Function::Tan),
            ]),
            result.0,
            "function 11,\n history = {:?}",
            result.1
        );
        result = tokenization("sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
            ]),
            result.0,
            "function 12,\n history = {:?}",
            result.1
        );
        result = tokenization("asin(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Function::Asin),
            ]),
            result.0,
            "function 13,\n history = {:?}",
            result.1
        );
        result = tokenization("acos(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Function::Acos),
            ]),
            result.0,
            "function 14,\n history = {:?}",
            result.1
        );
        result = tokenization("atan(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Function::Atan),
            ]),
            result.0,
            "function 15,\n history = {:?}",
            result.1
        );

        result = tokenization("-sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-1.0),
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 16,\n history = {:?}",
            result.1
        );

        result = tokenization("-1sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-1.0),
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 17,\n history = {:?}",
            result.1
        );

        result = tokenization("2*-sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-1.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 18,\n history = {:?}",
            result.1
        );

        result = tokenization("2*-sqrt(4)*2^2");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-1.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(2.0),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
            ]),
            result.0,
            "function 19,\n history = {:?}",
            result.1
        );

        result = tokenization("2*--sqrt(4)*2^2");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(2.0),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
            ]),
            result.0,
            "function 20,\n history = {:?}",
            result.1
        );

        result = tokenization("2*---sqrt(4)*2^-2");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-1.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(2.0),
                Instruction::Number(-2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
            ]),
            result.0,
            "function 21,\n history = {:?}",
            result.1
        );

        result = tokenization("2sqrt(4)sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 22,\n history = {:?}",
            result.1
        );

        result = tokenization("2sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Function(Function::Sqrt),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 23,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn variable() {
        let mut result = tokenization("2a");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Variable('a'),
                Instruction::Multiplication
            ]),
            result.0,
            "variable 1, \n history = {:?}",
            result.1
        );

        result = tokenization("2a+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Variable('a'),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "variable 2, \n history = {:?}",
            result.1
        );

        result = tokenization("2a(5)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Variable('a'),
                Instruction::Multiplication,
                Instruction::Number(5.0),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 3, \n history = {:?}",
            result.1
        );

        result = tokenization("2asin(5)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(5.0),
                Instruction::Function(Function::Asin),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 4, \n history = {:?}",
            result.1
        );

        result = tokenization("2Asin(5)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Variable('A'),
                Instruction::Multiplication,
                Instruction::Number(5.0),
                Instruction::Function(Function::Sin),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 5, \n history = {:?}",
            result.1
        );

        result = tokenization("2log5");
        assert_eq!(
            Err(TokenError::BadVariableParsing),
            result.0,
            "variable 6, \n history = {:?}",
            result.1
        );

        result = tokenization("2a2b");
        assert_eq!(
            Err(TokenError::BadVariableParsing),
            result.0,
            "variable 7, \n history = {:?}",
            result.1
        );

        result = tokenization("abc");
        assert_eq!(
            Ok(vec![
                Instruction::Variable('a'),
                Instruction::Variable('b'),
                Instruction::Multiplication,
                Instruction::Variable('c'),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 8, \n history = {:?}",
            result.1
        );

        result = tokenization("a*b*c");
        assert_eq!(
            Ok(vec![
                Instruction::Variable('a'),
                Instruction::Variable('b'),
                Instruction::Multiplication,
                Instruction::Variable('c'),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 9, \n history = {:?}",
            result.1
        );

        result = tokenization("abc(abc)");
        assert_eq!(
            Ok(vec![
                Instruction::Variable('a'),
                Instruction::Variable('b'),
                Instruction::Multiplication,
                Instruction::Variable('c'),
                Instruction::Multiplication,
                Instruction::Variable('a'),
                Instruction::Variable('b'),
                Instruction::Multiplication,
                Instruction::Variable('c'),
                Instruction::Multiplication,
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 10, \n history = {:?}",
            result.1
        );
    }
}
