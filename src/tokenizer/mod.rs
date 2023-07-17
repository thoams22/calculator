use crate::expression::Functions;

#[derive(PartialEq, Clone, Debug)]
pub enum CalcError {
    TokenNotSupported(char),
    MissingParenthesis,
    BadMinusCase,
    BadFloatParsing,
    BadVariableParsing,
    InsufficientOperand,
    InsufficientNumber,
    UnevaluableToken,
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Instruction {
    LeftParenthesis,
    ImplicitMultiplication,
    RightParenthesis,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Exponentiation,
    Function(Functions),
    Number(f64),
    Variable(char),
}


// helper to respect left to right order of operation for operation of same precedence
fn precedence(token: &Instruction) -> u8 {
    match token {
        Instruction::Addition | Instruction::Subtraction => 2,
        Instruction::Multiplication | Instruction::Division => 3,
        Instruction::Exponentiation => 4,
        _ => 0,
    }
}

// function supported in Lowercase if in uppercase will not be detected as function but as variable
fn is_function(token: &str) -> Option<Instruction> {
    match token {
        "ln(" => Some(Instruction::Function(Functions::Ln)),
        "log2(" => Some(Instruction::Function(Functions::Log2)),
        "log10(" => Some(Instruction::Function(Functions::Log10)),
        "log(" => Some(Instruction::Function(Functions::Log)),
        "sqrt(" => Some(Instruction::Function(Functions::Sqrt)),
        "sin(" => Some(Instruction::Function(Functions::Sin)),
        "cos(" => Some(Instruction::Function(Functions::Cos)),
        "tan(" => Some(Instruction::Function(Functions::Tan)),
        "asin(" => Some(Instruction::Function(Functions::Asin)),
        "acos(" => Some(Instruction::Function(Functions::Acos)),
        "atan(" => Some(Instruction::Function(Functions::Atan)),
        _ => None,
    }
}

fn try_parse_f64(
    current_number: &mut String,
    instruction_stack: &mut Vec<Instruction>,
    history: &mut Vec<Instruction>,
) -> Result<Vec<Instruction>, CalcError> {
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
        Err(_) => Err(CalcError::BadFloatParsing),
    }
}

fn try_parse_variable(
    current_function: &mut String,
    instruction_stack: &mut Vec<Instruction>,
    history: &mut Vec<Instruction>,
) -> Result<Vec<Instruction>, CalcError> {
    let mut tokenized: Vec<Instruction> = Vec::new();
    let binding = current_function.clone();

    for token in binding.chars() {
        // check if current_function is a function
        if let Some(result) = is_function(current_function) {
            println!("{:?}", current_function);
            // check if implicit multiplication with last token
            if let Some(last_history) = history.last() {
                match last_history {
                    Instruction::Number(_)
                    | Instruction::RightParenthesis
                    | Instruction::Variable(_) => {
                        // for left to right order of operation
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
            // push parenthesis and function
            instruction_stack.push(Instruction::LeftParenthesis);
            instruction_stack.push(result);
            history.push(result);
            history.push(Instruction::LeftParenthesis);
            // return because when funtion found we are a the end of current_function
            return Ok(tokenized);
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
            _ => return Err(CalcError::BadVariableParsing),
        }
        // remove the first char of current_function for the next is_function() check
        current_function.remove(0);
    }
    current_function.clear();
    Ok(tokenized)
}

pub fn tokenization(expression: &str) -> (Result<Vec<Instruction>, CalcError>, Vec<Instruction>) {
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
            // for detecting function like log2() and log10()
            if !current_function.is_empty() {
                current_function.push(token);
                continue;
            }

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
                    loop {
                        if let Some(last_instruction) = instruction_stack.last() {
                            if precedence(last_instruction) >= 3 {
                                tokenized.push(instruction_stack.pop().unwrap());
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    history.push(Instruction::Division);
                    instruction_stack.push(Instruction::Division);
                }
                '*' => {
                    loop {
                        if let Some(last_instruction) = instruction_stack.last() {
                            if precedence(last_instruction) >= 3 {
                                tokenized.push(instruction_stack.pop().unwrap());
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    history.push(Instruction::Multiplication);
                    instruction_stack.push(Instruction::Multiplication);
                }
                '+' => {
                    loop {
                        if let Some(last_instruction) = instruction_stack.last() {
                            if precedence(last_instruction) >= 2 {
                                tokenized.push(instruction_stack.pop().unwrap());
                            } else {
                                break;
                            }
                        } else {
                            break;
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
                        | (Some(Instruction::RightParenthesis), _)
                        | (Some(Instruction::Variable(_)), _) => {
                            loop {
                                if let Some(last_instruction) = instruction_stack.last() {
                                    if precedence(last_instruction) >= 2 {
                                        tokenized.push(instruction_stack.pop().unwrap());
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }
                            instruction_stack.push(Instruction::Subtraction);
                        }
                        (Some(Instruction::Subtraction), Some(Instruction::Subtraction)) => {
                            instruction_stack.pop();
                            instruction_stack.push(Instruction::Addition);
                        }
                        (Some(Instruction::Subtraction), Some(Instruction::Addition)) => {
                            instruction_stack.pop();
                            instruction_stack.push(Instruction::Subtraction);
                        }
                        (Some(Instruction::Division), _)
                        | (Some(Instruction::LeftParenthesis), _)
                        | (Some(Instruction::Multiplication), _)
                        | (Some(Instruction::Addition), _)
                        | (Some(Instruction::Exponentiation), _) => {
                            current_number.push('-');
                        }
                        (Some(Instruction::Subtraction), _) => {
                            if current_number.is_empty() {
                                current_number.push('-');
                            } else {
                                current_number.pop();
                            }
                        }
                        (_, _) => {
                            return (Err(CalcError::BadMinusCase), history);
                        }
                    };
                    history.push(Instruction::Subtraction);
                }
                '^' => {
                    history.push(Instruction::Exponentiation);
                    instruction_stack.push(Instruction::Exponentiation);
                }
                '(' => {
                    parenthesis += 1;
                    // tokenized.push(Instruction::LeftParenthesis);
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
                                // tokenized.push(Instruction::RightParenthesis);
                                instruction_stack.pop();
                                break;
                            }
                        } else {
                            return (Err(CalcError::MissingParenthesis), history);
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
                _ => return (Err(CalcError::TokenNotSupported(token)), history),
            }
        }
    }
    
    // check for variables
    if !current_function.is_empty() {

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

        match try_parse_variable(&mut current_function, &mut instruction_stack, &mut history) {
            Ok(result) => {
                tokenized.extend(result);
            }
            Err(error) => return (Err(error), history),
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
    // check if there was a pair number of parenthesis or Error
    if parenthesis != 0 {
        return (Err(CalcError::MissingParenthesis), history);
    }

    // pop instruction_stack in tokenized if ther was Operation left inside
    tokenized.extend(instruction_stack.drain(..).rev());

    (Ok(tokenized), history)
}

pub fn untokenization(tokens: Vec<Instruction>) -> Result<String, CalcError> {
    let mut stack = Vec::new();

    for token in tokens {
        match token {
            Instruction::Number(number) => stack.push(number.to_string()),
            Instruction::Variable(var) => stack.push(var.to_string()),
            Instruction::Addition => {
                let y = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("({x}+{y})"));
            }
            Instruction::Subtraction => {
                let y = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("({x}-{y})"));
            }
            Instruction::Multiplication => {
                let y = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("({x}*{y})"));
            }
            Instruction::Division => {
                let y = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("({x}*{y})"));
            }
            Instruction::Exponentiation => {
                let y = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("({x}^{y})"));
            }
            Instruction::Function(Functions::Ln) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("ln({x})"));
            }
            Instruction::Function(Functions::Log10) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("log10({x})"));
            }
            Instruction::Function(Functions::Log2) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("log2({x})"));
            }
            Instruction::Function(Functions::Log) => {
                let y = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("log({x}, {y})"));
            }
            Instruction::Function(Functions::Sqrt) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("sqrt({x})"));
            }
            Instruction::Function(Functions::Sin) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("sin({x})"));
            }
            Instruction::Function(Functions::Cos) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("cos({x})"));
            }
            Instruction::Function(Functions::Tan) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("tan({x})"));
            }
            Instruction::Function(Functions::Asin) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("asin({x})"));
            }
            Instruction::Function(Functions::Acos) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("acos({x})"));
            }
            Instruction::Function(Functions::Atan) => {
                let x = stack.pop().ok_or(CalcError::InsufficientNumber)?;
                stack.push(format!("atan({x})"));
            }
            _ => return Err(CalcError::UnevaluableToken),
        }
    }

    let result = stack.pop();

    if !stack.is_empty() {
        return Err(CalcError::InsufficientOperand);
    }

    match result {
        Some(mut res) => {
            if res.starts_with('(') && res.ends_with(')') {
                res.remove(0);
                res.remove(res.len()-1);
            }
            return Ok(res);
        }
        None => return Err(CalcError::InsufficientNumber)
    }

}

#[cfg(test)]
mod tests_tokenization {
    use crate::tokenizer::{tokenization, CalcError, Functions, Instruction};

    #[test]
    fn blank() {
        let result: (Result<Vec<Instruction>, CalcError>, Vec<Instruction>) =
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
                Instruction::Number(9.0),
                Instruction::Subtraction,
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
                Instruction::Number(4.0),
                Instruction::Subtraction,
            ]),
            result.0,
            "negative_number 2,\n history = {:?}",
            result.1
        );
        result = tokenization("-2-4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Subtraction,
            ]),
            result.0,
            "negative_number 3,\nhistory = {:?}",
            result.1
        );
        result = tokenization("-2--4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
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
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "negative_number 7,\n history = {:?}",
            result.1
        );
        result = tokenization("-2---4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Subtraction,
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
                Instruction::Number(2.0),
                Instruction::Subtraction,
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
                Instruction::Number(2.0),
                Instruction::Addition,
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
                Instruction::Number(8.0),
                Instruction::Subtraction,
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
            Err(CalcError::TokenNotSupported('@')),
            result.0,
            "unknown_token 1,\n history = {:?}",
            result.1
        );
        result = tokenization("#");
        assert_eq!(
            Err(CalcError::TokenNotSupported('#')),
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
            Err(CalcError::MissingParenthesis),
            result.0,
            "parenthesis 4,\n history = {:?}",
            result.1
        );
        result = tokenization("2*4+6)");
        assert_eq!(
            Err(CalcError::MissingParenthesis),
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
                Instruction::Number(3.0),
                Instruction::Subtraction,
                Instruction::Function(Functions::Ln),
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
                Instruction::Number(3.0),
                Instruction::Subtraction,
                Instruction::Function(Functions::Ln),
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
                Instruction::Number(3.0),
                Instruction::Subtraction,
                Instruction::Function(Functions::Ln),
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
                Instruction::Number(3.0),
                Instruction::Subtraction,
                Instruction::Function(Functions::Ln),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Number(5.0),
                Instruction::Subtraction,
                Instruction::Function(Functions::Ln),
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
                Instruction::Number(3.0),
                Instruction::Subtraction,
                Instruction::Function(Functions::Ln),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Addition,
                Instruction::Number(7.0),
                Instruction::Addition,
            ]),
            result.0,
            "function 5,\n history = {:?}",
            result.1
        );
        result = tokenization("log2(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Function(Functions::Log2),
            ]),
            result.0,
            "function 6,\n history = {:?}",
            result.1
        );
        result = tokenization("log10(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Function(Functions::Log10),
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
                Instruction::Function(Functions::Log),
            ]),
            result.0,
            "function 8,\n history = {:?}",
            result.1
        );
        result = tokenization("sin(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Functions::Sin),
            ]),
            result.0,
            "function 9,\n history = {:?}",
            result.1
        );
        result = tokenization("cos(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Functions::Cos),
            ]),
            result.0,
            "function 10,\n history = {:?}",
            result.1
        );
        result = tokenization("tan(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Functions::Tan),
            ]),
            result.0,
            "function 11,\n history = {:?}",
            result.1
        );
        result = tokenization("sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Function(Functions::Sqrt),
            ]),
            result.0,
            "function 12,\n history = {:?}",
            result.1
        );
        result = tokenization("asin(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Functions::Asin),
            ]),
            result.0,
            "function 13,\n history = {:?}",
            result.1
        );
        result = tokenization("acos(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Functions::Acos),
            ]),
            result.0,
            "function 14,\n history = {:?}",
            result.1
        );
        result = tokenization("atan(0)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(0.0),
                Instruction::Function(Functions::Atan),
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
                Instruction::Function(Functions::Sqrt),
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
                Instruction::Function(Functions::Sqrt),
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
                Instruction::Function(Functions::Sqrt),
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
                Instruction::Function(Functions::Sqrt),
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
                Instruction::Function(Functions::Sqrt),
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
                Instruction::Function(Functions::Sqrt),
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
                Instruction::Function(Functions::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Function(Functions::Sqrt),
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
                Instruction::Function(Functions::Sqrt),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 23,\n history = {:?}",
            result.1
        );

        result = tokenization("-sqrt(4)");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-1.0),
                Instruction::Number(4.0),
                Instruction::Function(Functions::Sqrt),
                Instruction::Multiplication,
            ]),
            result.0,
            "function 24,\n history = {:?}",
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
                Instruction::Function(Functions::Asin),
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
                Instruction::Function(Functions::Sin),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 5, \n history = {:?}",
            result.1
        );

        result = tokenization("2log5");
        assert_eq!(
            Err(CalcError::BadVariableParsing),
            result.0,
            "variable 6, \n history = {:?}",
            result.1
        );

        result = tokenization("2a2b");
        assert_eq!(
            Err(CalcError::BadVariableParsing),
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
        
        result = tokenization("(a-b)(a+b)");
        assert_eq!(
            Ok(vec![
                Instruction::Variable('a'),
                Instruction::Variable('b'),
                Instruction::Subtraction,
                Instruction::Variable('a'),
                Instruction::Variable('b'),
                Instruction::Addition,
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 11, \n history = {:?}",
            result.1
        );

        result = tokenization("(a-b)(a+-b)");
        assert_eq!(
            Ok(vec![
                Instruction::Variable('a'),
                Instruction::Variable('b'),
                Instruction::Subtraction,
                Instruction::Variable('a'),
                Instruction::Number(-1.0),
                Instruction::Variable('b'),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 12, \n history = {:?}",
            result.1
        );

        result = tokenization("-a");
        assert_eq!(
            Ok(vec![
                Instruction::Number(-1.0),
                Instruction::Variable('a'),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 13, \n history = {:?}",
            result.1
        );

        result = tokenization("2*-a");
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-1.0),
                Instruction::Multiplication,
                Instruction::Variable('a'),
                Instruction::Multiplication,
            ]),
            result.0,
            "variable 14, \n history = {:?}",
            result.1
        );
        
        result = tokenization("a*-a+a*-a");
        assert_eq!(
            Ok(vec![
                Instruction::Variable('a'),
                Instruction::Number(-1.0),
                Instruction::Multiplication,
                Instruction::Variable('a'),
                Instruction::Multiplication,
                Instruction::Variable('a'),
                Instruction::Number(-1.0),
                Instruction::Multiplication,
                Instruction::Variable('a'),
                Instruction::Multiplication,
                Instruction::Addition,
            ]),
            result.0,
            "variable 15, \n history = {:?}",
            result.1
        );
    }

    #[test]
    fn operation_priority() {
        let mut result = tokenization("16a^2+16a+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(16.0),
                Instruction::Variable('a'),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(16.0),
                Instruction::Variable('a'),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "operation_priority 1 \n history = {:?}",
            result.1
        );

        result = tokenization("16*2^2+16*2+4");
        assert_eq!(
            Ok(vec![
                Instruction::Number(16.0),
                Instruction::Number(2.0),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(16.0),
                Instruction::Number(2.0),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            result.0,
            "operation_priority 2 \n history = {:?}",
            result.1
        );

        result = tokenization("16*2*3*3^2+5*4*4+5");
        assert_eq!(
            Ok(vec![
                Instruction::Number(16.0),
                Instruction::Number(2.0),
                Instruction::Multiplication,
                Instruction::Number(3.0),
                Instruction::Multiplication,
                Instruction::Number(3.0),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(5.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Number(5.0),
                Instruction::Addition,
            ]),
            result.0,
            "operation_priority 3 \n history = {:?}",
            result.1
        );

        result = tokenization("16*abb^a+dcc+d");
        assert_eq!(
            Ok(vec![
                Instruction::Number(16.0),
                Instruction::Variable('a'),
                Instruction::Multiplication,
                Instruction::Variable('b'),
                Instruction::Multiplication,
                Instruction::Variable('b'),
                Instruction::Variable('a'),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Variable('d'),
                Instruction::Variable('c'),
                Instruction::Multiplication,
                Instruction::Variable('c'),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Variable('d'),
                Instruction::Addition,
            ]),
            result.0,
            "operation_priority 4 \n history = {:?}",
            result.1
        );

        result = tokenization("16*sqrt(4)log2(8)log2(8)^sqrt(4)+5*4*4+5");
        assert_eq!(
            Ok(vec![
                Instruction::Number(16.0),
                Instruction::Number(4.0),
                Instruction::Function(Functions::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Function(Functions::Log2),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Function(Functions::Log2),
                Instruction::Number(4.0),
                Instruction::Function(Functions::Sqrt),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(5.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Number(5.0),
                Instruction::Addition,
            ]),
            result.0,
            "operation_priority 5 \n history = {:?}",
            result.1
        );
    }
}

#[cfg(test)]
mod tests_untokenization {
    use crate::tokenizer::{Functions, Instruction, untokenization};

    #[test]
    fn ok() {
        let mut result = untokenization(vec![
            Instruction::Number(-2.0),
            Instruction::Number(4.0),
            Instruction::Addition,
        ]);
        assert_eq!(
            Ok("-2+4".to_string()),
            result,
            "ok 1"
        );

        result = untokenization(vec![
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
        ]);
        assert_eq!(
            Ok("((2*4)+6)*((2*4)+6)".to_string()),
            result,
            "ok 2"
        );
        result = untokenization(vec![
            Instruction::Number(2.0),
            Instruction::Number(5.0),
            Instruction::Function(Functions::Asin),
            Instruction::Multiplication,
        ]);
        assert_eq!(
            Ok("2*asin(5)".to_string()),
            result,
            "ok 3"
        );

        result = untokenization(vec![
            Instruction::Number(2.0),
            Instruction::Number(3.0),
            Instruction::Number(4.0),
            Instruction::Addition,
            Instruction::Multiplication,
            Instruction::Number(4.0),
            Instruction::Addition,
        ]);
        assert_eq!(
            Ok("(2*(3+4))+4".to_string()),
            result,
            "ok 4"
        );
        
        result = untokenization(vec![
            Instruction::Number(2.0),
            Instruction::Number(-1.0),
            Instruction::Multiplication,
            Instruction::Number(4.0),
            Instruction::Function(Functions::Sqrt),
            Instruction::Multiplication,
            Instruction::Number(2.0),
            Instruction::Number(-2.0),
            Instruction::Exponentiation,
            Instruction::Multiplication,
        ]);
        assert_eq!(
            Ok("((2*-1)*sqrt(4))*(2^-2)".to_string()),
            result,
            "ok 5"
        );

        result = untokenization(vec![
                Instruction::Number(16.0),
                Instruction::Variable('a'),
                Instruction::Number(2.0),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(16.0),
                Instruction::Variable('a'),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Number(4.0),
                Instruction::Addition,
        ]);
        assert_eq!(
            Ok("((16*(a^2))+(16*a))+4".to_string()),
            result,
            "ok 6"
        );

        result = untokenization(vec![
                Instruction::Number(16.0),
                Instruction::Number(4.0),
                Instruction::Function(Functions::Sqrt),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Function(Functions::Log2),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Function(Functions::Log2),
                Instruction::Number(4.0),
                Instruction::Function(Functions::Sqrt),
                Instruction::Exponentiation,
                Instruction::Multiplication,
                Instruction::Number(5.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Addition,
                Instruction::Number(5.0),
                Instruction::Addition,
        ]);
        assert_eq!(
            Ok("((((16*sqrt(4))*log2(8))*(log2(8)^sqrt(4)))+((5*4)*4))+5".to_string()),
            result,
            "ok 7"
        );
    }
}
