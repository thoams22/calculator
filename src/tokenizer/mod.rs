#[derive(PartialEq, Clone, Debug)]
pub enum TokenError {
    TokenNotSupported(char),
    FunctionNotSupported,
    MissingParenthesis,
    BadMinusCase,
    BadFloatParsing,
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

fn precedence(token: &Instruction) -> u8 {
    match token {
        Instruction::Addition | Instruction::Substraction => 2,
        Instruction::Multiplication | Instruction::Division => 3,
        Instruction::Exponentiation => 4,
        _ => 0,
    }
}

fn is_function(token: &str) -> Result<Instruction, TokenError> {
    match token {
        "ln" => Ok(Instruction::Function(Function::Ln)),
        "log2" => Ok(Instruction::Function(Function::Log2)),
        "log10" => Ok(Instruction::Function(Function::Log10)),
        "log" => Ok(Instruction::Function(Function::Log)),
        "sqrt" => Ok(Instruction::Function(Function::Sqrt)),
        "sin" => Ok(Instruction::Function(Function::Sin)),
        "cos" => Ok(Instruction::Function(Function::Cos)),
        "tan" => Ok(Instruction::Function(Function::Tan)),
        "asin" => Ok(Instruction::Function(Function::Asin)),
        "acos" => Ok(Instruction::Function(Function::Acos)),
        "atan" => Ok(Instruction::Function(Function::Atan)),
        _ => Err(TokenError::FunctionNotSupported),
    }
}

pub fn tokenization(expression: &str) -> (Result<Vec<Instruction>, TokenError>, Vec<Instruction>) {
    let mut tokenized: Vec<Instruction> = Vec::new();
    let mut instruction_stack: Vec<Instruction> = Vec::new();
    let mut history: Vec<Instruction> = Vec::new();

    let mut parenthesis: i16 = 0;
    let mut current_number = String::new();
    let mut current_function = String::new();

    for token in expression.chars() {
        if token.is_ascii_digit() || token == '.' {
            if let Some(Instruction::RightParenthesis) = history.last() {
                if let Some(last_instruction) = instruction_stack.last() {
                    if precedence(last_instruction) >= 3 {
                        tokenized.push(instruction_stack.pop().unwrap());
                    }
                }
                instruction_stack.push(Instruction::Multiplication);
                history.push(Instruction::ImplicitMultiplication);
            } else if !current_function.is_empty() {
                current_function.push(token);
                continue;
            }
            current_number.push(token);
        } else if token.is_ascii_lowercase() {
            if !current_number.is_empty() && current_number != "-" {
                match current_number.parse::<f64>() {
                    Ok(number) => {
                        tokenized.push(Instruction::Number(number));
                        if Some(&Instruction::ImplicitMultiplication) == history.last() {
                            tokenized.push(instruction_stack.pop().unwrap());
                        }
                        history.push(Instruction::Number(number));
                        current_number.clear();
                    }
                    Err(_) => return (Err(TokenError::BadFloatParsing), history),
                }
            }
            if let Some(last_instruction) = history.last() {
                match last_instruction {
                    Instruction::Number(_) | Instruction::RightParenthesis => {
                        instruction_stack.push(Instruction::Multiplication);
                        history.push(Instruction::ImplicitMultiplication);
                    }
                    _ => {}
                }
            }
            current_function.push(token);
        } else {
            if !current_number.is_empty() && current_number != "-" {
                match current_number.parse::<f64>() {
                    Ok(number) => {
                        tokenized.push(Instruction::Number(number));
                        if Some(&Instruction::ImplicitMultiplication) == history.last() {
                            tokenized.push(instruction_stack.pop().unwrap());
                        }
                        history.push(Instruction::Number(number));
                        current_number.clear();
                    }
                    Err(_) => return (Err(TokenError::BadFloatParsing), history),
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
                    if !current_function.is_empty() {
                        // if function is preceded by minus
                        if current_number == "-" {
                            tokenized.push(Instruction::Number(-1.0));
                            if let Some(last_instruction) = instruction_stack.last() {
                                if precedence(last_instruction) >= 3 {
                                    tokenized.push(instruction_stack.pop().unwrap());
                                }
                            Err(error) => println!(
                                "Error finding function : {:?} => {:?}",
                                current_function, error
                            ),
                        }
                        current_function.clear();
                        continue;
                    } else if let Some(Instruction::Number(_)) = history.last() {
                        if let Some(last_instruction) = instruction_stack.last() {
                            if precedence(last_instruction) >= 3 {
                                tokenized.push(instruction_stack.pop().unwrap());
                            }
                        }
                        history.push(Instruction::ImplicitMultiplication);
                        instruction_stack.push(Instruction::Multiplication);
                    }
                    instruction_stack.push(Instruction::LeftParenthesis);
                    history.push(Instruction::LeftParenthesis);
                }
                ')' => {
                    parenthesis -= 1;
                    history.push(Instruction::RightParenthesis);
                    loop {
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
                ',' => {
                    if !current_function.is_empty() {
                        match is_function(&current_function) {
                            Ok(result) => {
                                instruction_stack.push(result);
                                history.push(result);
                            }
                            Err(error) => println!(
                                "Error finding function : {:?} => {:?}",
                                current_function, error
                            ),
                        }
                        current_function.clear();
                    } else             if !current_number.is_empty() && current_number != "-" {
                        match current_number.parse::<f64>() {
                            Ok(number) => {
                                tokenized.push(Instruction::Number(number));
                                if Some(&Instruction::ImplicitMultiplication) == history.last() {
                                    tokenized.push(instruction_stack.pop().unwrap());
                                }
                                history.push(Instruction::Number(number));
                                current_number.clear();
                            }
                            Err(_) => return (Err(TokenError::BadFloatParsing), history),
                        }
                    }
                }
                '\r' | '\n' => break,
                ' ' => continue,
                _ => return (Err(TokenError::TokenNotSupported(token)), history),
            }
        }
    }

    if let Ok(number) = current_number.parse::<f64>() {
        tokenized.push(Instruction::Number(number));
        history.push(Instruction::Number(number));
        current_number.clear();
    }

    if parenthesis != 0 {
        return (Err(TokenError::MissingParenthesis), history);
    }

    tokenized.extend(instruction_stack.drain(..).rev());

    (Ok(tokenized), history)
}

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
    }
}
