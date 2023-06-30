#[derive(PartialEq, Clone, Debug)]
pub enum TokenError {
    InsufficientChar,
    TokenNotSupported,
    FunctionNotSupported,
    MissingParenthesis,
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
        _ => Err(TokenError::FunctionNotSupported),
    }
}

pub fn tokenization(expression: &str) -> Result<Vec<Instruction>, TokenError> { //[Vec<Instruction>; 2]
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
            }
            current_number.push(token);
        } else if token.is_ascii_lowercase() {
            if let Ok(number) = current_number.parse::<f64>() {
                tokenized.push(Instruction::Number(number));
                history.push(Instruction::Number(number));
                current_number.clear();
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
            if let Ok(number) = current_number.parse::<f64>() {
                tokenized.push(Instruction::Number(number));
                history.push(Instruction::Number(number));
                current_number.clear();
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
                        (None, _) | (Some(Instruction::Addition), None) => {
                            current_number.push('-');
                            history.push(Instruction::Substraction);
                            continue;
                        }
                        (Some(&Instruction::Substraction), Some(&Instruction::Substraction)) => {
                            instruction_stack.pop();
                            current_number.pop();
                            instruction_stack.push(Instruction::Addition);
                            history.push(Instruction::Addition);
                            continue;
                        }
                        (Some(&Instruction::Substraction), _) => {
                            current_number.pop();
                            history.push(Instruction::Addition);
                            continue;
                        }
                        (Some(&Instruction::Addition), Some(&Instruction::Addition)) => {
                            instruction_stack.pop();
                        }
                        (Some(&Instruction::Division), _)
                        | (Some(&Instruction::Multiplication), _)
                        | (Some(&Instruction::ImplicitMultiplication), _)
                        | (Some(&Instruction::LeftParenthesis), _)
                        | (Some(&Instruction::Exponentiation), _) => {
                            current_number.push('-');
                            history.push(Instruction::Substraction);
                            continue;
                        }
                        (_, Some(last_instruction)) => {
                            if precedence(last_instruction) >= 2 {
                                tokenized.push(instruction_stack.pop().unwrap());
                            }
                        }
                        (_, _) => {}
                    };
                    history.push(Instruction::Substraction);
                    instruction_stack.push(Instruction::Substraction);
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
                    instruction_stack.push(Instruction::LeftParenthesis);
                    history.push(Instruction::LeftParenthesis);
                    if !current_function.is_empty() {
                        match is_function(&current_function) {
                            Ok(result) => {
                                instruction_stack.push(result);
                                history.push(result);
                            }
                            Err(error) => println!("Error finding function  {:?}", error),
                        }
                        current_function.clear();
                    } else if let Some(Instruction::Number(_)) = history.last() {
                        history.push(Instruction::ImplicitMultiplication);
                        instruction_stack.push(Instruction::Multiplication);
                    }
                    parenthesis += 1;
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
                                if let Some(Instruction::Function(_)) = instruction_stack.last() {
                                    tokenized.push(instruction_stack.pop().unwrap());
                                }
                                break;
                            }
                        } else {
                            return Err(TokenError::MissingParenthesis);
                        }
                    }
                }
                '\r' | '\n' => break,
                ' ' => continue,
                _ => return Err(TokenError::TokenNotSupported),
            }
        }
    }
    
    if let Ok(number) = current_number.parse::<f64>() {
        tokenized.push(Instruction::Number(number));
        history.push(Instruction::Number(number));
        current_number.clear();
    }
    // println!("{:?}", history);

    if parenthesis != 0 {
        return Err(TokenError::MissingParenthesis);
    }

    tokenized.extend(instruction_stack.drain(..).rev());
    

    if tokenized.len() < 3 {
        return Err(TokenError::InsufficientChar);
    }

    // Ok([tokenized, history])
    Ok(tokenized)
}

#[cfg(test)]
mod tests {

    use crate::tokenizer::{tokenization, Function, Instruction, TokenError};

    #[test]
    fn char() {
        assert_eq!(
            Err(TokenError::InsufficientChar),
            tokenization("1"),
            "char 1"
        );
        assert_eq!(
            Err(TokenError::InsufficientChar),
            tokenization("+"),
            "char 2"
        );
        assert_eq!(
            Err(TokenError::InsufficientChar),
            tokenization("2+"),
            "char 3"
        );
        assert_eq!(
            Err(TokenError::InsufficientChar),
            tokenization("-6"),
            "char 4"
        );
        assert_eq!(
            Err(TokenError::InsufficientChar),
            tokenization("2222-"),
            "char 5"
        );
    }

    #[test]
    fn blank() {
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
                Instruction::Substraction,
            ]),
            tokenization(" 2* 3   / 4         +4 -9")
        );
    }

    #[test]
    fn negative_number() {
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            tokenization("-2+4"),
            "negative_number 1"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Substraction,
            ]),
            tokenization("2-4"),
            "negative_number 2"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Substraction,
            ]),
            tokenization("-2-4"),
            "negative_number 3"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            tokenization("-2--4"),
            "negative_number 4"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            tokenization("--2+4"),
            "negative_number 5"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            tokenization("---2+4"),
            "negative_number 6"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
            ]),
            tokenization("-2--4"),
            "negative_number 7"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Substraction,
            ]),
            tokenization("-2---4"),
            "negative_number 8"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Division,
            ]),
            tokenization("-2/-4"),
            "negative_number 9"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(-4.0),
                Instruction::Multiplication,
            ]),
            tokenization("-2*-4"),
            "negative_number 10"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Substraction,
            ]),
            tokenization("-2+-4"),
            "negative_number 11"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Division,
            ]),
            tokenization("--2/--4"),
            "negative_number 12"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Multiplication,
            ]),
            tokenization("-2*--4"),
            "negative_number 13"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Number(2.0),
                Instruction::Division,
                Instruction::Number(2.0),
                Instruction::Substraction,
                Instruction::Multiplication,
            ]),
            tokenization("-2*(--4/2-2)"),
            "negative_number 14"
        );
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
            tokenization("-2*(--4/-2--2)"),
            "negative_number 15"
        );
    }

    #[test]
    fn unknown_token() {
        assert_eq!(
            Err(TokenError::TokenNotSupported),
            tokenization("@"),
            "unknown_token 1"
        );
        assert_eq!(
            Err(TokenError::TokenNotSupported),
            tokenization("#"),
            "unknown_token 2"
        );
    }

    #[test]
    fn parenthesis() {
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
            tokenization("2*(3+4)+4"),
            "parenthesis 1"
        );

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
            tokenization("2+((3*4)+4)*6"),
            "parenthesis 2"
        );

        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Number(6.0),
                Instruction::Multiplication,
                Instruction::Addition,
            ]),
            tokenization("2+(4)*6"),
            "parenthesis 3"
        );

        assert_eq!(
            Err(TokenError::MissingParenthesis),
            tokenization("2*(4+6"),
            "parenthesis 4"
        );
        assert_eq!(
            Err(TokenError::MissingParenthesis),
            tokenization("2*4+6)"),
            "parenthesis 5"
        );
    }

    #[test]
    fn exponentiation() {
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(4.0),
                Instruction::Exponentiation,
            ]),
            tokenization("2^4"),
            "exponentiation 1"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(2.0),
                Instruction::Number(-4.0),
                Instruction::Exponentiation,
            ]),
            tokenization("2^-4"),
            "exponentiation 2"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Exponentiation,
            ]),
            tokenization("-2^--4"),
            "exponentiation 3"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Number(6.0),
                Instruction::Exponentiation,
                Instruction::Exponentiation,
            ]),
            tokenization("-2^4^6"),
            "exponentiation 4"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Exponentiation,
                Instruction::Number(6.0),
                Instruction::Exponentiation,
            ]),
            tokenization("(-2^4)^6"),
            "exponentiation 5"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(-2.0),
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Number(6.0),
                Instruction::Exponentiation,
            ]),
            tokenization("(-2+4)^6"),
            "exponentiation 6"
        );
    }

    #[test]
    fn implicit_multiplication() {
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
            tokenization("2(3+4)+4"),
            "implicit_multiplication 1"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(3.0),
                Instruction::Number(4.0),
                Instruction::Addition,
                Instruction::Number(4.0),
                Instruction::Multiplication,
            ]),
            tokenization("(3+4)4"),
            "implicit_multiplication 2"
        );
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
            tokenization("2*2(3(4^2)4)5"),
            "implicit_multiplication 3"
        );
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
            tokenization("2(3+4)4+3"),
            "implicit_multiplication 4"
        );
    }

    #[test]
    fn function() {
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(3.0),
                Instruction::Substraction,
                Instruction::Function(Function::Ln),
            ]),
            tokenization("ln(4-3)"),
            "function 1"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(4.0),
                Instruction::Number(3.0),
                Instruction::Substraction,
                Instruction::Function(Function::Ln),
                Instruction::Multiplication
            ]),
            tokenization("4ln(4-3)"),
            "function 2"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(3.0),
                Instruction::Substraction,
                Instruction::Function(Function::Ln),
                Instruction::Number(4.0),
                Instruction::Multiplication,
            ]),
            tokenization("ln(4-3)4"),
            "function 3"
        );
        assert_eq!(
            Ok(vec![
                Instruction::Number(4.0),
                Instruction::Number(3.0),
                Instruction::Substraction,
                Instruction::Function(Function::Ln),
                Instruction::Number(4.0),
                Instruction::Multiplication,
                Instruction::Number(8.0),
                Instruction::Number(5.0),
                Instruction::Substraction,
                Instruction::Function(Function::Ln),
                Instruction::Multiplication
            ]),
            tokenization("ln(4-3)4ln(8-5)"),
            "function 4"
        );
    }
}
