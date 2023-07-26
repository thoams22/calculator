use crate::expression::{
    constant::{is_constant, Constant, Constants},
    function::{is_function, Functions},
    Expression,
};

#[derive(PartialEq, Clone, Debug)]
pub enum CalcError {
    TokenNotSupported(char),
    MissingParenthesis,
    BadMinusCase,
    BadFloatParsing,
    BadVariableParsing,
    BadInstructionToExpressionConvertion,
    UnevaluableToken,
    MissingExpression,
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
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
    Constant(Constants),
    Comma,
}

enum FunctionOrConstant {
    Function(Functions),
    Constant(Constants),
}

fn instruction_to_expression(
    last_instruction: &Instruction,
    tokenised: &mut Vec<Expression>,
) -> Result<Expression, CalcError> {
    match last_instruction {
        Instruction::Addition => {
            let y = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            let x = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            Ok(Expression::addition(x, y))
        }
        Instruction::Subtraction => {
            let y = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            let x = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            Ok(Expression::subtraction(x, y))
        }
        Instruction::Multiplication => {
            let y = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            let x = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            Ok(Expression::multiplication(x, y))
        }
        Instruction::Division => {
            let y = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            let x = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            Ok(Expression::division(x, y))
        }
        Instruction::Exponentiation => {
            let y = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            let x = tokenised.pop().ok_or(CalcError::MissingExpression)?;
            Ok(Expression::exponentiation(x, y))
        }
        Instruction::Function(fun) => match fun {
            Functions::Ln => Ok(Expression::ln(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Log2 => Ok(Expression::log2(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Log10 => Ok(Expression::log10(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Log => {
                let y = tokenised.pop().ok_or(CalcError::MissingExpression)?;
                let x = tokenised.pop().ok_or(CalcError::MissingExpression)?;
                Ok(Expression::log(x, y))
            }
            Functions::Sqrt => Ok(Expression::sqrt(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Sin => Ok(Expression::sin(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Cos => Ok(Expression::cos(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Tan => Ok(Expression::tan(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Asin => Ok(Expression::asin(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Acos => Ok(Expression::acos(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
            Functions::Atan => Ok(Expression::atan(
                tokenised.pop().ok_or(CalcError::MissingExpression)?,
            )),
        },
        Instruction::Number(num) => Ok(Expression::number(*num)),
        Instruction::Variable(var) => Ok(Expression::variable(*var)),
        Instruction::Constant(constant) => {
            Ok(Expression::Constant(Constant::from_constants(*constant)))
        }
        _ => Err(CalcError::BadInstructionToExpressionConvertion),
    }
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

fn get_function_or_constant(name: &str) -> Option<FunctionOrConstant> {
    if let Some(function) = is_function(name) {
        Some(FunctionOrConstant::Function(function))
    } else {
        is_constant(name).map(FunctionOrConstant::Constant)
    }
}

fn try_parse_f64(
    current_number: &mut String,
    instruction_stack: &mut Vec<Instruction>,
    history: &mut Vec<Instruction>,
    tokenized: &mut Vec<Expression>,
) -> Result<(), CalcError> {
    match current_number.parse::<f64>() {
        Ok(number) => {
            tokenized.push(Expression::Number(number));
            if let Some(&Instruction::ImplicitMultiplication) = history.last() {
                let y = tokenized.pop().ok_or(CalcError::MissingExpression)?;
                let x = tokenized.pop().ok_or(CalcError::MissingExpression)?;
                instruction_stack.pop();
                tokenized.push(Expression::multiplication(x, y))
            }
            history.push(Instruction::Number(number));
            current_number.clear();
            Ok(())
        }
        Err(_) => Err(CalcError::BadFloatParsing),
    }
}

fn try_parse_variable(
    current_function: &mut String,
    instruction_stack: &mut Vec<Instruction>,
    mut history: Vec<Instruction>,
    mut tokenized: Vec<Expression>,
) -> (Result<Vec<Expression>, CalcError>, Vec<Instruction>) {
    let binding = current_function.clone();
    for token in binding.chars() {
        // check if current_function is a function
        if let Some(function) = get_function_or_constant(current_function.trim_end_matches('(')) {
            // Handle implicit multiplication if needed
            match handle_implicit_multiplication(instruction_stack, tokenized, history) {
                (Ok(retured_tokenized), retured_history) => {
                    tokenized = retured_tokenized;
                    history = retured_history;
                }
                (Err(error), history) => return (Err(error), history),
            }

            // Push the function or constant
            match function {
                FunctionOrConstant::Function(func) => {
                    instruction_stack.push(Instruction::Function(func));
                    instruction_stack.push(Instruction::LeftParenthesis);
                    history.push(Instruction::Function(func));
                    history.push(Instruction::LeftParenthesis);
                }
                FunctionOrConstant::Constant(constant) => {
                    tokenized.push(Expression::Constant(Constant::from_constants(constant)));
                    history.push(Instruction::Constant(constant));
                }
            }
            current_function.clear();
            // return because when funtion found we are a the end of current_function
            return (Ok(tokenized), history);
        }
        match token {
            '(' => {
                // check for implicit multiplication
                match handle_implicit_multiplication(instruction_stack, tokenized, history) {
                    (Ok(retured_tokenized), retured_history) => {
                        tokenized = retured_tokenized;
                        history = retured_history;
                    }
                    (Err(error), history) => return (Err(error), history),
                }
                // push parenthesis because we are a the end of current_function
                instruction_stack.push(Instruction::LeftParenthesis);
                history.push(Instruction::LeftParenthesis);
            }
            c if c.is_ascii_alphabetic() => {
                // check for implicit multiplication
                match handle_implicit_multiplication(instruction_stack, tokenized, history) {
                    (Ok(retured_tokenized), retured_history) => {
                        tokenized = retured_tokenized;
                        history = retured_history;
                    }
                    (Err(error), history) => return (Err(error), history),
                }
                // push the variable
                tokenized.push(Expression::Variable(c));
                history.push(Instruction::Variable(c));
            }
            _ => return (Err(CalcError::BadVariableParsing), history),
        }
        // remove the first char of current_function for the next is_function() check
        current_function.remove(0);
    }
    current_function.clear();
    (Ok(tokenized), history)
}

fn handle_implicit_multiplication(
    instruction_stack: &mut Vec<Instruction>,
    mut tokenized: Vec<Expression>,
    mut history: Vec<Instruction>,
) -> (Result<Vec<Expression>, CalcError>, Vec<Instruction>) {
    if let Some(Instruction::Number(_))
    | Some(Instruction::Variable(_))
    | Some(Instruction::RightParenthesis)
    | Some(Instruction::Constant(_)) = history.last()
    {
        if let Some(last_instruction) = instruction_stack.last() {
            if precedence(last_instruction) >= 3 {
                match instruction_to_expression(last_instruction, &mut tokenized) {
                    Ok(expr) => {
                        tokenized.push(expr);
                        instruction_stack.pop();
                    }
                    Err(error) => return (Err(error), history),
                }
            }
        }
        instruction_stack.push(Instruction::Multiplication);
        history.push(Instruction::ImplicitMultiplication);
    }
    (Ok(tokenized), history)
}

pub fn tokenization(expression: &str) -> (Result<Expression, CalcError>, Vec<Instruction>) {
    // variable for the shunting-yard algorithm
    let mut tokenized: Vec<Expression> = Vec::new();
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
                        match instruction_to_expression(last_instruction, &mut tokenized) {
                            Ok(expr) => {
                                tokenized.push(expr);
                                instruction_stack.pop();
                            }
                            Err(error) => return (Err(error), history),
                        }
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
                if let Err(error) = try_parse_f64(
                    &mut current_number,
                    &mut instruction_stack,
                    &mut history,
                    &mut tokenized,
                ) {
                    return (Err(error), history);
                }
            }
            current_function.push(token);
        } else {
            // check if this was a number before and it is not yet push to tokenized
            if !current_number.is_empty() && current_number != "-" {
                if let Err(error) = try_parse_f64(
                    &mut current_number,
                    &mut instruction_stack,
                    &mut history,
                    &mut tokenized,
                ) {
                    return (Err(error), history);
                }
            }
            // check for variables
            if !current_function.is_empty() && token != '(' {
                if current_number == "-" {
                    tokenized.push(Expression::Number(-1.0));
                    if let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 3 {
                            match instruction_to_expression(last_instruction, &mut tokenized) {
                                Ok(expr) => {
                                    tokenized.push(expr);
                                    instruction_stack.pop();
                                }
                                Err(error) => return (Err(error), history),
                            }
                        }
                    }
                    instruction_stack.push(Instruction::Multiplication);
                    current_number.clear();
                }

                match try_parse_variable(
                    &mut current_function,
                    &mut instruction_stack,
                    history,
                    tokenized,
                ) {
                    (Ok(retured_tokenized), retured_history) => {
                        tokenized = retured_tokenized;
                        history = retured_history;
                    }
                    (Err(error), history) => return (Err(error), history),
                }
            }
            match token {
                '/' => {
                    while let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 3 {
                            match instruction_to_expression(last_instruction, &mut tokenized) {
                                Ok(expr) => {
                                    tokenized.push(expr);
                                    instruction_stack.pop();
                                }
                                Err(error) => return (Err(error), history),
                            }
                        } else {
                            break;
                        }
                    }

                    history.push(Instruction::Division);
                    instruction_stack.push(Instruction::Division);
                }
                '*' => {
                    while let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 3 {
                            match instruction_to_expression(last_instruction, &mut tokenized) {
                                Ok(expr) => {
                                    tokenized.push(expr);
                                    instruction_stack.pop();
                                }
                                Err(error) => return (Err(error), history),
                            }
                        } else {
                            break;
                        }
                    }
                    history.push(Instruction::Multiplication);
                    instruction_stack.push(Instruction::Multiplication);
                }
                '+' => {
                    while let Some(last_instruction) = instruction_stack.last() {
                        if precedence(last_instruction) >= 2 {
                            match instruction_to_expression(last_instruction, &mut tokenized) {
                                Ok(expr) => {
                                    tokenized.push(expr);
                                    instruction_stack.pop();
                                }
                                Err(error) => return (Err(error), history),
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
                            while let Some(last_instruction) = instruction_stack.last() {
                                if precedence(last_instruction) >= 2 {
                                    match instruction_to_expression(
                                        last_instruction,
                                        &mut tokenized,
                                    ) {
                                        Ok(expr) => {
                                            tokenized.push(expr);
                                            instruction_stack.pop();
                                        }
                                        Err(error) => return (Err(error), history),
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
                            tokenized.push(Expression::Number(-1.0));
                            if let Some(last_instruction) = instruction_stack.last() {
                                if precedence(last_instruction) >= 3 {
                                    match instruction_to_expression(
                                        last_instruction,
                                        &mut tokenized,
                                    ) {
                                        Ok(expr) => {
                                            tokenized.push(expr);
                                            instruction_stack.pop();
                                        }
                                        Err(error) => return (Err(error), history),
                                    }
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
                            history,
                            tokenized,
                        ) {
                            (Ok(retured_tokenized), retured_history) => {
                                tokenized = retured_tokenized;
                                history = retured_history;
                            }
                            (Err(error), history) => return (Err(error), history),
                        }

                        current_function.clear();
                        continue;
                    }
                    // if ( is preceded by a number add implicit multiplication
                    else {
                        match handle_implicit_multiplication(
                            &mut instruction_stack,
                            tokenized,
                            history,
                        ) {
                            (Ok(retured_tokenized), retured_history) => {
                                tokenized = retured_tokenized;
                                history = retured_history;
                            }
                            (Err(error), history) => return (Err(error), history),
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
                                match instruction_to_expression(last_instruction, &mut tokenized) {
                                    Ok(expr) => {
                                        tokenized.push(expr);
                                        instruction_stack.pop();
                                    }
                                    Err(error) => return (Err(error), history),
                                }
                            } else {
                                // tokenized.push(Instruction::RightParenthesis);
                                instruction_stack.pop();
                                if let Some(function) = instruction_stack.last() {
                                    if let &Instruction::Function(_) = function {
                                        match instruction_to_expression(function, &mut tokenized) {
                                            Ok(expr) => {
                                                tokenized.push(expr);
                                                instruction_stack.pop();
                                            }
                                            Err(error) => return (Err(error), history),
                                        }
                                    }
                                }
                                break;
                            }
                        } else {
                            return (Err(CalcError::MissingParenthesis), history);
                        }
                    }
                }
                // for log(x, y)
                ',' => {
                    loop {
                        if let Some(last_instruction) = instruction_stack.last() {
                            if last_instruction != &Instruction::LeftParenthesis{
                                match instruction_to_expression(last_instruction, &mut tokenized) {
                                    Ok(expr) => {
                                        tokenized.push(expr);
                                        instruction_stack.pop();
                                    }
                                    Err(error) => return (Err(error), history),
                                }
                            } else {
                                break;
                            }
                        } else {
                            return (Err(CalcError::MissingParenthesis), history);
                        }
                    }
                    history.push(Instruction::Comma);
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
            tokenized.push(Expression::Number(-1.0));
            if let Some(last_instruction) = instruction_stack.last() {
                if precedence(last_instruction) >= 3 {
                    match instruction_to_expression(last_instruction, &mut tokenized) {
                        Ok(expr) => {
                            tokenized.push(expr);
                            instruction_stack.pop();
                        }
                        Err(error) => return (Err(error), history),
                    }
                }
            }
            instruction_stack.push(Instruction::Multiplication);
            current_number.clear();
        }

        match try_parse_variable(
            &mut current_function,
            &mut instruction_stack,
            history,
            tokenized,
        ) {
            (Ok(retured_tokenized), retured_history) => {
                tokenized = retured_tokenized;
                history = retured_history;
            }
            (Err(error), history) => return (Err(error), history),
        }
    }

    // if the last token is a number parse it or Error
    if !current_number.is_empty() {
        if let Err(error) = try_parse_f64(
            &mut current_number,
            &mut instruction_stack,
            &mut history,
            &mut tokenized,
        ) {
            return (Err(error), history);
        }
    }
    // check if there was a pair number of parenthesis or Error
    if parenthesis != 0 {
        return (Err(CalcError::MissingParenthesis), history);
    }

    // pop instruction_stack in tokenized if ther was Operation left inside
    instruction_stack.drain(..).rev().for_each(|instru| {
        match instruction_to_expression(&instru, &mut tokenized) {
            Ok(expr) => tokenized.push(expr),

            Err(_error) => {}
        }
    });

    (Ok(tokenized[0].clone()), history)
}

#[cfg(test)]
mod tests_tokenization {
    use crate::{
        expression::Expression,
        tokenizer::{tokenization, CalcError, Instruction},
    };

    #[test]
    fn blank() {
        let result: (Result<Expression, CalcError>, Vec<Instruction>) =
            tokenization(" 2* 3   / 4         +4 -9");
        assert_eq!(
            Ok(Expression::subtraction(
                Expression::addition(
                    Expression::division(
                        Expression::multiplication(
                            Expression::Number(2.0),
                            Expression::Number(3.0)
                        ),
                        Expression::Number(4.0)
                    ),
                    Expression::Number(4.0)
                ),
                Expression::Number(9.0)
            )),
            result.0,
            "blank 1,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn negative_number() {
        let mut result = tokenization("-2+4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(-2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "negative_number 1,\n history = {:?}",
            result.1
        );
        result = tokenization("2-4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(2.0),
                Expression::multiplication(Expression::Number(-1.0), Expression::Number(4.0))
            )),
            result.0,
            "negative_number 2,\n history = {:?}",
            result.1
        );

        result = tokenization("-2-4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(-2.0),
                Expression::multiplication(Expression::Number(-1.0), Expression::Number(4.0))
            )),
            result.0,
            "negative_number 3,\nhistory = {:?}",
            result.1
        );
        result = tokenization("-2--4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(-2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "negative_number 4,\n history = {:?}",
            result.1
        );
        result = tokenization("--2+4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "negative_number 5,\n history = {:?}",
            result.1
        );
        result = tokenization("---2+4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(-2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "negative_number 6,\n history = {:?}",
            result.1
        );
        result = tokenization("-2--4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(-2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "negative_number 7,\n history = {:?}",
            result.1
        );
        result = tokenization("-2---4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(-2.0),
                Expression::multiplication(Expression::Number(-1.0), Expression::Number(4.0))
            )),
            result.0,
            "negative_number 8,\n history = {:?}",
            result.1
        );
        result = tokenization("-2/-4");
        assert_eq!(
            Ok(Expression::division(
                Expression::Number(-2.0),
                Expression::Number(-4.0)
            )),
            result.0,
            "negative_number 9,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*-4");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-2.0),
                Expression::Number(-4.0)
            )),
            result.0,
            "negative_number 10,\n history = {:?}",
            result.1
        );

        result = tokenization("-2+-4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(-2.0),
                Expression::Number(-4.0)
            )),
            result.0,
            "negative_number 11,\n history = {:?}",
            result.1
        );

        result = tokenization("--2/--4");
        assert_eq!(
            Ok(Expression::division(
                Expression::Number(2.0),
                Expression::Number(4.0),
            )),
            result.0,
            "negative_number 12,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*--4");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "negative_number 13,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*(--4/2-2)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-2.0),
                Expression::addition(
                    Expression::division(Expression::Number(4.0), Expression::Number(2.0)),
                    Expression::multiplication(Expression::Number(-1.0), Expression::Number(2.0))
                )
            )),
            result.0,
            "negative_number 14,\n history = {:?}",
            result.1
        );
        result = tokenization("-2*(--4/-2--2)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-2.0),
                Expression::addition(
                    Expression::division(Expression::Number(4.0), Expression::Number(-2.0)),
                    Expression::Number(2.0)
                )
            )),
            result.0,
            "negative_number 15,\n history = {:?}",
            result.1
        );
        result = tokenization("--2^---4");
        assert_eq!(
            Ok(Expression::exponentiation(
                Expression::Number(2.0),
                Expression::Number(-4.0)
            )),
            result.0,
            "negative_number 16,\n history = {:?}",
            result.1
        );
        result = tokenization("2(-4)-8");
        assert_eq!(
            Ok(Expression::addition(
                Expression::multiplication(Expression::Number(2.0), Expression::Number(-4.0)),
                Expression::multiplication(Expression::Number(-1.0), Expression::Number(8.0))
            )),
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
            Ok(Expression::addition(
                Expression::multiplication(
                    Expression::Number(2.0),
                    Expression::addition(Expression::Number(3.0), Expression::Number(4.0))
                ),
                Expression::Number(4.0)
            )),
            result.0,
            "parenthesis 1,\n history = {:?}",
            result.1
        );
        result = tokenization("2+((3*4)+4)*6");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(2.0),
                Expression::multiplication(
                    Expression::addition(
                        Expression::multiplication(
                            Expression::Number(3.0),
                            Expression::Number(4.0)
                        ),
                        Expression::Number(4.0)
                    ),
                    Expression::Number(6.0)
                )
            ),),
            result.0,
            "parenthesis 2,\n history = {:?}",
            result.1
        );
        result = tokenization("2+(4)*6");
        assert_eq!(
            Ok(Expression::addition(
                Expression::Number(2.0),
                Expression::multiplication(Expression::Number(4.0), Expression::Number(6.0)),
            )),
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
            Ok(Expression::multiplication(
                Expression::addition(
                    Expression::multiplication(Expression::Number(2.0), Expression::Number(4.0)),
                    Expression::Number(6.0)
                ),
                Expression::addition(
                    Expression::multiplication(Expression::Number(2.0), Expression::Number(4.0)),
                    Expression::Number(6.0)
                ),
            )),
            result.0,
            "parenthesis 6,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn exponentiation() {
        let mut result = tokenization("2^4");
        assert_eq!(
            Ok(Expression::exponentiation(
                Expression::Number(2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "exponentiation 1,\n history = {:?}",
            result.1
        );
        result = tokenization("2^-4");
        assert_eq!(
            Ok(Expression::exponentiation(
                Expression::Number(2.0),
                Expression::Number(-4.0)
            )),
            result.0,
            "exponentiation 2,\n history = {:?}",
            result.1
        );
        result = tokenization("-2^--4");
        assert_eq!(
            Ok(Expression::exponentiation(
                Expression::Number(-2.0),
                Expression::Number(4.0)
            )),
            result.0,
            "exponentiation 3,\n history = {:?}",
            result.1
        );
        result = tokenization("-2^4^6");
        assert_eq!(
            Ok(Expression::exponentiation(
                Expression::Number(-2.0),
                Expression::exponentiation(Expression::Number(4.0), Expression::Number(6.0)),
            )),
            result.0,
            "exponentiation 4,\n history = {:?}",
            result.1
        );
        result = tokenization("(-2^4)^6");
        assert_eq!(
            Ok(Expression::exponentiation(
                Expression::exponentiation(Expression::Number(-2.0), Expression::Number(4.0)),
                Expression::Number(6.0)
            )),
            result.0,
            "exponentiation 5,\n history = {:?}",
            result.1
        );
        result = tokenization("(-2+4)^6");
        assert_eq!(
            Ok(Expression::exponentiation(
                Expression::addition(Expression::Number(-2.0), Expression::Number(4.0)),
                Expression::Number(6.0)
            )),
            result.0,
            "exponentiation 6,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn implicit_multiplication() {
        let mut result = tokenization("2(3+4)+4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::multiplication(
                    Expression::Number(2.0),
                    Expression::addition(Expression::Number(3.0), Expression::Number(4.0)),
                ),
                Expression::Number(4.0),
            )),
            result.0,
            "implicit_multiplication 1,\n history = {:?}",
            result.1
        );
        result = tokenization("(3+4)4");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::addition(Expression::Number(3.0), Expression::Number(4.0)),
                Expression::Number(4.0)
            )),
            result.0,
            "implicit_multiplication 2,\n history = {:?}",
            result.1
        );
        result = tokenization("2*2(3(4^2)4)5");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(2.0), Expression::Number(2.0)),
                    Expression::multiplication(
                        Expression::multiplication(
                            Expression::Number(3.0),
                            Expression::exponentiation(
                                Expression::Number(4.0),
                                Expression::Number(2.0)
                            )
                        ),
                        Expression::Number(4.0)
                    )
                ),
                Expression::Number(5.0)
            )),
            result.0,
            "implicit_multiplication 3,\n history = {:?}",
            result.1
        );
        result = tokenization("2(3+4)4+3");
        assert_eq!(
            Ok(Expression::addition(
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::Number(2.0),
                        Expression::addition(Expression::Number(3.0), Expression::Number(4.0))
                    ),
                    Expression::Number(4.0)
                ),
                Expression::Number(3.0)
            )),
            result.0,
            "implicit_multiplication 4,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn function() {
        let mut result = tokenization("ln(4-3)");
        assert_eq!(
            Ok(Expression::ln(Expression::addition(
                Expression::Number(4.0),
                Expression::multiplication(Expression::Number(-1.0), Expression::Number(3.0)),
            ))),
            result.0,
            "function 1,\n history = {:?}",
            result.1
        );
        result = tokenization("4ln(4-3)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::number(4.0),
                Expression::ln(Expression::addition(
                    Expression::Number(4.0),
                    Expression::multiplication(Expression::Number(-1.0), Expression::Number(3.0)),
                )),
            )),
            result.0,
            "function 2,\n history = {:?}",
            result.1
        );
        result = tokenization("ln(4-3)4");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::ln(Expression::addition(
                    Expression::Number(4.0),
                    Expression::multiplication(Expression::Number(-1.0), Expression::Number(3.0)),
                )),
                Expression::number(4.0),
            )),
            result.0,
            "function 3,\n history = {:?}",
            result.1
        );

        result = tokenization("ln(4-3)4ln(8-5)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(
                    Expression::ln(Expression::addition(
                        Expression::Number(4.0),
                        Expression::multiplication(
                            Expression::Number(-1.0),
                            Expression::Number(3.0)
                        )
                    )),
                    Expression::Number(4.0)
                ),
                Expression::ln(Expression::addition(
                    Expression::Number(8.0),
                    Expression::multiplication(Expression::Number(-1.0), Expression::Number(5.0))
                ))
            )),
            result.0,
            "function 4,\n history = {:?}",
            result.1
        );
        result = tokenization("4ln(4-3)--8--7");
        assert_eq!(
            Ok(Expression::addition(
                Expression::addition(
                    Expression::multiplication(
                        Expression::Number(4.0),
                        Expression::ln(Expression::addition(
                            Expression::Number(4.0),
                            Expression::multiplication(
                                Expression::Number(-1.0),
                                Expression::Number(3.0)
                            )
                        ))
                    ),
                    Expression::Number(8.0)
                ),
                Expression::Number(7.0)
            )),
            result.0,
            "function 5,\n history = {:?}",
            result.1
        );
        result = tokenization("log2(4)");
        assert_eq!(
            Ok(Expression::log2(Expression::Number(4.0))),
            result.0,
            "function 6,\n history = {:?}",
            result.1
        );
        result = tokenization("log10(4)");
        assert_eq!(
            Ok(Expression::log10(Expression::Number(4.0))),
            result.0,
            "function 7,\n history = {:?}",
            result.1
        );
        result = tokenization("log(3, 4)");
        assert_eq!(
            Ok(Expression::log(
                Expression::Number(3.0),
                Expression::Number(4.0)
            )),
            result.0,
            "function 8,\n history = {:?}",
            result.1
        );
        result = tokenization("sin(0)");
        assert_eq!(
            Ok(Expression::sin(Expression::Number(0.0))),
            result.0,
            "function 9,\n history = {:?}",
            result.1
        );
        result = tokenization("cos(0)");
        assert_eq!(
            Ok(Expression::cos(Expression::Number(0.0))),
            result.0,
            "function 10,\n history = {:?}",
            result.1
        );
        result = tokenization("tan(0)");
        assert_eq!(
            Ok(Expression::tan(Expression::Number(0.0))),
            result.0,
            "function 11,\n history = {:?}",
            result.1
        );
        result = tokenization("sqrt(4)");
        assert_eq!(
            Ok(Expression::sqrt(Expression::Number(4.0))),
            result.0,
            "function 12,\n history = {:?}",
            result.1
        );
        result = tokenization("asin(0)");
        assert_eq!(
            Ok(Expression::asin(Expression::Number(0.0))),
            result.0,
            "function 13,\n history = {:?}",
            result.1
        );
        result = tokenization("acos(0)");
        assert_eq!(
            Ok(Expression::acos(Expression::Number(0.0))),
            result.0,
            "function 14,\n history = {:?}",
            result.1
        );
        result = tokenization("atan(0)");
        assert_eq!(
            Ok(Expression::atan(Expression::Number(0.0))),
            result.0,
            "function 15,\n history = {:?}",
            result.1
        );

        result = tokenization("-sqrt(4)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-1.0),
                Expression::sqrt(Expression::Number(4.0))
            )),
            result.0,
            "function 16,\n history = {:?}",
            result.1
        );

        result = tokenization("-1sqrt(4)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-1.0),
                Expression::sqrt(Expression::Number(4.0))
            )),
            result.0,
            "function 17,\n history = {:?}",
            result.1
        );

        result = tokenization("2*-sqrt(4)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(Expression::Number(2.0), Expression::Number(-1.0),),
                Expression::sqrt(Expression::Number(4.0))
            )),
            result.0,
            "function 18,\n history = {:?}",
            result.1
        );

        result = tokenization("2*-sqrt(4)*2^2");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(2.0), Expression::Number(-1.0)),
                    Expression::sqrt(Expression::Number(4.0))
                ),
                Expression::exponentiation(Expression::Number(2.0), Expression::Number(2.0))
            )),
            result.0,
            "function 19,\n history = {:?}",
            result.1
        );

        result = tokenization("2*--sqrt(4)*2^2");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(
                    Expression::Number(2.0),
                    Expression::sqrt(Expression::Number(4.0))
                ),
                Expression::exponentiation(Expression::Number(2.0), Expression::Number(2.0))
            )),
            result.0,
            "function 20,\n history = {:?}",
            result.1
        );

        result = tokenization("2*---sqrt(4)*2^-2");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(2.0), Expression::Number(-1.0)),
                    Expression::sqrt(Expression::Number(4.0))
                ),
                Expression::exponentiation(Expression::Number(2.0), Expression::Number(-2.0))
            )),
            result.0,
            "function 21,\n history = {:?}",
            result.1
        );

        result = tokenization("2sqrt(4)sqrt(4)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(
                    Expression::Number(2.0),
                    Expression::sqrt(Expression::Number(4.0))
                ),
                Expression::sqrt(Expression::Number(4.0))
            )),
            result.0,
            "function 22,\n history = {:?}",
            result.1
        );

        result = tokenization("2sqrt(4)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(2.0),
                Expression::sqrt(Expression::Number(4.0))
            )),
            result.0,
            "function 23,\n history = {:?}",
            result.1
        );

        result = tokenization("-sqrt(4)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-1.0),
                Expression::sqrt(Expression::Number(4.0))
            )),
            result.0,
            "function 24,\n history = {:?}",
            result.1
        );
    }

    #[test]
    fn variable() {
        let mut result = tokenization("2a");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(2.0),
                Expression::Variable('a')
            )),
            result.0,
            "variable 1, \n history = {:?}",
            result.1
        );

        result = tokenization("2a+4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::multiplication(Expression::Number(2.0), Expression::Variable('a')),
                Expression::Number(4.0)
            )),
            result.0,
            "variable 2, \n history = {:?}",
            result.1
        );

        result = tokenization("2a(5)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(Expression::Number(2.0), Expression::Variable('a')),
                Expression::Number(5.0),
            ),),
            result.0,
            "variable 3, \n history = {:?}",
            result.1
        );

        result = tokenization("2asin(5)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(2.0),
                Expression::asin(Expression::Number(5.0))
            )),
            result.0,
            "variable 4, \n history = {:?}",
            result.1
        );

        result = tokenization("2Asin(5)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(Expression::Number(2.0), Expression::Variable('A')),
                Expression::sin(Expression::Number(5.0))
            )),
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
            Ok(Expression::multiplication(
                Expression::multiplication(Expression::Variable('a'), Expression::Variable('b')),
                Expression::Variable('c')
            )),
            result.0,
            "variable 8, \n history = {:?}",
            result.1
        );

        result = tokenization("a*b*c");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(Expression::Variable('a'), Expression::Variable('b')),
                Expression::Variable('c')
            )),
            result.0,
            "variable 9, \n history = {:?}",
            result.1
        );

        result = tokenization("abc(abc)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::Variable('a'),
                        Expression::Variable('b')
                    ),
                    Expression::Variable('c')
                ),
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::Variable('a'),
                        Expression::Variable('b')
                    ),
                    Expression::Variable('c')
                )
            )),
            result.0,
            "variable 10, \n history = {:?}",
            result.1
        );

        result = tokenization("(a-b)(a+b)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::subtraction(Expression::Variable('a'), Expression::Variable('b')),
                Expression::addition(Expression::Variable('a'), Expression::Variable('b'))
            )),
            result.0,
            "variable 11, \n history = {:?}",
            result.1
        );

        result = tokenization("(a-b)(a+-b)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::subtraction(Expression::Variable('a'), Expression::Variable('b')),
                Expression::addition(
                    Expression::Variable('a'),
                    Expression::multiplication(Expression::Number(-1.0), Expression::Variable('b'))
                )
            )),
            result.0,
            "variable 12, \n history = {:?}",
            result.1
        );

        result = tokenization("-a");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::Number(-1.0),
                Expression::Variable('a')
            )),
            result.0,
            "variable 13, \n history = {:?}",
            result.1
        );

        result = tokenization("2*-a");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(Expression::Number(2.0), Expression::Number(-1.0)),
                Expression::Variable('a')
            )),
            result.0,
            "variable 14, \n history = {:?}",
            result.1
        );

        result = tokenization("a*-a+a*-a");
        assert_eq!(
            Ok(Expression::addition(
                Expression::multiplication(
                    Expression::multiplication(Expression::Variable('a'), Expression::Number(-1.0),),
                    Expression::Variable('a')
                ),
                Expression::multiplication(
                    Expression::multiplication(Expression::Variable('a'), Expression::Number(-1.0),),
                    Expression::Variable('a')
                )
            )),
            result.0,
            "variable 15, \n history = {:?}",
            result.1
        );
    }

    #[test]
    fn operation_priority() {
        let mut result = tokenization("16a^2+16a+4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::addition(
                    Expression::multiplication(
                        Expression::Number(16.0),
                        Expression::exponentiation(
                            Expression::variable('a'),
                            Expression::number(2.0)
                        )
                    ),
                    Expression::multiplication(Expression::Number(16.0), Expression::variable('a'))
                ),
                Expression::Number(4.0)
            )),
            result.0,
            "operation_priority 1 \n history = {:?}",
            result.1
        );

        result = tokenization("16*2^2+16*2+4");
        assert_eq!(
            Ok(Expression::addition(
                Expression::addition(
                    Expression::multiplication(
                        Expression::Number(16.0),
                        Expression::exponentiation(
                            Expression::number(2.0),
                            Expression::number(2.0)
                        )
                    ),
                    Expression::multiplication(Expression::Number(16.0), Expression::Number(2.0))
                ),
                Expression::Number(4.0)
            )),
            result.0,
            "operation_priority 2 \n history = {:?}",
            result.1
        );

        result = tokenization("16*2*3*3^2+5*4*4+5");
        assert_eq!(
            Ok(Expression::addition(
                Expression::addition(
                    Expression::multiplication(
                        Expression::multiplication(
                            Expression::multiplication(
                                Expression::Number(16.0),
                                Expression::Number(2.0)
                            ),
                            Expression::Number(3.0)
                        ),
                        Expression::exponentiation(
                            Expression::Number(3.0),
                            Expression::Number(2.0)
                        )
                    ),
                    Expression::multiplication(
                        Expression::multiplication(
                            Expression::Number(5.0),
                            Expression::Number(4.0)
                        ),
                        Expression::Number(4.0)
                    )
                ),
                Expression::Number(5.0)
            )),
            result.0,
            "operation_priority 3 \n history = {:?}",
            result.1
        );

        result = tokenization("16*abb^a+dcc+d");
        assert_eq!(
            Ok(Expression::addition(
                Expression::addition(
                    Expression::multiplication(
                        Expression::multiplication(
                            Expression::multiplication(
                                Expression::Number(16.0),
                                Expression::Variable('a')
                            ),
                            Expression::Variable('b')
                        ),
                        Expression::exponentiation(
                            Expression::Variable('b'),
                            Expression::Variable('a')
                        )
                    ),
                    Expression::multiplication(
                        Expression::multiplication(
                            Expression::Variable('d'),
                            Expression::Variable('c')
                        ),
                        Expression::Variable('c')
                    )
                ),
                Expression::Variable('d')
            )),
            result.0,
            "operation_priority 4 \n history = {:?}",
            result.1
        );

        result = tokenization("16*sqrt(4)log2(8)log2(8)^sqrt(4)+5*4*4+5");
        assert_eq!(
            Ok(Expression::addition(
                Expression::addition(
                    Expression::multiplication(
                        Expression::multiplication(
                            Expression::multiplication(
                                Expression::Number(16.0),
                                Expression::sqrt(Expression::Number(4.0))
                            ),
                            Expression::log2(Expression::Number(8.0))
                        ),
                        Expression::exponentiation(
                            Expression::log2(Expression::Number(8.0)),
                            Expression::sqrt(Expression::Number(4.0))
                        )
                    ),
                    Expression::multiplication(
                        Expression::multiplication(
                            Expression::Number(5.0),
                            Expression::Number(4.0)
                        ),
                        Expression::Number(4.0)
                    )
                ),
                Expression::Number(5.0)
            )),
            result.0,
            "operation_priority 5 \n history = {:?}",
            result.1
        );
    }

    #[test]
    fn constant() {
        let mut result = tokenization("api");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::variable('a'),
                Expression::pi()
            )),
            result.0,
            "constant 1\nhistory : {:?}",
            result.1
        );

        result = tokenization("a*pi");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::variable('a'),
                Expression::pi()
            )),
            result.0,
            "constant 2\nhistory : {:?}",
            result.1
        );

        result = tokenization("a*phi");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::variable('a'),
                Expression::phi()
            )),
            result.0,
            "constant 3\nhistory : {:?}",
            result.1
        );

        result = tokenization("a*e");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::variable('a'),
                Expression::e()
            )),
            result.0,
            "constant 4\nhistory : {:?}",
            result.1
        );

        result = tokenization("aln(e)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::variable('a'),
                Expression::ln(Expression::e())
            )),
            result.0,
            "constant 5\nhistory : {:?}",
            result.1
        );

        result = tokenization("eln(e)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::variable('e'),
                Expression::ln(Expression::e())
            )),
            result.0,
            "constant 6\nhistory : {:?}",
            result.1
        );

        result = tokenization("piln(e)");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::multiplication(Expression::variable('p'), Expression::variable('i'),),
                Expression::ln(Expression::e())
            )),
            result.0,
            "constant 7\nhistory : {:?}",
            result.1
        );

        result = tokenization("ln(e)e");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::ln(Expression::e()),
                Expression::e()
            )),
            result.0,
            "constant 6\nhistory : {:?}",
            result.1
        );

        result = tokenization("ln(e)pi");
        assert_eq!(
            Ok(Expression::multiplication(
                Expression::ln(Expression::e()),
                Expression::pi()
            )),
            result.0,
            "constant 7\nhistory : {:?}",
            result.1
        );
    }
}
