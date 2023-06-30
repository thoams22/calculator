use crate::tokenizer::{Instruction, Function};

#[derive(PartialEq, Clone, Debug)]
pub enum EvalError {
    InsufficientOperand,
    InsufficientNumber,
    UnevaluableToken,
}

pub fn evaluation(tokens: Vec<Instruction>) -> Result<f64, EvalError> {
    let mut stack = Vec::new();

    for token in tokens {
        match token {
            Instruction::Number(number) => stack.push(number),
            Instruction::Addition => {
                let y = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x + y);
            }
            Instruction::Substraction => {
                let y = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x - y);
            }
            Instruction::Multiplication => {
                let y = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x * y);
            }
            Instruction::Division => {
                let y = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x / y);
            }
            Instruction::Exponentiation => {
                let y = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.powf(y));
            }
            Instruction::Function(Function::Ln) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.ln());
            }

            _ => return Err(EvalError::UnevaluableToken),
        }
    }

    let result = stack.pop();

    if !stack.is_empty() {
        return Err(EvalError::InsufficientOperand);
    }

    result.ok_or(EvalError::InsufficientOperand)
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::{evaluation, EvalError},
        tokenizer::{Instruction, Function, tokenization},
    };

    fn test_evaluate(expression: &str) -> Result<f64, EvalError> {
        if let Ok(tokenized) = tokenization(&expression) {
            evaluation(tokenized)
        }
        else {
            Err(EvalError::InsufficientNumber)
        }
    }

    #[test]
    fn number() {
        assert_eq!(
            Err(EvalError::InsufficientNumber),
            evaluation(vec![
                Instruction::Number(22.0),
                Instruction::Number(33.0),
                Instruction::Multiplication,
                Instruction::Addition,
            ])
        );
        assert_eq!(
            Err(EvalError::InsufficientNumber),
            evaluation(vec![Instruction::Multiplication,])
        );
        assert_eq!(
            Err(EvalError::InsufficientNumber),
            evaluation(vec![Instruction::Multiplication, Instruction::Addition,])
        );
    }

    #[test]
    fn operand() {
        assert_eq!(
            Err(EvalError::InsufficientOperand),
            evaluation(vec![Instruction::Number(22.0), Instruction::Number(33.0),])
        );
    }

    #[test]
    fn ok() {
        assert_eq!(
            Ok(990.0),
            test_evaluate("22*45"),
            "Ok 1"
        );
        assert_eq!(
            Ok(1474.0),
            test_evaluate("22+33*44"),
            "Ok 2"
        );
        assert_eq!(
            Ok(1.8446744073709552e19),
            test_evaluate("2^4^3"),
            "Ok 3"
        );
        assert_eq!(
            Ok(16777216.0),
            test_evaluate("(-2^4)^6"),
            "Ok 4"
        );
        assert_eq!(
            Ok(0.0),
            test_evaluate("ln(4-3)"),
            "Ok 5"
        );
        assert_eq!(
            Ok(1.0),
            test_evaluate("ln(4-3)4ln(8-7)+1"),
            "Ok 6"
        );
        assert_eq!(
            Ok(-98.0),
            test_evaluate("4-3*4*8-7+1"),
            "Ok 7"
        );
    }
}
