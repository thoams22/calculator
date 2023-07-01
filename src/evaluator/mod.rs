use crate::tokenizer::{Function, Instruction};

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
            Instruction::Function(Function::Log10) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.log10());
            }
            Instruction::Function(Function::Log2) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.log2());
            }
            Instruction::Function(Function::Log) => {
                let y = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(y.log(x));
            }
            Instruction::Function(Function::Sqrt) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.sqrt());
            }
            Instruction::Function(Function::Sin) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.sin());
            }
            Instruction::Function(Function::Cos) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.cos());
            }
            Instruction::Function(Function::Tan) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.tan());
            }
            Instruction::Function(Function::Asin) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.asin());
            }
            Instruction::Function(Function::Acos) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.acos());
            }
            Instruction::Function(Function::Atan) => {
                let x = stack.pop().ok_or(EvalError::InsufficientNumber)?;
                stack.push(x.atan());
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
    use std::{f64::consts::FRAC_PI_2};

    use crate::{
        evaluator::{evaluation, EvalError},
        tokenizer::{tokenization, Instruction},
    };

    fn test_evaluate(expression: &str) -> (Result<f64, EvalError>, Vec<Instruction>) {
        match tokenization(&expression) {
            (Ok(result), history) => (evaluation(result), history),
            (Err(error), history) => {
                println!("Error in tokenization {:?},\n history {:?}", error, history);
                (Err(EvalError::UnevaluableToken), history)
            }
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
            evaluation(vec![Instruction::Multiplication, Instruction::Addition])
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
        let mut result: (Result<f64, EvalError>, Vec<Instruction>) = test_evaluate("22*45");
        assert_eq!(Ok(990.0), result.0, "Ok 1,\n history = {:?}", result.1);

        result = test_evaluate("22+33*44");
        assert_eq!(Ok(1474.0), result.0, "Ok 2,\n history = {:?}", result.1);

        result = test_evaluate("2^4^3");
        assert_eq!(
            Ok(1.8446744073709552e19),
            result.0,
            "Ok 3,\n history = {:?}",
            result.1
        );

        result = test_evaluate("(-2^4)^6");
        assert_eq!(Ok(16777216.0), result.0, "Ok 4,\n history = {:?}", result.1);

        result = test_evaluate("ln(4-3)");
        assert_eq!(Ok(0.0), result.0, "Ok 5,\n history = {:?}", result.1);

        result = test_evaluate("ln(4-3)4ln(8-7)+1");
        assert_eq!(Ok(1.0), result.0, "Ok 6,\n history = {:?}", result.1);

        result = test_evaluate("4-3*4*8-7+1");
        assert_eq!(Ok(-98.0), result.0, "Ok 7,\n history = {:?}", result.1);

        result = test_evaluate("log2(4)");
        assert_eq!(Ok(2.0), result.0, "Ok 8,\n history = {:?}", result.1);

        result = test_evaluate("log10(100)");
        assert_eq!(Ok(2.0), result.0, "Ok 9,\n history = {:?}", result.1);

        result = test_evaluate("log(4, 16)");
        assert_eq!(Ok(2.0), result.0, "Ok 10,\n history = {:?}", result.1);

        result = test_evaluate("sin(0)");
        assert_eq!(Ok(0.0), result.0, "Ok 11,\n history = {:?}", result.1);

        result = test_evaluate("cos(0)");
        assert_eq!(Ok(1.0), result.0, "Ok 12,\n history = {:?}", result.1);

        result = test_evaluate("tan(0)");
        assert_eq!(Ok(0.0), result.0, "Ok 13,\n history = {:?}", result.1);

        result = test_evaluate("asin(0)");
        assert_eq!(Ok(0.0), result.0, "Ok 14,\n history = {:?}", result.1);

        result = test_evaluate("acos(0)");
        assert_eq!(Ok(FRAC_PI_2), result.0, "Ok 15,\n history = {:?}", result.1);

        result = test_evaluate("atan(0)");
        assert_eq!(Ok(0.0), result.0, "Ok 16,\n history = {:?}", result.1);

        result = test_evaluate("sqrt(25)");
        assert_eq!(Ok(5.0), result.0, "Ok 16,\n history = {:?}", result.1);
    }
}
