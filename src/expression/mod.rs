use crate::tokenizer::CalcError;

use self::addition::Addition;
use self::division::Division;
use self::exponentiation::Exponentiation;
use self::function::Function;
use self::multiplication::Multiplication;

mod addition;
mod division;
mod exponentiation;
pub(crate) mod function;
mod multiplication;
mod math;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Number(f64),
    Variable(char),
    Addition(Box<Addition>),
    Multiplication(Box<Multiplication>),
    Division(Box<Division>),
    Exponentiation(Box<Exponentiation>),
    Function(Box<Function>),
}

// constructeur
impl Expression {
    pub fn number(number: f64) -> Expression {
        Expression::Number(number)
    }
    pub fn variable(variable: char) -> Expression {
        Expression::Variable(variable)
    }

    pub fn addition(first: Expression, second: Expression) -> Expression {
        Expression::Addition(Box::new(Addition::new(first, second)))
    }

    pub fn addition_from_vec(vec: Vec<Expression>) -> Expression {
        Expression::Addition(Box::new(Addition::from_vec(vec)))
    }

    pub fn subtraction(first: Expression, second: Expression) -> Expression {
        Expression::Addition(Box::new(Addition::new(
            first,
            Expression::multiplication(Self::Number(-1.0), second),
        )))
    }

    pub fn multiplication(first: Expression, second: Expression) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::new(first, second)))
    }

    pub fn multiplication_from_vec(vec: Vec<Expression>) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::from_vec(vec)))
    }

    pub fn division(numerator: Expression, denominator: Expression) -> Expression {
        Expression::Division(Box::new(Division::new(numerator, denominator)))
    }

    pub fn exponentiation(base: Expression, exponent: Expression) -> Expression {
        Expression::Exponentiation(Box::new(Exponentiation::new(base, exponent)))
    }

    pub fn sqrt(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::sqrt(expr)))
    }

    pub fn ln(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::ln(expr)))
    }

    pub fn log2(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::log2(expr)))
    }

    pub fn log10(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::log10(expr)))
    }

    pub fn sin(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::sin(expr)))
    }

    pub fn asin(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::asin(expr)))
    }

    pub fn acos(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::acos(expr)))
    }

    pub fn cos(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::cos(expr)))
    }

    pub fn tan(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::tan(expr)))
    }

    pub fn atan(expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::atan(expr)))
    }

    pub fn log(base: Expression, expr: Expression) -> Expression {
        Expression::Function(Box::new(Function::log(base, expr)))
    }
}

impl Expression {
    pub fn simplify(self) -> Expression {
        match self {
            Expression::Number(_) => self,
            Expression::Variable(_) => self,
            Expression::Addition(addition) => addition.simplify(),
            Expression::Multiplication(multiplication) => multiplication.simplify(),
            Expression::Division(division) => division.simplify(),
            Expression::Exponentiation(exponentiation) => exponentiation.simplify(),
            Expression::Function(function) => function.simplify(),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Expression::Number(_) => 1,
            Expression::Variable(_) => 1,
            Expression::Addition(addition) => addition.len(),
            Expression::Multiplication(multiplication) => multiplication.len(),
            Expression::Division(division) => division.len(),
            Expression::Exponentiation(exponentiation) => exponentiation.len(),
            Expression::Function(function) => function.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<Expression> {
        match self {
            Expression::Number(number) => Some(Expression::Number(*number)),
            Expression::Variable(variable) => Some(Expression::Variable(*variable)),
            Expression::Addition(addition) => addition.get(index).cloned(),
            Expression::Multiplication(multiplication) => multiplication.get(index).cloned(),
            Expression::Division(division) => division.get(index).cloned(),
            Expression::Exponentiation(exponentiation) => exponentiation.get(index).cloned(),
            Expression::Function(function) => function.get(index).cloned(),
        }
    }

    pub fn equal(&self, other: &Expression) -> bool {
        match self {
            Expression::Number(number) => {
                if let Expression::Number(number_2) = other {
                    if number == number_2 {
                        return true;
                    }
                }
                false
            }
            Expression::Variable(variable) => {
                if let Expression::Variable(variable_2) = other {
                    if variable == variable_2 {
                        return true;
                    }
                }
                false
            }
            Expression::Addition(addition) => addition.equal(other),
            Expression::Multiplication(multiplication) => multiplication.equal(other),
            Expression::Division(division) => division.equal(other),
            Expression::Exponentiation(exponentiation) => exponentiation.equal(other),
            Expression::Function(function) => function.equal(other),
        }
    }

    pub fn evaluate(&self) -> Result<f64, CalcError> {
        match self {
            Expression::Number(number) => Ok(*number),
            Expression::Variable(_) => Err(CalcError::UnevaluableToken),
            Expression::Addition(addition) => addition.evaluate(),
            Expression::Multiplication(multiplication) => multiplication.evaluate(),
            Expression::Division(division) => division.evaluate(),
            Expression::Exponentiation(exponentiation) => exponentiation.evaluate(),
            Expression::Function(function) => function.evaluate(),
        }
    }

    pub fn to_string(&self) -> Result<String, CalcError> {
        match self {
            Expression::Number(number) => Ok(number.to_string()),
            Expression::Variable(variable) => Ok(variable.to_string()),
            Expression::Addition(addition) => addition.to_string(),
            Expression::Multiplication(multiplication) => multiplication.to_string(),
            Expression::Division(division) => division.to_string(),
            Expression::Exponentiation(exponentiation) => exponentiation.to_string(),
            Expression::Function(function) => function.to_string(),
        }
    }
}

#[cfg(test)]
mod tests_expression {
    use crate::expression::{
        addition::Addition, division::Division, exponentiation::Exponentiation,
        multiplication::Multiplication, Expression,
    };

    #[test]
    fn equal() {
        let mut result = Expression::addition(Expression::Variable('a'), Expression::Variable('b'));
        assert!(result.equal(&Expression::Addition(Box::new(Addition::new(
            Expression::Variable('b'),
            Expression::Variable('a'),
        )))));

        result = Expression::subtraction(Expression::Variable('a'), Expression::Variable('b'));
        assert!(result.equal(&Expression::addition(
            Expression::Variable('a'),
            Expression::multiplication(Expression::Number(-1.0), Expression::Variable('b'),),
        )));

        result = Expression::Addition(Box::new(Addition::new(
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Variable('a'),
            ))),
            Expression::Number(1.0),
        )));
        assert!(result.equal(&Expression::Addition(Box::new(Addition::new(
            Expression::Number(1.0),
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Variable('a'),
            )))
        )))));

        result = Expression::Multiplication(Box::new(Multiplication::from_vec(vec![
            Expression::Number(4.0),
            Expression::Variable('a'),
            Expression::Variable('b'),
        ])));
        assert!(result.equal(&Expression::Multiplication(Box::new(
            Multiplication::from_vec(vec![
                Expression::Variable('b'),
                Expression::Variable('a'),
                Expression::Number(4.0),
            ])
        ))));
    }

    #[test]
    fn basic() {
        // 3 + 3 - 3
        let mut result = Expression::addition(
            Expression::Number(3.0),
            Expression::subtraction(Expression::Number(3.0), Expression::Number(3.0)),
        )
        .simplify();
        assert_eq!(
            result,
            Expression::Number(3.0),
            "basic 1\n result {:?}",
            result
        );

        // 3 * 3 / 3
        result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::Number(3.0),
            Expression::Division(Box::new(Division::new(
                Expression::Number(3.0),
                Expression::Number(3.0),
            ))),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(3.0),
            "basic 2\n result {:?}",
            result
        );

        // 3 * 3 + 3 / 3
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(3.0),
                Expression::Number(3.0),
            ))),
            Expression::Division(Box::new(Division::new(
                Expression::Number(3.0),
                Expression::Number(3.0),
            ))),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(10.0),
            "basic 3\n result {:?}",
            result
        );

        // a - b
        result = Expression::subtraction(Expression::Variable('a'), Expression::Variable('b'))
            .simplify();
        assert!(
            result.equal(&Expression::subtraction(
                Expression::Variable('a'),
                Expression::Variable('b'),
            )),
            "basic 4\n result {:?}",
            result
        );

        // a - b
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Variable('a'),
            Expression::Variable('b'),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            )))),
            "basic 5\n result {:?}",
            result
        );

        // a + a
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Variable('a'),
            Expression::Variable('a'),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Variable('a'),
            )))),
            "basic 6\n result {:?}",
            result
        );

        // a - a
        result = Expression::subtraction(Expression::Variable('a'), Expression::Variable('a'))
            .simplify();
        assert_eq!(
            result,
            Expression::Number(0.0),
            "basic 7\n result {:?}",
            result
        );

        // a * a
        result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::Variable('a'),
            Expression::Variable('a'),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Exponentiation(Box::new(Exponentiation::new(
                Expression::Variable('a'),
                Expression::Number(2.0),
            )))),
            "basic 8\n result {:?}",
            result
        );

        // a / a
        result = Expression::Division(Box::new(Division::new(
            Expression::Variable('a'),
            Expression::Variable('a'),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(1.0),
            "basic 9\n result {:?}",
            result
        );

        // a + 1
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Variable('a'),
            Expression::Number(1.0),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Number(1.0),
            )))),
            "basic 10\n result {:?}",
            result
        );

        // 2a + 1
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Variable('a'),
            ))),
            Expression::Number(1.0),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Addition(Box::new(Addition::new(
                Expression::Multiplication(Box::new(Multiplication::new(
                    Expression::Number(2.0),
                    Expression::Variable('a'),
                ))),
                Expression::Number(1.0),
            )))),
            "basic 11\n result {:?}",
            result
        );

        // a + 0
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Variable('a'),
            Expression::Number(0.0),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Variable('a'),
            "basic 11\n result {:?}",
            result
        );

        // 1 + 0
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Number(1.0),
            Expression::Number(0.0),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(1.0),
            "basic 12\n result {:?}",
            result
        );

        // a * 0
        result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::Variable('a'),
            Expression::Number(0.0),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(0.0),
            "basic 13\n result {:?}",
            result
        );

        // 1 * 0
        result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::Number(1.0),
            Expression::Number(0.0),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(0.0),
            "basic 14\n result {:?}",
            result
        );

        // 1 ^ 0
        result =
            Expression::exponentiation(Expression::Number(1.0), Expression::Number(0.0)).simplify();
        assert_eq!(
            result,
            Expression::Number(1.0),
            "basic 15\n result {:?}",
            result
        );

        // a ^ 0
        result = Expression::exponentiation(Expression::Variable('a'), Expression::Number(0.0))
            .simplify();
        assert_eq!(
            result,
            Expression::Number(1.0),
            "basic 16\n result {:?}",
            result
        );

        // (a+b) ^ 0
        result = Expression::exponentiation(
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
            Expression::Number(0.0),
        )
        .simplify();
        assert_eq!(
            result,
            Expression::Number(1.0),
            "basic 17\n result {:?}",
            result
        );

        // 1 ^ 1
        result =
            Expression::exponentiation(Expression::Number(1.0), Expression::Number(1.0)).simplify();
        assert_eq!(
            result,
            Expression::Number(1.0),
            "basic 18\n result {:?}",
            result
        );

        // a ^ 1
        result = Expression::exponentiation(Expression::Variable('a'), Expression::Number(1.0))
            .simplify();
        assert_eq!(
            result,
            Expression::Variable('a'),
            "basic 19\n result {:?}",
            result
        );

        // (a+b) ^ 1
        result = Expression::exponentiation(
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
            Expression::Number(1.0),
        )
        .simplify();
        assert!(
            result.equal(&Expression::addition(
                Expression::Variable('a'),
                Expression::Variable('b')
            )),
            "basic 20\n result {:?}",
            result
        );
    }

    #[test]
    fn mulltiple_basic_add() {
        // (a+b)+(a+b)
        let mut result = Expression::Addition(Box::new(Addition::new(
            Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            ))),
            Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            ))),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Addition(Box::new(Addition::new(
                Expression::Multiplication(Box::new(Multiplication::new(
                    Expression::Number(2.0),
                    Expression::Variable('b'),
                ))),
                Expression::Multiplication(Box::new(Multiplication::new(
                    Expression::Number(2.0),
                    Expression::Variable('a'),
                )))
            )))),
            "mulltiple_basic_add 1\n result {:?}",
            result
        );

        // (2+4)+(4-2)
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Addition(Box::new(Addition::new(
                Expression::Number(2.0),
                Expression::Number(4.0),
            ))),
            Expression::subtraction(Expression::Number(4.0), Expression::Number(2.0)),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(8.0),
            "mulltiple_basic_add 2\n result {:?}",
            result
        );

        // (a-a)+(a-a)
        result = Expression::Addition(Box::new(Addition::new(
            Expression::subtraction(Expression::Variable('a'), Expression::Variable('a')),
            Expression::subtraction(Expression::Variable('a'), Expression::Variable('a')),
        )))
        .simplify();
        assert_eq!(
            result,
            Expression::Number(0.0),
            "mulltiple_basic_add 3\n result {:?}",
            result
        );

        // (a+a)+(a+a)
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Variable('a'),
            ))),
            Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Variable('a'),
            ))),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(4.0),
                Expression::Variable('a')
            )))),
            "mulltiple_basic_add 4\n result {:?}",
            result
        );

        // (a+a)+a
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Variable('a'),
            ))),
            Expression::Variable('a'),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(3.0),
                Expression::Variable('a')
            )))),
            "mulltiple_basic_add 5\n result {:?}",
            result
        );

        // 2ab + 2ab
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Multiplication(Box::new(Multiplication::new(
                    Expression::Variable('a'),
                    Expression::Variable('b'),
                ))),
            ))),
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Multiplication(Box::new(Multiplication::new(
                    Expression::Variable('a'),
                    Expression::Variable('b'),
                ))),
            ))),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Multiplication(Box::new(
                Multiplication::from_vec(vec![
                    Expression::Number(4.0),
                    Expression::Variable('a'),
                    Expression::Variable('b')
                ])
            ))),
            "mulltiple_basic_add 6\n result {:?}",
            result
        );

        // 2ab + 2a
        result = Expression::Addition(Box::new(Addition::new(
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Multiplication(Box::new(Multiplication::new(
                    Expression::Variable('a'),
                    Expression::Variable('b'),
                ))),
            ))),
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Number(2.0),
                Expression::Variable('a'),
            ))),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Addition(Box::new(Addition::new(
                Expression::Multiplication(Box::new(Multiplication::from_vec(vec![
                    Expression::Number(2.0),
                    Expression::Variable('b'),
                    Expression::Variable('a'),
                ]))),
                Expression::Multiplication(Box::new(Multiplication::new(
                    Expression::Number(2.0),
                    Expression::Variable('a'),
                ))),
            )))),
            "mulltiple_basic_add 7\n result {:?}",
            result
        );

        // a^2 + a^2
        result = Expression::Addition(Box::new(Addition::new(
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Multiplication(Box::new(
                Multiplication::from_vec(vec![
                    Expression::Number(2.0),
                    Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
                ])
            ))),
            "mulltiple_basic_add 8\n result {:?}",
            result
        );

        // (a+b)^2
        result = Expression::exponentiation(
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
            Expression::Number(2.0),
        )
        .simplify();
        assert!(
            result.equal(&Expression::addition_from_vec(vec![
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('a'),
                    Expression::Number(2.0),
                ))),
                Expression::multiplication_from_vec(vec![
                    Expression::Variable('b'),
                    Expression::Variable('a'),
                    Expression::Number(2.0)
                ]),
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('b'),
                    Expression::Number(2.0),
                )))
            ])),
            "mulltiple_basic_add 8\n result {:?}",
            result
        );
    }

    #[test]
    fn mulltiple_basic_mult() {
        // (a*b)*(a*b)
        let mut result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            ))),
            Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            ))),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Multiplication(Box::new(Multiplication::new(
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('a'),
                    Expression::Number(2.0),
                ))),
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('b'),
                    Expression::Number(2.0),
                )))
            )))),
            "mulltiple_basic_mult 1 \n result {:?}",
            result
        );

        // (a+b)*(a-b)
        result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::Addition(Box::new(Addition::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            ))),
            Expression::subtraction(Expression::Variable('a'), Expression::Variable('b')),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::subtraction(
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('a'),
                    Expression::Number(2.0),
                ))),
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('b'),
                    Expression::Number(2.0),
                )))
            )),
            "mulltiple_basic_mult 2 \n result : {:?}",
            result
        );

        // 2*a*2*b
        result = Expression::multiplication(
            Expression::multiplication(Expression::Number(2.0), Expression::Variable('b')),
            Expression::multiplication(Expression::Number(2.0), Expression::Variable('a')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::multiplication_from_vec(vec![
                Expression::Number(4.0),
                Expression::Variable('b'),
                Expression::Variable('a')
            ])),
            "mulltiple_basic_mult 3 \n result : {:?}",
            result
        );

        // 2*(a+b)
        result = Expression::multiplication(
            Expression::Number(2.0),
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::addition(
                Expression::multiplication(Expression::Number(2.0), Expression::Variable('b'),),
                Expression::multiplication(Expression::Number(2.0), Expression::Variable('a'),),
            )),
            "mulltiple_basic_mult 4 \n result : {:?}",
            result
        );

        // a*(a+b)
        result = Expression::multiplication(
            Expression::Variable('a'),
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::addition(
                Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
                Expression::multiplication(Expression::Variable('a'), Expression::Variable('b'),),
            )),
            "mulltiple_basic_mult 5 \n result : {:?}",
            result
        );

        // (a-b)*(a-b)
        result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::subtraction(Expression::Variable('a'), Expression::Variable('b')),
            Expression::subtraction(Expression::Variable('a'), Expression::Variable('b')),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::addition_from_vec(vec![
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('a'),
                    Expression::Number(2.0),
                ))),
                Expression::multiplication_from_vec(vec![
                    Expression::Variable('b'),
                    Expression::Variable('a'),
                    Expression::Number(-2.0)
                ]),
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('b'),
                    Expression::Number(2.0),
                )))
            ])),
            "mulltiple_basic_mult 6 \n result : {:?}",
            result
        );

        // (a+b)*(a+b)
        result = Expression::Multiplication(Box::new(Multiplication::new(
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::addition_from_vec(vec![
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('a'),
                    Expression::Number(2.0),
                ))),
                Expression::multiplication_from_vec(vec![
                    Expression::Variable('b'),
                    Expression::Variable('a'),
                    Expression::Number(2.0)
                ]),
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('b'),
                    Expression::Number(2.0),
                )))
            ])),
            "mulltiple_basic_mult 7 \n result : {:?}",
            result
        );

        // a^2*a
        result = Expression::multiplication(
            Expression::Exponentiation(Box::new(Exponentiation::new(
                Expression::Variable('a'),
                Expression::Number(2.0),
            ))),
            Expression::Variable('a'),
        )
        .simplify();
        assert!(
            result.equal(&Expression::exponentiation(
                Expression::Variable('a'),
                Expression::Number(3.0),
            )),
            "mulltiple_basic_mult 8 \n result : {:?}",
            result
        );

        // a^2*a^2
        result = Expression::multiplication(
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
        )
        .simplify();
        assert!(
            result.equal(&Expression::exponentiation(
                Expression::Variable('a'),
                Expression::Number(4.0),
            )),
            "mulltiple_basic_mult 9 \n result : {:?}",
            result
        );

        // a^2*(a+b)
        result = Expression::multiplication(
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
            Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::addition(
                Expression::exponentiation(Expression::Variable('a'), Expression::Number(3.0)),
                Expression::multiplication(
                    Expression::exponentiation(Expression::Variable('a'), Expression::Number(2.0)),
                    Expression::Variable('b')
                )
            )),
            "mulltiple_basic_mult 10 \n result : {:?}",
            result
        );
    }
}
