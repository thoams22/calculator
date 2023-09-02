use std::collections::HashMap;

use crate::tokenizer::CalcError;

use self::addition::Addition;
use self::constant::Constant;
use self::equality::Equality;
use self::exponentiation::Exponentiation;
use self::function::Function;
use self::multiplication::Multiplication;

pub(crate) mod addition;
pub(crate) mod constant;
pub(crate) mod equality;
mod exponentiation;
pub(crate) mod function;
pub(crate) mod math;
mod multiplication;
mod solver;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Number(f64),
    Variable(char),
    Constant(Constant),
    Addition(Box<Addition>),
    Multiplication(Box<Multiplication>),
    Exponentiation(Box<Exponentiation>),
    // Fraction(Box<Fraction>),
    Function(Box<Function>),
    Equality(Box<Equality>),
}

// helper constructeur
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

    pub fn fraction (numerator: Expression, denominator: Expression) -> Expression {
        Expression::division(numerator, denominator)
    
    }

    pub fn division(numerator: Expression, denominator: Expression) -> Expression {
        Expression::multiplication(
            numerator,
            Expression::exponentiation(denominator, Expression::Number(-1.0)),
        )
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

    pub fn e() -> Expression {
        Expression::Constant(Constant::e())
    }

    pub fn pi() -> Expression {
        Expression::Constant(Constant::pi())
    }

    pub fn phi() -> Expression {
        Expression::Constant(Constant::phi())
    }

    pub fn equality(left_side: Expression, right_side: Expression) -> Expression {
        Expression::Equality(Box::new(Equality::new(left_side, right_side)))
    }
}

impl Expression {

    pub fn simplify(self) -> Expression {
        match self {
            Expression::Number(_) => self,
            Expression::Variable(_) => self,
            Expression::Constant(_) => self,
            Expression::Addition(addition) => addition.simplify(),
            Expression::Multiplication(multiplication) => multiplication.simplify(),
            Expression::Exponentiation(exponentiation) => exponentiation.simplify(),
            Expression::Function(function) => function.simplify(),
            Expression::Equality(equality) => equality.simplify(),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Expression::Number(_) => 1,
            Expression::Variable(_) => 1,
            Expression::Constant(_) => 1,
            Expression::Addition(addition) => addition.len(),
            Expression::Multiplication(multiplication) => multiplication.len(),
            Expression::Exponentiation(exponentiation) => exponentiation.len(),
            Expression::Function(function) => function.len(),
            Expression::Equality(equality) => equality.len(),
        }
    }

    pub fn get(&self, index: usize) -> Option<Expression> {
        match self {
            Expression::Number(number) => {
                if index > 0 {
                    return None;
                }
                Some(Expression::Number(*number))
            }
            Expression::Variable(variable) => {
                if index > 0 {
                    return None;
                }
                Some(Expression::Variable(*variable))
            }
            Expression::Constant(constant) => {
                if index > 0 {
                    return None;
                }
                Some(Expression::Constant(*constant))
            }
            Expression::Addition(addition) => addition.get(index).cloned(),
            Expression::Multiplication(multiplication) => multiplication.get(index).cloned(),
            Expression::Exponentiation(exponentiation) => exponentiation.get(index).cloned(),
            Expression::Function(function) => function.get(index).cloned(),
            Expression::Equality(equality) => equality.get(index).cloned(),
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
            Expression::Constant(constant) => {
                if let Expression::Constant(constant_2) = other {
                    if constant == constant_2 {
                        return true;
                    }
                }
                false
            }
            Expression::Addition(addition) => addition.equal(other),
            Expression::Multiplication(multiplication) => multiplication.equal(other),
            Expression::Exponentiation(exponentiation) => exponentiation.equal(other),
            Expression::Function(function) => function.equal(other),
            Expression::Equality(equality) => equality.equal(other),
        }
    }

    pub fn evaluate(&self) -> Result<f64, CalcError> {
        match self {
            Expression::Number(number) => Ok(*number),
            Expression::Variable(_) => Err(CalcError::UnevaluableToken),
            Expression::Constant(constant) => constant.evaluate(),
            Expression::Addition(addition) => addition.evaluate(),
            Expression::Multiplication(multiplication) => multiplication.evaluate(),
            Expression::Exponentiation(exponentiation) => exponentiation.evaluate(),
            Expression::Function(function) => function.evaluate(),
            Expression::Equality(_) => Err(CalcError::UnevaluableToken),
        }
    }

    pub fn to_string(&self) -> Result<String, CalcError> {
        match self {
            Expression::Number(number) => Ok(number.to_string()),
            Expression::Variable(variable) => Ok(variable.to_string()),
            Expression::Constant(constant) => Ok(constant.to_string()),
            Expression::Addition(addition) => addition.to_string(),
            Expression::Multiplication(multiplication) => multiplication.to_string(),
            Expression::Exponentiation(exponentiation) => exponentiation.to_string(),
            Expression::Function(function) => function.to_string(),
            Expression::Equality(equality) => equality.to_string(),
        }
    }

    pub fn contain_var(&self) -> Option<HashMap<char, u8>> {
        match self {
            Expression::Variable(variable) => Some(HashMap::from([(*variable, 1)])),
            Expression::Exponentiation(_)
            | Expression::Function(_)
            | Expression::Multiplication(_)
            | Expression::Addition(_) => {
                let mut variables: HashMap<char, u8> = HashMap::new();
                for expr in 0..self.len() {
                    if let Some(vars) = self.get(expr).unwrap().contain_var() {
                        for (var, value) in vars {
                            *variables.entry(var).or_insert(0) += value;
                        }
                    }
                }
                Some(variables)
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests_expression {
    use crate::expression::{
        addition::Addition, exponentiation::Exponentiation, multiplication::Multiplication,
        Expression,
    };

    #[ignore]
    #[test]
    fn test() {
        let function = Exponentiation::new(
            Expression::variable('a'),
            Expression::log(Expression::variable('a'), Expression::variable('x')),
        );
        if let Expression::Function(fun) = function.get_exponent() {
            let simplified = function.simplify_exponent_logarithm(&fun);
            println!("{:?}", simplified);
        }
    }

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
        result = Expression::multiplication(
            Expression::Number(3.0),
            Expression::division(Expression::Number(3.0), Expression::Number(3.0)),
        )
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
            Expression::division(Expression::Number(3.0), Expression::Number(3.0)),
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
        result =
            Expression::division(Expression::Variable('a'), Expression::Variable('a')).simplify();
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
            result.equal(&Expression::exponentiation(
                Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
                Expression::Number(2.0),
            )),
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
            result.equal(&Expression::exponentiation(
                Expression::subtraction(Expression::Variable('a'), Expression::Variable('b')),
                Expression::Number(2.0)
            )),
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
            result.equal(&Expression::exponentiation(
                Expression::addition(Expression::Variable('a'), Expression::Variable('b')),
                Expression::Number(2.0)
            )),
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

    #[test]
    fn logarithme() {
        // ln(e)
        let mut result = Expression::ln(Expression::e()).simplify();
        assert!(
            result.equal(&Expression::Number(1.0)),
            "logarithme 1\n {:?}",
            result
        );

        // log10(10)
        result = Expression::log10(Expression::Number(10.0)).simplify();
        assert!(
            result.equal(&Expression::Number(1.0)),
            "logarithme 2\n {:?}",
            result
        );

        // log2(2)
        result = Expression::log2(Expression::Number(2.0)).simplify();
        assert!(
            result.equal(&Expression::Number(1.0)),
            "logarithme 3\n {:?}",
            result
        );

        // log(20, 20)
        result = Expression::log(Expression::Number(20.0), Expression::Number(20.0)).simplify();
        assert!(
            result.equal(&Expression::Number(1.0)),
            "logarithme 4\n {:?}",
            result
        );

        // ln(1)
        result = Expression::ln(Expression::Number(1.0)).simplify();
        assert!(
            result.equal(&Expression::Number(0.0)),
            "logarithme 5\n {:?}",
            result
        );

        // log10(1)
        result = Expression::log10(Expression::Number(1.0)).simplify();
        assert!(
            result.equal(&Expression::Number(0.0)),
            "logarithme 6\n {:?}",
            result
        );

        // log2(1)
        result = Expression::log2(Expression::Number(1.0)).simplify();
        assert!(
            result.equal(&Expression::Number(0.0)),
            "logarithme 7\n {:?}",
            result
        );

        // log(20, 1)
        result = Expression::log(Expression::Number(20.0), Expression::Number(1.0)).simplify();
        assert!(
            result.equal(&Expression::Number(0.0)),
            "logarithme 8\n {:?}",
            result
        );

        // ln(e^3)
        result = Expression::ln(Expression::exponentiation(
            Expression::e(),
            Expression::Number(3.0),
        ))
        .simplify();
        assert!(
            result.equal(&Expression::Number(3.0)),
            "logarithme 9\n {:?}",
            result
        );

        // log10(10^3)
        result = Expression::log10(Expression::exponentiation(
            Expression::Number(10.0),
            Expression::Number(3.0),
        ))
        .simplify();
        assert!(
            result.equal(&Expression::Number(3.0)),
            "logarithme 10\n {:?}",
            result
        );

        // log2(2^3)
        result = Expression::log2(Expression::exponentiation(
            Expression::Number(2.0),
            Expression::Number(3.0),
        ))
        .simplify();
        assert!(
            result.equal(&Expression::Number(3.0)),
            "logarithme 11\n {:?}",
            result
        );

        // log(20, 20^3)
        result = Expression::log(
            Expression::Number(20.0),
            Expression::exponentiation(Expression::Number(20.0), Expression::Number(3.0)),
        )
        .simplify();
        assert!(
            result.equal(&Expression::Number(3.0)),
            "logarithme 12\n {:?}",
            result
        );

        // ln(8^3)
        result = Expression::ln(Expression::exponentiation(
            Expression::Number(8.0),
            Expression::Number(3.0),
        ))
        .simplify();
        assert!(
            result.equal(&Expression::multiplication(
                Expression::Number(9.0),
                Expression::ln(Expression::Number(2.0))
            )),
            "logarithme 13\n {:?}",
            result
        );

        // ln(8)
        result = Expression::ln(Expression::Number(8.0)).simplify();
        assert!(
            result.equal(&Expression::multiplication(
                Expression::Number(3.0),
                Expression::ln(Expression::Number(2.0))
            )),
            "logarithme 14\n {:?}",
            result
        );
    }

    #[test]
    fn mulltiple_basic_division() {
        // (b/a)/b
        let mut result = Expression::division(
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            Expression::Variable('b'),
        )
        .simplify();
        assert!(
            result.equal(&Expression::exponentiation(
                Expression::variable('a'),
                Expression::number(-1.0),
            )),
            "mulltiple_basic_division 1 \n {:?}",
            result
        );

        // b/(b/a)
        result = Expression::division(
            Expression::Variable('b'),
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::variable('a')),
            "mulltiple_basic_division 2 \n {:?}",
            result
        );

        // (b/a)/(b/a)
        result = Expression::division(
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::number(1.0)),
            "mulltiple_basic_division 3 \n {:?}",
            result
        );

        // (b/a)/(a/b)
        result = Expression::division(
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            Expression::division(Expression::Variable('a'), Expression::Variable('b')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::multiplication(
                Expression::exponentiation(Expression::Variable('b'), Expression::number(2.0)),
                Expression::exponentiation(Expression::Variable('a'), Expression::number(-2.0))
            ),),
            "mulltiple_basic_division 4 \n {:?}",
            result
        );

        // (b/a)*(b/a)
        result = Expression::multiplication(
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::multiplication(
                Expression::exponentiation(Expression::Variable('b'), Expression::number(2.0)),
                Expression::exponentiation(Expression::Variable('a'), Expression::number(-2.0))
            ),),
            "mulltiple_basic_division 5 \n {:?}",
            result
        );

        // (b/a)+(b/a)
        result = Expression::addition(
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::multiplication_from_vec(vec![
                Expression::number(2.0),
                Expression::Variable('b'),
                Expression::exponentiation(Expression::Variable('a'), Expression::number(-1.0))
            ])),
            "mulltiple_basic_division 7 \n {:?}",
            result
        );

        // (b/a)-(b/a)
        result = Expression::subtraction(
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::number(0.0),),
            "mulltiple_basic_division 8 \n {:?}",
            result
        );

        // (b/a)+(a/b)
        result = Expression::addition(
            Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            Expression::division(Expression::Variable('a'), Expression::Variable('b')),
        )
        .simplify();
        assert!(
            result.equal(&Expression::addition(
                Expression::division(Expression::Variable('b'), Expression::Variable('a')),
                Expression::division(Expression::Variable('a'), Expression::Variable('b')),
            )),
            "mulltiple_basic_division 9 \n {:?}",
            result
        );

        // (((b/a)/b)/a)
        result = Expression::division(
            Expression::division(
                Expression::division(Expression::Variable('b'), Expression::Variable('a')),
                Expression::Variable('b'),
            ),
            Expression::Variable('a'),
        )
        .simplify();
        assert!(
            result.equal(&Expression::exponentiation(
                Expression::Variable('a'),
                Expression::Number(-2.0)
            ),),
            "mulltiple_basic_division 10 \n {:?}",
            result
        );

        // (b/(a/(b/a)))
        result = Expression::division(
            Expression::Variable('b'),
            Expression::division(
                Expression::Variable('a'),
                Expression::division(Expression::Variable('b'), Expression::Variable('a')),
            ),
        )
        .simplify();
        assert!(
            result.equal(&Expression::multiplication(
                Expression::exponentiation(Expression::Variable('b'), Expression::Number(2.0)),
                Expression::exponentiation(Expression::Variable('a'), Expression::Number(-2.0))
            )),
            "mulltiple_basic_division 10 \n {:?}",
            result
        );
    }
}
