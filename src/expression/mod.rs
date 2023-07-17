use self::addition::Addition;
use self::division::Division;
use self::exponentiation::Exponentiation;
use self::multiplication::Multiplication;
use self::subtraction::Subtraction;
// use self::function::Function;

mod addition;
mod division;
mod exponentiation;
mod multiplication;
mod subtraction;
// mod function;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Number(f64),
    Variable(char),
    Addition(Box<Addition>),
    Subtraction(Box<Subtraction>),
    Multiplication(Box<Multiplication>),
    Division(Box<Division>),
    Exponentiation(Box<Exponentiation>),
    // Function(Box<Function>),
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Functions {
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

pub trait Expr {
    fn simplify(self) -> Expression;
    fn size(&self) -> u32;
    fn get(&self, index: usize) -> Option<&Expression>;
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
}

impl Expression {
    pub fn simplify(self) -> Expression {
        match self {
            Expression::Number(_) => self,
            Expression::Variable(_) => self,
            Expression::Addition(addition) => addition.simplify(),
            Expression::Subtraction(subtraction) => subtraction.simplify(),
            Expression::Multiplication(multiplication) => multiplication.simplify(),
            Expression::Division(division) => division.simplify(),
            Expression::Exponentiation(exponentiation) => exponentiation.simplify(),
            // Expression::Function(function) => function.simplify(),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Expression::Number(_) => 1,
            Expression::Variable(_) => 1,
            Expression::Addition(addition) => addition.size(),
            Expression::Subtraction(subtraction) => subtraction.size(),
            Expression::Multiplication(multiplication) => multiplication.size(),
            Expression::Division(division) => division.size(),
            Expression::Exponentiation(exponentiation) => exponentiation.size(),
        }
    }

    pub fn get(&self, index: usize) -> Option<Expression> {
        match self {
            Expression::Number(number) => Some(Expression::Number(*number)),
            Expression::Variable(variable) => Some(Expression::Variable(*variable)),
            Expression::Addition(addition) => addition.get(index).cloned(),
            Expression::Subtraction(subtraction) => subtraction.get(index).cloned(),
            Expression::Multiplication(multiplication) => multiplication.get(index).cloned(),
            Expression::Division(division) => division.get(index).cloned(),
            Expression::Exponentiation(exponentiation) => exponentiation.get(index).cloned(),
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
            Expression::Subtraction(subtraction) => subtraction.equal(other),
            Expression::Multiplication(multiplication) => multiplication.equal(other),
            Expression::Division(division) => division.equal(other),
            Expression::Exponentiation(exponentiation) => exponentiation.equal(other),
        }
    }
}

#[cfg(test)]
mod tests_expression {
    use crate::expression::{
        addition::Addition, division::Division, exponentiation::Exponentiation,
        multiplication::Multiplication, subtraction::Subtraction, Expression,
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
            Expression::multiplication(
                Expression::Number(-1.0),
                Expression::Variable('b'),
            ),
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
        let mut result = Expression::Addition(Box::new(Addition::new(
            Expression::Number(3.0),
            Expression::Subtraction(Box::new(Subtraction::new(
                Expression::Number(3.0),
                Expression::Number(3.0),
            ))),
        )))
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
        result = Expression::Subtraction(Box::new(Subtraction::new(
            Expression::Variable('a'),
            Expression::Variable('b'),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Subtraction(Box::new(Subtraction::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            )))),
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
        result = Expression::Subtraction(Box::new(Subtraction::new(
            Expression::Variable('a'),
            Expression::Variable('a'),
        )))
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
            Expression::Subtraction(Box::new(Subtraction::new(
                Expression::Number(4.0),
                Expression::Number(2.0),
            ))),
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
            Expression::Subtraction(Box::new(Subtraction::new(
                Expression::Variable('a'),
                Expression::Variable('a'),
            ))),
            Expression::Subtraction(Box::new(Subtraction::new(
                Expression::Variable('a'),
                Expression::Variable('a'),
            ))),
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
            Expression::Subtraction(Box::new(Subtraction::new(
                Expression::Variable('a'),
                Expression::Variable('b'),
            ))),
        )))
        .simplify();
        assert!(
            result.equal(&Expression::Subtraction(Box::new(Subtraction::new(
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('a'),
                    Expression::Number(2.0),
                ))),
                Expression::Exponentiation(Box::new(Exponentiation::new(
                    Expression::Variable('b'),
                    Expression::Number(2.0),
                )))
            )))),
            "mulltiple_basic_mult 2 \n result {:?}",
            result
        );
    }

    // #[test]
    // fn diff_exp() {
    //     let mut result = reduction(vec![
    //         Instruction::Variable('a'),
    //         Instruction::Variable('b'),
    //         Instruction::Subtraction,
    //         Instruction::Variable('a'),
    //         Instruction::Variable('b'),
    //         Instruction::Addition,
    //         Instruction::Multiplication,
    //     ]);
    //     assert_eq!(
    //         result,
    //         vec![
    //             Instruction::Variable('a'),
    //             Instruction::Number(2.0),
    //             Instruction::Exponentiation,
    //             Instruction::Variable('b'),
    //             Instruction::Number(2.0),
    //             Instruction::Exponentiation,
    //             Instruction::Subtraction,
    //         ],
    //         "diff_exp 1"
    //     );

    //     result = reduction(vec![
    //         Instruction::Variable('a'),
    //         Instruction::Variable('a'),
    //         Instruction::Multiplication,
    //         Instruction::Variable('b'),
    //         Instruction::Subtraction,
    //         Instruction::Variable('a'),
    //         Instruction::Variable('a'),
    //         Instruction::Multiplication,
    //         Instruction::Variable('b'),
    //         Instruction::Addition,
    //         Instruction::Multiplication,
    //     ]);
    //     assert_eq!(
    //         result,
    //         vec![
    //             Instruction::Variable('a'),
    //             Instruction::Number(4.0),
    //             Instruction::Exponentiation,
    //             Instruction::Variable('b'),
    //             Instruction::Number(2.0),
    //             Instruction::Exponentiation,
    //             Instruction::Subtraction,
    //         ],
    //         "diff_exp 2"
    //     );

    //     result = reduction(vec![
    //         Instruction::Number(16.0),
    //         Instruction::Number(2.0),
    //         Instruction::Multiplication,
    //         Instruction::Number(3.0),
    //         Instruction::Multiplication,
    //         Instruction::Number(3.0),
    //         Instruction::Number(2.0),
    //         Instruction::Exponentiation,
    //         Instruction::Multiplication,
    //         Instruction::Number(5.0),
    //         Instruction::Number(4.0),
    //         Instruction::Multiplication,
    //         Instruction::Number(4.0),
    //         Instruction::Multiplication,
    //         Instruction::Addition,
    //         Instruction::Number(5.0),
    //         Instruction::Addition,
    //     ]);
    //     assert_eq!(result, vec![Instruction::Number(949.0),], "diff_exp 3");

    //     result = reduction(vec![
    //         Instruction::Number(16.0),
    //         Instruction::Number(4.0),
    //         Instruction::Function(Function::Sqrt),
    //         Instruction::Multiplication,
    //         Instruction::Number(8.0),
    //         Instruction::Function(Function::Log2),
    //         Instruction::Multiplication,
    //         Instruction::Number(8.0),
    //         Instruction::Function(Function::Log2),
    //         Instruction::Number(4.0),
    //         Instruction::Function(Function::Sqrt),
    //         Instruction::Exponentiation,
    //         Instruction::Multiplication,
    //         Instruction::Number(5.0),
    //         Instruction::Number(4.0),
    //         Instruction::Multiplication,
    //         Instruction::Number(4.0),
    //         Instruction::Multiplication,
    //         Instruction::Addition,
    //         Instruction::Number(5.0),
    //         Instruction::Addition,
    //     ]);
    //     assert_eq!(
    //         result,
    //         vec![
    //             Instruction::Number(16.0),
    //             Instruction::Number(4.0),
    //             Instruction::Function(Function::Sqrt),
    //             Instruction::Multiplication,
    //             Instruction::Number(8.0),
    //             Instruction::Function(Function::Log2),
    //             Instruction::Multiplication,
    //             Instruction::Number(8.0),
    //             Instruction::Function(Function::Log2),
    //             Instruction::Number(4.0),
    //             Instruction::Function(Function::Sqrt),
    //             Instruction::Exponentiation,
    //             Instruction::Multiplication,
    //             Instruction::Number(85.0),
    //             Instruction::Addition,
    //         ],
    //         "diff_exp 4"
    //     );
    // }
}
