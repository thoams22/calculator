pub mod addition;
pub mod multiplication;
pub mod fraction;
pub mod equality;
pub mod exponentiation;
pub mod negation;
pub mod function;
pub(crate) mod math;

use self::addition::Addition;
use self::multiplication::Multiplication;
use self::fraction::Fraction;
use self::equality::Equality;
use self::exponentiation::Exponentiation;
use self::negation::Negation;
use self::function::FunctionType;
use crate::ast::ConstantKind;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Number(i64),
    Variable(char),
    Constant(ConstantKind),
    Addition(Box<Addition>),
    Multiplication(Box<Multiplication>),
    Exponentiation(Box<Exponentiation>),
    Fraction(Box<Fraction>),
    Equality(Box<Equality>),
    Negation(Box<Negation>),
    Function(Box<FunctionType>),
}

// Helper constructor
impl Expression {
    pub fn addition(left: Expression, right: Expression) -> Expression {
        Expression::Addition(Box::new(Addition::new(left, right)))
    }

    pub fn multiplication(left: Expression, right: Expression) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::new(left, right)))
    }

    pub fn multiplication_from_vec(sub_expr: Vec<Expression>) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::from_vec(sub_expr)))
    }

    pub fn fraction(numerator: Expression, denominator: Expression) -> Expression {
        Expression::Fraction(Box::new(Fraction::new(numerator, denominator)))
    }

    pub fn negation(operand: Expression) -> Expression {
        Expression::Negation(Box::new(Negation::new(operand)))
    }

    pub fn function(function: FunctionType) -> Expression {
        Expression::Function(Box::new(function))
    }

    pub fn exponentiation(base: Expression, exponent: Expression) -> Expression {
        Expression::Exponentiation(Box::new(Exponentiation::new(base, exponent)))
    }

    pub fn equality(left: Expression, right: Expression) -> Expression {
        Expression::Equality(Box::new(Equality::new(left, right)))
    }
}

impl Expression {

    pub fn equal(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Number(left), Expression::Number(right)) => left == right,
            (Expression::Variable(left), Expression::Variable(right)) => left == right,
            (Expression::Constant(left), Expression::Constant(right)) => left == right,
            (Expression::Addition(left), Expression::Addition(right)) => left.equal(right),
            (Expression::Multiplication(left), Expression::Multiplication(right)) => {
                left.equal(right)
            }
            (Expression::Exponentiation(left), Expression::Exponentiation(right)) => {
                left.equal(right)
            }
            // (Expression::Fraction(left), Expression::Fraction(right)) => left.equal(right),
            // (Expression::Equality(left), Expression::Equality(right)) => left.equal(right),
            // (Expression::Negation(left), Expression::Negation(right)) => left.equal(right),
            // (Expression::Function(left), Expression::Function(right)) => left.equal(right),
            _ => false,
        }
    }

    pub fn simplify(self) -> Expression{
        match self {
            Expression::Addition(add) => add.simplify(),
            Expression::Multiplication(mult) => mult.simplify(),
            Expression::Exponentiation(expo) => expo.simplify(),
            Expression::Fraction(frac) => frac.simplify(),
            Expression::Equality(eq) => eq.simplify(),
            Expression::Negation(neg) => neg.simplify(),
            Expression::Function(func) => func.simplify(),
            _ => self
        }
    }
}