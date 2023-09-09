use crate::simplify::Expression;

use super::{addition::Addition, math::gcd};

#[derive(PartialEq, Debug, Clone)]
pub struct Fraction {
    sub_expr: [Expression; 2],
    simplified: bool,
}

impl Fraction {
    pub fn new(numerator: Expression, denominator: Expression) -> Self {
        Self {
            sub_expr: [numerator, denominator],
            simplified: false,
        }
    }
}

impl Fraction {
    pub fn equal(&self, other: &Addition) -> bool {
        if self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
        {
            return true;
        }

        false
    }
}

impl Fraction {
    pub fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Fraction(Box::new(self));
        }

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }
        self.simplified = true;

        match (self.sub_expr[0].clone(), self.sub_expr[1].clone()) {
            (numerator, denominator) if numerator.equal(&denominator) => Expression::Number(1),
            (Expression::Number(num1), Expression::Number(num2)) => {
                let gcd = gcd(num1, num2);
                Expression::fraction(
                    Expression::Number(num1 / gcd),
                    Expression::Number(num2 / gcd),
                )
            }
            // add plolynomiale simplification
            (_, _) => Expression::Fraction(Box::new(self)),
        }
    }
}
