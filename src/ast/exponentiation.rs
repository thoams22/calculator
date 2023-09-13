use crate::ast::Expression;

use super::{
    function::{FunctionType, PredefinedFunction},
    math::multinomial_expansion,
    multiplication::Multiplication,
    ConstantKind,
};

#[derive(PartialEq, Debug, Clone)]
pub struct Exponentiation {
    pub sub_expr: [Expression; 2],
    pub simplified: bool,
}

impl Exponentiation {
    pub fn new(base: Expression, exponent: Expression) -> Self {
        Self {
            sub_expr: [base, exponent],
            simplified: false,
        }
    }
}

impl Exponentiation {
    pub fn equal(&self, other: &Exponentiation) -> bool {
        self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
    }

    pub fn get_base(&self) -> Expression {
        self.sub_expr[0].clone()
    }

    pub fn get_exponent(&self) -> Expression {
        self.sub_expr[1].clone()
    }

    pub fn set_base(&mut self, base: Expression) {
        self.sub_expr[0] = base;
    }

    pub fn set_exponent(&mut self, exponent: Expression) {
        self.sub_expr[1] = exponent;
    }
}

impl Exponentiation {
    pub fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Exponentiation(Box::new(self));
        }

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }
        self.simplified = true;

        match (self.get_base(), self.get_exponent()) {
            (Expression::Number(num1), Expression::Number(num2)) => {
                Expression::Number(num1.pow((num2).try_into().unwrap()))
            }
            (_, Expression::Function(fun)) => self.simplify_exponent_logarithm(&fun),
            // (a*b)^c = a^c * b^c
            (Expression::Multiplication(mult), exponent) => {
                let mut distributed_exponent = Multiplication::from_vec(vec![]);
                mult.iter().for_each(|expr| {
                    distributed_exponent
                        .sub_expr
                        .push(Expression::exponentiation(expr.clone(), exponent.clone()))
                });
                Expression::Multiplication(Box::new(distributed_exponent)).simplify()
            }
            (Expression::Addition(add), Expression::Number(num)) => {
                if num.is_positive() {
                    multinomial_expansion(num, *add)
                } else {
                    self.simplify_exponent_one_zero()
                }
            }
            // (a/b)^c = (a^c)/(b^c)
            (Expression::Fraction(frac), exponent) => Expression::fraction(
                Expression::exponentiation(frac.get_numerator(), exponent.clone()),
                Expression::exponentiation(frac.get_denominator(), exponent),
            )
            .simplify(),
            // (a^b)^c = a^(b*c)
            (Expression::Exponentiation(expression), exponent) => Expression::exponentiation(
                expression.get_base(),
                Expression::multiplication(expression.get_exponent(), exponent.clone()),
            )
            .simplify(),
            (_, _) => self.simplify_exponent_one_zero(),
        }
    }

    // e^ln(x) = x
    // a^log(a, x) = x
    pub fn simplify_exponent_logarithm(self, function: &FunctionType) -> Expression {
        if let FunctionType::Predefined(fun, args) = function {
            match fun {
                PredefinedFunction::Ln => {
                    if self
                        .get_base()
                        .equal(&Expression::Constant(ConstantKind::E))
                    {
                        return args[0].clone();
                    }
                }
                PredefinedFunction::Log => {
                    if self.get_base().equal(&args[0]) {
                        return args[1].clone();
                    }
                }
                _ => (),
            }
        }

        Expression::Exponentiation(Box::new(self))
    }

    fn simplify_exponent_one_zero(self) -> Expression {
        if let Expression::Number(num) = self.get_exponent() {
            if num == 0 {
                return Expression::Number(1);
            } else if num == 1 {
                return self.sub_expr[0].clone();
            } else {
                return Expression::Exponentiation(Box::new(self));
            }
        }
        Expression::Exponentiation(Box::new(self))
    }
}
