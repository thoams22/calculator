use core::num;

use crate::ast::Expression;

use super::{addition::Addition, math::gcd, multiplication::Multiplication};

#[derive(PartialEq, Debug, Clone)]
pub struct Fraction {
    pub sub_expr: [Expression; 2],
    pub simplified: bool,
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
    pub fn equal(&self, other: &Fraction) -> bool {
        self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
    }

    pub fn get_numerator(&self) -> Expression {
        self.sub_expr[0].clone()
    }

    pub fn get_denominator(&self) -> Expression {
        self.sub_expr[1].clone()
    }

    pub fn set_numerator(&mut self, numerator: Expression) {
        self.sub_expr[0] = numerator;
    }

    pub fn set_denominator(&mut self, denominator: Expression) {
        self.sub_expr[1] = denominator;
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

        if let Expression::Number(0) = self.get_numerator() {
            return Expression::Number(0);
        } else if let Expression::Number(1) = self.get_denominator() {
            return self.get_numerator();
        }

        match (self.get_numerator(), self.get_denominator()) {
            (numerator, denominator) if numerator.equal(&denominator) => Expression::Number(1),
            (Expression::Number(num1), Expression::Number(num2)) => {
                let gcd = gcd(num1, num2);

                if gcd == 1 || gcd.is_negative() {
                    if num2 < 0 {
                        return Expression::fraction(
                            Expression::Number(-num1),
                            Expression::Number(-num2),
                        );
                    }
                    return Expression::Fraction(Box::new(self));
                }
                Expression::fraction(
                    Expression::Number(num1 / gcd),
                    Expression::Number(num2 / gcd),
                )
                .simplify()
            }
            (Expression::Fraction(frac), expr) => Expression::fraction(
                frac.get_numerator(),
                Expression::multiplication(frac.get_denominator(), expr),
            )
            .simplify(),
            (expr, Expression::Fraction(frac)) => Expression::fraction(
                Expression::multiplication(frac.get_denominator(), expr),
                frac.get_numerator(),
            )
            .simplify(),
            (expr, Expression::Negation(expr_2)) => {
                Expression::fraction(Expression::negation(expr), expr_2.sub_expr).simplify()
            }
            (num, denom) => {
                let commons_num: Vec<(i64, Vec<Expression>)> = Self::factorise(num.clone());
                if commons_num.iter().all(|comm: &(i64, Vec<Expression>)| {
                    commons_num[0].0 == comm.0 && commons_num[0].1 == comm.1
                }) {
                    let commons_denom: Vec<(i64, Vec<Expression>)> = Self::factorise(denom.clone());
                    if commons_denom.iter().all(|comm: &(i64, Vec<Expression>)| {
                        commons_denom[0].0 == comm.0 && commons_denom[0].1 == comm.1
                    }) {
                        let commons: (i64, Vec<Expression>) = (
                            if commons_num[0].0 == commons_denom[0].0 {
                                commons_num[0].0
                            } else {
                                1
                            },
                            commons_num[0]
                                .1
                                .iter()
                                .filter(|expr| commons_denom[0].1.contains(expr))
                                .cloned()
                                .collect::<Vec<Expression>>(),
                        );

                        if commons == (1, Vec::new()) {
                            return Expression::Fraction(Box::new(self));
                        }
                        return Expression::fraction(
                            Self::delete_commons(num, commons.clone()),
                            Self::delete_commons(denom, commons),
                        ).simplify();
                    }
                }

                Expression::Fraction(Box::new(self))
            }
        }
    }

    fn delete_commons(expr: Expression, commons: (i64, Vec<Expression>)) -> Expression {
        match expr {
            Expression::Number(num) => Expression::Number(num / commons.0),
            Expression::Variable(_) => {
                if commons.1.contains(&expr) {
                    Expression::Number(1)
                } else {
                    expr
                }
            }
            Expression::Constant(_) => {
                if commons.1.contains(&expr) {
                    Expression::Number(1)
                } else {
                    expr
                }
            }
            Expression::Addition(add) => {
                let mut new_add = Addition::from_vec(Vec::new());
                for sub_expr in add.sub_expr {
                    new_add
                        .sub_expr
                        .push(Self::delete_commons(sub_expr, commons.clone()));
                }
                new_add.simplify()
            }
            Expression::Multiplication(mult) => {
                let mut new_mult = Multiplication::from_vec(Vec::new());
                for sub_expr in mult.sub_expr {
                    new_mult
                        .sub_expr
                        .push(Self::delete_commons(sub_expr, commons.clone()));
                }
                new_mult.simplify()
            }
            Expression::Exponentiation(expo) => {
                if commons.1.contains(&expo.get_base()) {
                    Expression::exponentiation(
                        expo.get_base(),
                        Expression::addition(expo.get_exponent(), Expression::Number(-1)),
                    )
                    .simplify()
                } else {
                    Expression::Exponentiation(expo)
                }
            }
            Expression::Negation(neg) => {
                Expression::negation(Self::delete_commons(neg.sub_expr, commons))
            }
            Expression::Function(_) => {
                if commons.1.contains(&expr) {
                    Expression::Number(1)
                } else {
                    expr
                }
            }
            _ => expr,
        }
    }

    fn factorise(expr: Expression) -> Vec<(i64, Vec<Expression>)> {
        match expr {
            Expression::Addition(add) => {
                let mut components: Vec<(i64, Vec<(Expression, Expression)>)> = Vec::new();
                for sub_expr in add.sub_expr {
                    components.push(Fraction::extract_coefficient_expression_exponent(sub_expr));
                }
                let mut commons: Vec<(i64, Vec<Expression>)> = Vec::new();
                for (i, component) in components.windows(2).enumerate() {
                    if component[0].0 == component[1].0 {
                        if component[0].1.len() > 0 && component[1].1.len() > 0 {
                            if component[0].1[0].0 == component[1].1[0].0 {
                                commons.push((component[0].0, vec![component[0].1[0].0.clone()]));
                            }
                        } else {
                            commons.push((component[0].0, Vec::new()));
                        }
                    } else if component[0].1.len() > 0 && component[1].1.len() > 0 {
                        if component[0].1[0].0 == component[1].1[0].0 {
                            commons.push((0, vec![component[0].1[0].0.clone()]));
                        }
                    }
                    if commons.len() != i + 1 {
                        commons.push((0, Vec::new()));
                    }
                }
                commons
            }
            _ => {
                let (coefficient, expressions) =
                    Fraction::extract_coefficient_expression_exponent(expr);
                vec![(
                    coefficient,
                    expressions
                        .iter()
                        .map(|(expr, _)| expr.clone())
                        .collect::<Vec<Expression>>(),
                )]
            }
        }
    }

    pub fn extract_coefficient_expression_exponent(
        expr: Expression,
    ) -> (i64, Vec<(Expression, Expression)>) {
        let mut coefficient = 1;
        let mut expressions: Vec<(Expression, Expression)> = Vec::new();
        match expr {
            Expression::Multiplication(mult) => {
                for i in 0..mult.sub_expr.len() {
                    match &mult.sub_expr[i] {
                        Expression::Number(num) => {
                            coefficient *= num;
                        }
                        Expression::Exponentiation(expo) => {
                            expressions.push((expo.get_base(), expo.get_exponent()));
                        }
                        _ => {
                            expressions.push((mult.sub_expr[i].clone(), Expression::Number(1)));
                        }
                    }
                }
                (coefficient, expressions)
            }
            Expression::Number(num) => {
                coefficient *= num;
                (coefficient, expressions)
            }
            Expression::Negation(neg) => {
                coefficient *= -1;
                match neg.sub_expr {
                    Expression::Number(num) => {
                        coefficient *= num;
                    }
                    Expression::Exponentiation(expo) => {
                        expressions.push((expo.get_base(), expo.get_exponent()));
                    }
                    _ => {
                        expressions.push((neg.sub_expr, Expression::Number(1)));
                    }
                }
                (coefficient, expressions)
            }
            Expression::Exponentiation(expo) => {
                expressions.push((expo.get_base(), expo.get_exponent()));
                (coefficient, expressions)
            }
            _ => (coefficient, vec![(expr, Expression::Number(1))]),
        }
    }
}