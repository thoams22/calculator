use core::num;

use crate::ast::Expression;

use super::{
    addition::Addition,
    math::{gcd, prime_factor},
    multiplication::Multiplication,
};

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
                let commons_num: (Vec<(i64, Expression)>, Vec<(Expression, Expression)>) =
                    Self::factorise(num.clone());
                if commons_num != (vec![(1, Expression::Number(1))], vec![])
                    || commons_num != (vec![], vec![])
                {
                    let commons_denom: (Vec<(i64, Expression)>, Vec<(Expression, Expression)>) =
                        Self::factorise(denom.clone());
                    if commons_denom != (vec![(1, Expression::Number(1))], vec![])
                        || commons_denom != (vec![], vec![])
                    {
                        let commons: (Vec<(i64, Expression)>, Vec<(Expression, Expression)>) = (
                            commons_num
                                .0
                                .iter()
                                .filter_map(|(prime, expo)| {
                                    commons_denom
                                        .0
                                        .iter()
                                        .position(|(prime_2, expo_2)| {
                                            if prime == prime_2 {
                                                match (expo, expo_2) {
                                                    (Expression::Number(_), _) => true,
                                                    (_, Expression::Number(_)) => true,
                                                    _ => expo.equal(expo_2),
                                                }
                                            } else {
                                                false
                                            }
                                        })
                                        .map(|pos| {
                                            let exponenent = if let (
                                                Expression::Number(num),
                                                Expression::Number(num_2),
                                            ) =
                                                (expo, commons_denom.0[pos].1.clone())
                                            {
                                                Expression::Number(*num.min(&num_2))
                                            } else if let (_, Expression::Number(num_2)) =
                                                (expo, commons_denom.0[pos].1.clone())
                                            {
                                                Expression::Number(num_2)
                                            } else if let (Expression::Number(num), _) =
                                                (expo, commons_denom.0[pos].1.clone())
                                            {
                                                Expression::Number(*num)
                                            } else {
                                                expo.clone()
                                            };
                                            (*prime, exponenent)
                                        })
                                })
                                .collect(),
                            commons_num
                                .1
                                .iter()
                                .filter_map(|(prime, expo)| {
                                    commons_denom
                                        .1
                                        .iter()
                                        .position(|(prime_2, expo_2)| {
                                            if prime == prime_2 {
                                                match (expo, expo_2) {
                                                    (Expression::Number(_), _) => true,
                                                    (_, Expression::Number(_)) => true,
                                                    _ => expo.equal(expo_2),
                                                }
                                            } else {
                                                false
                                            }
                                        })
                                        .map(|pos| {
                                            let exponenent = if let (
                                                Expression::Number(num),
                                                Expression::Number(num_2),
                                            ) =
                                                (expo, commons_denom.1[pos].1.clone())
                                            {
                                                Expression::Number(*num.min(&num_2))
                                            } else if let (_, Expression::Number(num_2)) =
                                                (expo, commons_denom.1[pos].1.clone())
                                            {
                                                Expression::Number(num_2)
                                            } else if let (Expression::Number(num), _) =
                                                (expo, commons_denom.1[pos].1.clone())
                                            {
                                                Expression::Number(*num)
                                            } else {
                                                expo.clone()
                                            };
                                            (prime.clone(), exponenent)
                                        })
                                })
                                .collect(),
                        );

                        if commons == (vec![(1, Expression::Number(1))], vec![])
                            || commons == (vec![], vec![])
                        {
                            return Expression::Fraction(Box::new(self));
                        }
                        return Expression::fraction(
                            Self::delete_commons(num, commons.clone()),
                            Self::delete_commons(denom, commons),
                        )
                        .simplify();
                    }
                }

                Expression::Fraction(Box::new(self))
            }
        }
    }

    fn delete_commons(
        expr: Expression,
        commons: (Vec<(i64, Expression)>, Vec<(Expression, Expression)>),
    ) -> Expression {
        match expr {
            Expression::Number(mut num) => {
                commons.0.iter().for_each(|(prime, expo)| {
                    if let Expression::Number(num_expo) = expo {
                        num /= prime.pow(*num_expo as u32);
                    }
                });
                Expression::Number(num)
            }
            Expression::Variable(_) => {
                if commons.1.iter().any(|(expr_comm, expo)| {
                    if expr == *expr_comm {
                        matches!(expo, Expression::Number(1))
                    } else {
                        false
                    }
                }) {
                    Expression::Number(1)
                } else {
                    expr
                }
            }
            Expression::Constant(_) => {
                if commons.1.iter().any(|(expr_comm, expo)| {
                    if expr == *expr_comm {
                        matches!(expo, Expression::Number(1))
                    } else {
                        false
                    }
                }) {
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
                let mut exponenent: Expression = Expression::Number(-1);
                if commons.1.iter().any(|(expr_comm, expo_com)| {
                    if expo.get_base() == *expr_comm {
                        exponenent = expo_com.clone();
                        true
                    } else {
                        false
                    }
                }) {
                    Expression::exponentiation(
                        expo.get_base(),
                        Expression::addition(expo.get_exponent(), Expression::negation(exponenent)),
                    )
                    .simplify()
                } else if let Expression::Number(num) = expo.get_base() {
                    if commons.0.iter().any(|(expr_comm, expo_com)| {
                        if num == *expr_comm {
                            exponenent = expo_com.clone();
                            true
                        } else {
                            false
                        }
                    }) {
                        Expression::exponentiation(
                            expo.get_base(),
                            Expression::addition(
                                expo.get_exponent(),
                                Expression::negation(exponenent),
                            ),
                        )
                        .simplify()
                    } else {
                        Expression::Number(num)
                    }
                } else {
                    Expression::Exponentiation(expo)
                }
            }
            Expression::Negation(neg) => {
                Expression::negation(Self::delete_commons(neg.sub_expr, commons))
            }
            Expression::Function(_) => {
                if commons.1.iter().any(|(expr_comm, expo)| {
                    if expr == *expr_comm {
                        matches!(expo, Expression::Number(1))
                    } else {
                        false
                    }
                }) {
                    Expression::Number(1)
                } else {
                    expr
                }
            }
            _ => expr,
        }
    }

    /// `expr` must be simplified
    fn factorise(expr: Expression) -> (Vec<(i64, Expression)>, Vec<(Expression, Expression)>) {
        match expr {
            Expression::Addition(add) => {
                let mut components: Vec<(Vec<(i64, Expression)>, Vec<(Expression, Expression)>)> =
                    Vec::new();
                for sub_expr in add.sub_expr {
                    components.push(Fraction::extract_coefficient_expression_exponent(sub_expr));
                }

                let mut commons: (Vec<(i64, Expression)>, Vec<(Expression, Expression)>) =
                    components[0].clone();

                components.iter().skip(1).for_each(|component| {
                    commons.0 = commons
                        .0
                        .iter()
                        .filter_map(|(prime, expo)| {
                            component
                                .0
                                .iter()
                                .position(|(prime_2, expo_2)| {
                                    if prime == prime_2 {
                                        match (expo, expo_2) {
                                            (Expression::Number(_), Expression::Number(_)) => true,
                                            _ => expo.equal(expo_2),
                                        }
                                    } else {
                                        false
                                    }
                                })
                                .map(|pos| {
                                    let exponenent = if let (
                                        Expression::Number(num),
                                        Expression::Number(num_2),
                                    ) = (expo, component.0[pos].1.clone())
                                    {
                                        Expression::Number(*num.min(&num_2))
                                    } else {
                                        expo.clone()
                                    };
                                    (*prime, exponenent)
                                })
                        })
                        .collect();
                    commons.1 = commons
                        .1
                        .iter()
                        .filter_map(|(prime, expo)| {
                            component
                                .1
                                .iter()
                                .position(|(prime_2, expo_2)| {
                                    if prime == prime_2 {
                                        match (expo, expo_2) {
                                            (Expression::Number(_), Expression::Number(_)) => true,
                                            _ => expo.equal(expo_2),
                                        }
                                    } else {
                                        false
                                    }
                                })
                                .map(|pos| {
                                    let exponenent = if let (
                                        Expression::Number(num),
                                        Expression::Number(num_2),
                                    ) = (expo, component.1[pos].1.clone())
                                    {
                                        Expression::Number(*num.min(&num_2))
                                    } else {
                                        expo.clone()
                                    };
                                    (prime.clone(), exponenent)
                                })
                        })
                        .collect();
                });
                commons
            }
            _ => {
                let (coefficient, expressions) =
                    Fraction::extract_coefficient_expression_exponent(expr);
                (coefficient, expressions)
            }
        }
    }

    /// Expression must be simplified
    pub fn extract_coefficient_expression_exponent(
        expr: Expression,
    ) -> (Vec<(i64, Expression)>, Vec<(Expression, Expression)>) {
        let mut coefficient = 1;
        let mut expressions: Vec<(Expression, Expression)> = Vec::new();
        match expr {
            // 12a^2
            Expression::Multiplication(mult) => {
                for i in 0..mult.sub_expr.len() {
                    match &mult.sub_expr[i] {
                        // 2
                        Expression::Number(num) => {
                            coefficient *= num;
                        }
                        // a^2
                        Expression::Exponentiation(expo) => {
                            expressions.push((expo.get_base(), expo.get_exponent()));
                        }
                        _ => {
                            expressions.push((mult.sub_expr[i].clone(), Expression::Number(1)));
                        }
                    }
                }
                (
                    prime_factor(coefficient)
                        .iter()
                        .map(|(prime, expo)| (*prime, Expression::Number(*expo)))
                        .collect::<Vec<(i64, Expression)>>(),
                    expressions,
                )
            }
            Expression::Number(num) => (
                prime_factor(num)
                    .iter()
                    .map(|(prime, expo)| (*prime, Expression::Number(*expo)))
                    .collect::<Vec<(i64, Expression)>>(),
                expressions,
            ),
            Expression::Negation(neg) => {
                Self::extract_coefficient_expression_exponent(neg.sub_expr)
            }
            Expression::Exponentiation(expo) => {
                let mut coefs: Vec<(i64, Expression)> = vec![(1, Expression::Number(1))];
                if let Expression::Number(num) = expo.get_base() {
                    coefficient *= num;
                    coefs = prime_factor(coefficient)
                        .iter()
                        .map(|(prime, exp)| {
                            (
                                *prime,
                                Expression::multiplication(
                                    Expression::Number(*exp),
                                    expo.get_exponent(),
                                )
                                .simplify(),
                            )
                        })
                        .collect::<Vec<(i64, Expression)>>();
                } else {
                    expressions.push((expo.get_base(), expo.get_exponent()));
                }
                (coefs, expressions)
            }
            _ => (
                prime_factor(coefficient)
                    .iter()
                    .map(|(prime, expo)| (*prime, Expression::Number(*expo)))
                    .collect::<Vec<(i64, Expression)>>(),
                vec![(expr, Expression::Number(1))],
            ),
        }
    }
}
