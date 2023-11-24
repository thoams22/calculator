use std::collections::HashMap;

use crate::ast::Expression;

use super::{addition::Addition, multiplication::Multiplication, varibale::Variable, Expr, State};

use crate::utils::{extract_coefficient_expression_exponent, gcd, ExpressionExponent, PrimeFactor};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
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

impl Expr for Fraction {
    fn equal(&self, other: &Fraction) -> bool {
        self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
    }

    fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Fraction(Box::new(self));
        }

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }
        self.simplified = true;

        if let Expression::Number(num) = self.get_numerator() {
            if num.sub_expr == 0 {
                return Expression::number(0);
            }
        } 
        if let Expression::Number(num) = self.get_denominator() {
            if num.sub_expr == 1 {
                return self.get_numerator();
            }
        }

        match (self.get_numerator(), self.get_denominator()) {
            (numerator, denominator) if numerator.equal(&denominator) => Expression::number(1),
            (Expression::Number(num1), Expression::Number(num2)) => {
                let gcd = gcd(num1.sub_expr, num2.sub_expr);

                if gcd == 1 || gcd.is_negative() {
                    if num2.sub_expr < 0 {
                        return Expression::fraction(
                            Expression::number(-num1.sub_expr),
                            Expression::number(-num2.sub_expr),
                        );
                    }
                    return Expression::Fraction(Box::new(self));
                }
                Expression::fraction(
                    Expression::number(num1.sub_expr / gcd),
                    Expression::number(num2.sub_expr / gcd),
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
            (num, denom) => match Self::simplify_common_factor(num, denom) {
                Ok(expr_simpl) => expr_simpl,
                Err(_) => Expression::Fraction(Box::new(self)),
            },
        }
    }

    fn contain_vars(&self) -> Option<std::collections::HashMap<super::varibale::Variable, usize>> {
        let mut map: HashMap<Variable, usize> = HashMap::new();
        for expr in self.sub_expr.iter() {
            if let Some(mut sub_map) = expr.contain_vars() {
                for (key, value) in sub_map.iter_mut() {
                    if let Some(occurence) = map.get_mut(key) {
                        *occurence += *value;
                    } else {
                        map.insert(key.clone(), *value);
                    }
                }
            }
        }
        if map.is_empty() {
            None
        } else {
            Some(map)
        }
    }

    fn contain_var(&self, variable: &super::varibale::Variable) -> bool {
        self.sub_expr.iter().any(|expr| expr.contain_var(variable))
    }

    fn get_order(&self) -> i64 {
        self.get_numerator()
            .get_order()
            .max(self.get_denominator().get_order())
    }

    fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        println!("{}Fraction :", current_span);
        print!("{}", current_span);
        self.sub_expr[0].print_tree(Some(&new_span));
        print!("{}", current_span);
        self.sub_expr[1].print_tree(Some(&new_span));
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) {
        // used to calculate the position of the fraction bar
        let (pos_y, mut pos_x) = match prev_state {
            State::Over(pos_y, pos_x) => {
                (pos_y + self.get_denominator().get_height(memoized), pos_x)
            }
            State::Under(pos_y, pos_x) => {
                (pos_y - self.get_numerator().get_height(memoized), pos_x)
            }
            State::Same(pos_y, pos_x) => (pos_y, pos_x),
        };

        let num_length = self.get_numerator().get_length(memoized);
        let denom_length = self.get_denominator().get_length(memoized);

        // determines the length of the fraction bar
        // and center the shortest part
        if denom_length < num_length {
            for i in 0..num_length {
                position.push(("-".to_string(), (pos_y, pos_x + i)));
            }
            self.get_numerator()
                .calc_pos(position, State::Over(pos_y + 1, pos_x), memoized);
            pos_x += (num_length - denom_length) / 2;
            self.get_denominator()
                .calc_pos(position, State::Under(pos_y - 1, pos_x), memoized);
        } else {
            for i in 0..denom_length {
                position.push(("-".to_string(), (pos_y, pos_x + i)));
            }
            self.get_denominator()
                .calc_pos(position, State::Under(pos_y - 1, pos_x), memoized);
            pos_x += (denom_length - num_length) / 2;
            self.get_numerator()
                .calc_pos(position, State::Over(pos_y + 1, pos_x), memoized);
        }
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        //  num
        // -----
        // denom
        // _____

        self.get_numerator()
            .get_length(memoized)
            .max(self.get_denominator().get_length(memoized))
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        // num |
        // --- |
        // den |
        self.get_numerator().get_height(memoized) + self.get_denominator().get_height(memoized) + 1
    }

    fn get_above_height(
        &self,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        // num |
        // --- |
        // den
        self.get_numerator().get_height(memoized) + 1
    }
}

impl Fraction {
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

    fn simplify_common_factor(num: Expression, denom: Expression) -> Result<Expression, ()> {
        let commons_num: (Vec<PrimeFactor>, Vec<ExpressionExponent>) = Self::factorise(num.clone());
        if commons_num != (vec![PrimeFactor::new(1, Expression::number(1))], vec![])
            || commons_num != (vec![], vec![])
        {
            let commons_denom: (Vec<PrimeFactor>, Vec<ExpressionExponent>) =
                Self::factorise(denom.clone());
            if commons_denom != (vec![PrimeFactor::new(1, Expression::number(1))], vec![])
                || commons_denom != (vec![], vec![])
            {
                let commons: (Vec<PrimeFactor>, Vec<ExpressionExponent>) =
                    Self::isolate_commons(commons_num, commons_denom);

                if commons == (vec![PrimeFactor::new(1, Expression::number(1))], vec![])
                    || commons == (vec![], vec![])
                {
                    return Err(());
                }
                return Ok(Expression::fraction(
                    Self::delete_commons(num, commons.clone()),
                    Self::delete_commons(denom, commons),
                )
                .simplify());
            }
        }

        Err(())
    }

    fn isolate_commons(
        commons_num: (Vec<PrimeFactor>, Vec<ExpressionExponent>),
        commons_denom: (Vec<PrimeFactor>, Vec<ExpressionExponent>),
    ) -> (Vec<PrimeFactor>, Vec<ExpressionExponent>) {
        (
            commons_num
                .0
                .iter()
                .filter_map(|prime_factor| {
                    commons_denom
                        .0
                        .iter()
                        .position(|prime_factor_2| {
                            if prime_factor.prime == prime_factor_2.prime {
                                match (&prime_factor.exponent, &prime_factor_2.exponent) {
                                    (Expression::Number(_), _) => true,
                                    (_, Expression::Number(_)) => true,
                                    _ => prime_factor.exponent.equal(&prime_factor.exponent),
                                }
                            } else {
                                false
                            }
                        })
                        .map(|pos| {
                            let exponenent =
                                match (&prime_factor.exponent, &commons_denom.0[pos].exponent) {
                                    (Expression::Number(num), Expression::Number(num_2)) => {
                                        Expression::number(num.sub_expr.min(num_2.sub_expr))
                                    }
                                    (_, Expression::Number(num_2)) => Expression::number(num_2.sub_expr),
                                    (Expression::Number(num), _) => Expression::number(num.sub_expr),
                                    _ => prime_factor.exponent.clone(),
                                };
                            PrimeFactor::new(prime_factor.prime, exponenent)
                        })
                })
                .collect(),
            commons_num
                .1
                .iter()
                .filter_map(|expr_expo| {
                    commons_denom
                        .1
                        .iter()
                        .position(|expr_expo_2| {
                            if expr_expo.expression == expr_expo_2.expression {
                                match (&expr_expo.exponent, &expr_expo_2.exponent) {
                                    (Expression::Number(_), _) | (_, Expression::Number(_)) => true,
                                    (Expression::Variable(var), expr)
                                    | (expr, Expression::Variable(var)) => expr.contain_var(var),
                                    _ => expr_expo.exponent.equal(&expr_expo_2.exponent),
                                }
                            } else {
                                false
                            }
                        })
                        .map(|pos| {
                            println!("{}:{}", expr_expo.exponent, commons_denom.1[pos].exponent);
                            let exponenent =
                                match (&expr_expo.exponent, &commons_denom.1[pos].exponent) {
                                    (Expression::Number(num), Expression::Number(num_2)) => {
                                        Expression::number(num.sub_expr.min(num_2.sub_expr))
                                    }
                                    (_, Expression::Number(num_2)) => Expression::number(num_2.sub_expr),
                                    (Expression::Number(num), _) => Expression::number(num.sub_expr),
                                    (Expression::Variable(var), expr)
                                    | (expr, Expression::Variable(var)) => {
                                        let commons_expo = Self::factorise(expr.clone());
                                        let commons = Self::isolate_commons(
                                            commons_expo,
                                            (
                                                vec![],
                                                vec![ExpressionExponent::new(
                                                    Expression::Variable(var.clone()),
                                                    Expression::number(1),
                                                )],
                                            ),
                                        );
                                        if commons.1.len() == 1
                                            && commons.1[0].exponent == Expression::number(1)
                                        {
                                            commons.1[0].expression.clone()
                                        } else {
                                            Expression::exponentiation(
                                                commons.1[0].expression.clone(),
                                                commons.1[0].expression.clone(),
                                            )
                                        }
                                        // expr_expo.exponent.clone()
                                    }
                                    _ => expr_expo.exponent.clone(),
                                };
                            ExpressionExponent::new(expr_expo.expression.clone(), exponenent)
                        })
                })
                .collect(),
        )
    }

    fn delete_commons(
        expr: Expression,
        commons: (Vec<PrimeFactor>, Vec<ExpressionExponent>),
    ) -> Expression {
        match expr {
            Expression::Number(mut num) => {
                commons.0.iter().for_each(|prime_factor| {
                    if let Expression::Number(num_expo) = &prime_factor.exponent {
                        num.sub_expr /= prime_factor.prime.pow(num_expo.sub_expr as u32);
                    }
                });
                Expression::number(num.sub_expr)
            }
            Expression::Variable(_) => {
                if commons.1.iter().any(|expression_exponent| {
                    if expr == expression_exponent.expression {
                        matches!(&expression_exponent.exponent, Expression::Number(num) if num.sub_expr == 1)
                    } else {
                        false
                    }
                }) {
                    Expression::number(1)
                } else {
                    expr
                }
            }
            Expression::Constant(_) => {
                if commons.1.iter().any(|expression_exponent| {
                    if expr == expression_exponent.expression {
                        matches!(&expression_exponent.exponent, Expression::Number(num) if num.sub_expr == 1)
                    } else {
                        false
                    }
                }) {
                    Expression::number(1)
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
                let mut exponenent: Expression = Expression::number(-1);
                if commons.1.iter().any(|expression_exponent| {
                    if expo.get_base() == expression_exponent.expression {
                        exponenent = expression_exponent.exponent.clone();
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
                    if commons.0.iter().any(|prime_factor| {
                        if num.sub_expr == prime_factor.prime {
                            exponenent = prime_factor.exponent.clone();
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
                        Expression::number(num.sub_expr)
                    }
                } else {
                    Expression::Exponentiation(expo)
                }
            }
            Expression::Negation(neg) => {
                Expression::negation(Self::delete_commons(neg.sub_expr, commons))
            }
            Expression::Function(_) => {
                if commons.1.iter().any(|expression_exponent| {
                    if expr == expression_exponent.expression {
                        matches!(&expression_exponent.exponent, Expression::Number(num) if num.sub_expr == 1)
                    } else {
                        false
                    }
                }) {
                    Expression::number(1)
                } else {
                    expr
                }
            }
            _ => expr,
        }
    }

    /// Return the common part in the expression if its an addition else return the uniques subcomposant and their power
    ///
    /// ### Exemple
    /// ```
    /// // 4x^2
    /// let expression = Expression::multiplication(
    ///     Expression::number(4),
    ///     Expression::exponentiation(Expression::Variable('x'), Expression::number(2)));
    ///
    /// // ((2, 2), (x, 2))
    /// let factors = factorise(expression);
    /// ```
    fn factorise(expr: Expression) -> (Vec<PrimeFactor>, Vec<ExpressionExponent>) {
        match expr {
            Expression::Addition(add) => {
                let mut components: Vec<(Vec<PrimeFactor>, Vec<ExpressionExponent>)> = Vec::new();
                for sub_expr in add.sub_expr {
                    components.push(extract_coefficient_expression_exponent(sub_expr));
                }

                let mut commons: (Vec<PrimeFactor>, Vec<ExpressionExponent>) =
                    components[0].clone();

                components.iter().skip(1).for_each(|component| {
                    commons.0 = commons
                        .0
                        .iter()
                        .filter_map(|prime_factor| {
                            component
                                .0
                                .iter()
                                .position(|prime_factor_2| {
                                    if prime_factor.prime == prime_factor_2.prime {
                                        match (&prime_factor.exponent, &prime_factor_2.exponent) {
                                            (Expression::Number(_), Expression::Number(_)) => true,
                                            _ => prime_factor
                                                .exponent
                                                .equal(&prime_factor_2.exponent),
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
                                        (&prime_factor.exponent, &component.0[pos].exponent)
                                    {
                                        Expression::number(num.sub_expr.min(num_2.sub_expr))
                                    } else {
                                        prime_factor.exponent.clone()
                                    };
                                    PrimeFactor::new(prime_factor.prime, exponenent)
                                })
                        })
                        .collect();
                    commons.1 = commons
                        .1
                        .iter()
                        .filter_map(|expr| {
                            component
                                .1
                                .iter()
                                .position(|expr_2| {
                                    if expr.expression == expr_2.expression {
                                        match (&expr.exponent, &expr.exponent) {
                                            (Expression::Number(_), Expression::Number(_)) => true,
                                            _ => expr.exponent.equal(&expr.exponent),
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
                                        (&expr.exponent, &component.1[pos].exponent.clone())
                                    {
                                        Expression::number(num.sub_expr.min(num_2.sub_expr))
                                    } else {
                                        expr.exponent.clone()
                                    };
                                    ExpressionExponent::new(expr.expression.clone(), exponenent)
                                })
                        })
                        .collect();
                });
                commons
            }
            _ => {
                let (prime_factor, expressions) = extract_coefficient_expression_exponent(expr);
                (prime_factor, expressions)
            }
        }
    }
}
