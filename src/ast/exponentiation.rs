use std::collections::HashMap;

use crate::ast::Expression;

use super::{
    function::{FunctionType, PredefinedFunction},
    multiplication::Multiplication,
    negation,
    varibale::Variable,
    ConstantKind, Expr, State,
};

use crate::utils::multinomial_expansion;

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
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

impl Expr for Exponentiation {
    fn equal(&self, other: &Exponentiation) -> bool {
        self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
    }

    fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Exponentiation(Box::new(self));
        }

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }
        self.simplified = true;

        match (self.get_base(), self.get_exponent()) {
            // epxr^-neg = 1/expr^neg
            // where expr & neg are Expression Type
            (_, Expression::Negation(negation)) => Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(self.get_base(), negation.sub_expr),
            )
            .simplify(),
            // epxr^-int = 1/expr^int
            // where expr is Expression Type
            // and int is a Number
            (_, Expression::Number(num)) if num.sub_expr < 0 => Expression::fraction(
                Expression::number(1),
                Expression::exponentiation(self.get_base(), Expression::number(-num.sub_expr)),
            )
            .simplify(),
            // n^n
            // where n is a natural(N) number
            (Expression::Number(num1), Expression::Number(num2)) => {
                Expression::number(num1.sub_expr.pow((num2.sub_expr).try_into().unwrap()))
            }
            // n is an integer
            // i^(0*n) = 1,
            // i^(1*n) = i,
            // i^(2*n) = -1,
            // i^(3*n) = -i
            (Expression::ImaginaryUnit, Expression::Number(num)) => match num.sub_expr {
                c if c % 4 == 0 => Expression::number(1),
                c if c % 4 == 1 => Expression::ImaginaryUnit,
                c if c % 4 == 2 => Expression::number(-1),
                c if c % 4 == 3 => Expression::negation(Expression::ImaginaryUnit),
                _ => panic!("ImaginaryUnit exponentiated to a number where number % 4 not in [0, 1, 2, 3]")
            },
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
            // (a + b + ...)^n
            // where n is a natural(N) number
            // (Expression::Addition(add), Expression::Number(num)) => {
            //     if num.sub_expr.is_positive() {
            //         multinomial_expansion(num.sub_expr, *add)
            //     } else {
            //         self.simplify_exponent_one_zero()
            //     }
            // }
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
        if let Expression::Number(num) = self.get_exponent() {
            num.sub_expr
        } else {
            2
        }
    }

    fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        println!("{}Exponentiation :", current_span);
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
        match prev_state {
            State::Over(mut pos_y, mut pos_x) | State::Same(mut pos_y, mut pos_x) => {
                match self.get_base() {
                    Expression::Number(_)
                    | Expression::Function(_)
                    | Expression::Variable(_)
                    | Expression::Constant(_)
                    | Expression::ImaginaryUnit => {
                        self.get_base()
                            .calc_pos(position, State::Same(pos_y, pos_x), memoized);

                        pos_x += self.get_base().get_length(memoized);
                        pos_y += self.get_base().get_above_height(memoized);
                        self.get_exponent()
                            .calc_pos(position, State::Over(pos_y, pos_x), memoized);
                    }
                    _ => {
                        self.make_parenthesis(&mut pos_y, &mut pos_x, position, false, memoized);

                        self.get_base()
                            .calc_pos(position, State::Same(pos_y, pos_x), memoized);

                        pos_x += self.get_base().get_length(memoized);
                        self.make_parenthesis(&mut pos_y, &mut pos_x, position, true, memoized);
                        pos_y += self.get_base().get_above_height(memoized);

                        self.get_exponent()
                            .calc_pos(position, State::Over(pos_y, pos_x), memoized);
                    }
                }
            }
            State::Under(mut pos_y, mut pos_x) => match self.get_base() {
                Expression::Number(_)
                | Expression::Function(_)
                | Expression::Variable(_)
                | Expression::Constant(_)
                | Expression::ImaginaryUnit => {
                    pos_y -= self.get_exponent().get_height(memoized) - 1
                        + self.get_base().get_above_height(memoized);

                    self.get_base()
                        .calc_pos(position, State::Same(pos_y, pos_x), memoized);
                    pos_x += self.get_base().get_length(memoized);

                    self.get_exponent().calc_pos(
                        position,
                        State::Over(pos_y + self.get_base().get_above_height(memoized), pos_x),
                        memoized,
                    );
                }
                _ => {
                    pos_y -= self.get_exponent().get_height(memoized) - 1
                        + self.get_base().get_above_height(memoized);

                    self.make_parenthesis(&mut pos_y, &mut pos_x, position, false, memoized);

                    self.get_base()
                        .calc_pos(position, State::Same(pos_y, pos_x), memoized);
                    pos_x += self.get_base().get_length(memoized);
                    self.make_parenthesis(&mut pos_y, &mut pos_x, position, true, memoized);

                    self.get_exponent().calc_pos(
                        position,
                        State::Over(pos_y + self.get_base().get_above_height(memoized), pos_x),
                        memoized,
                    );
                }
            },
        }
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        //  y
        // x
        // __
        match self.get_base() {
            Expression::Number(_)
            | Expression::Function(_)
            | Expression::Variable(_)
            | Expression::Constant(_)
            | Expression::ImaginaryUnit => {
                self.get_base().get_length(memoized) + self.get_exponent().get_length(memoized)
            }
            _ => {
                // add parenthesis
                self.get_base().get_length(memoized) + self.get_exponent().get_length(memoized) + 2
            }
        }
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        //  y |
        // x  |
        self.get_base().get_height(memoized) + self.get_exponent().get_height(memoized)
    }

    fn get_above_height(
        &self,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        //  y |
        // x  |
        self.get_base().get_above_height(memoized) + self.get_exponent().get_height(memoized)
    }
}

impl Exponentiation {
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
        // println!("{:?}", self);
        if let Expression::Number(num) = self.get_exponent() {
            if num.sub_expr == 0 {
                return Expression::number(1);
            } else if num.sub_expr == 1 {
                return self.sub_expr[0].clone();
            } else {
                return Expression::Exponentiation(Box::new(self));
            }
        }
        Expression::Exponentiation(Box::new(self))
    }
}
