use std::{collections::HashMap, fmt::Display};

use crate::ast::Expression;

use super::{varibale::Variable, Expr, State};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Negation {
    pub sub_expr: Expression,
    pub simplified: bool,
}

impl Negation {
    pub fn new(sub_expr: Expression) -> Self {
        Self {
            sub_expr,
            simplified: false,
        }
    }
}

impl Expr for Negation {
    fn equal(&self, other: &Negation) -> bool {
        self.sub_expr.equal(&other.sub_expr)
    }

    fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Negation(Box::new(self));
        }

        self.simplified = true;
        self.sub_expr = self.sub_expr.clone().simplify();

        match self.sub_expr {
            Expression::Negation(expr) => expr.sub_expr,
            Expression::Number(num) => Expression::number(-num.sub_expr),
            Expression::Addition(add) => {
                let mut addition = add.clone();
                addition.simplified = false;
                addition.sub_expr = add
                    .sub_expr
                    .iter()
                    .map(|expr| Expression::negation(expr.clone()).simplify())
                    .collect();
                addition.simplify()
            }
            Expression::Multiplication(mult) => {
                let mut multiplication = mult.clone();
                multiplication.simplified = false;
                let first = Expression::negation(mult.sub_expr[0].clone()).simplify();
                multiplication.sub_expr[0] = first;
                multiplication.simplify()
            }
            Expression::Fraction(frac) => {
                let mut fraction = frac.clone();
                fraction.simplified = false;
                fraction
                    .set_numerator(Expression::negation(frac.get_numerator().clone()).simplify());
                fraction.simplify()
            }
            _ => Expression::Negation(Box::new(self)),
        }
    }

    fn contain_vars(&self) -> Option<HashMap<Variable, usize>> {
        let mut map: HashMap<Variable, usize> = HashMap::new();
        if let Some(mut sub_map) = self.sub_expr.contain_vars() {
            for (key, value) in sub_map.iter_mut() {
                if let Some(occurence) = map.get_mut(key) {
                    *occurence += *value;
                } else {
                    map.insert(key.clone(), *value);
                }
            }
        }

        if map.is_empty() {
            None
        } else {
            Some(map)
        }
    }

    fn contain_var(&self, variable: &Variable) -> bool {
        self.sub_expr.contain_var(variable)
    }

    fn get_order(&self) -> i64 {
        self.sub_expr.get_order()
    }

    fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        println!("{}Negation :", current_span);
        print!("{}", current_span);
        self.sub_expr.print_tree(Some(&new_span));
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) {
        let (mut pos_y, mut pos_x) = prev_state.get_pos();

        position.push(("-".to_string(), (pos_y, pos_x)));
        pos_x += 1;

        self.make_parenthesis(&mut pos_y, &mut pos_x, position, false, memoized); 

        self.sub_expr
            .calc_pos(position, State::Same(pos_y, pos_x), memoized);
        pos_x += self.sub_expr.get_length(memoized);
        
        self.make_parenthesis(&mut pos_y, &mut pos_x, position, true, memoized);
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        self.sub_expr.get_length(memoized) + 1
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        self.sub_expr.get_height(memoized)
    }

    fn get_above_height(
        &self,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        self.sub_expr.get_above_height(memoized)
    }
}

impl Display for Negation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "-({})", self.sub_expr)
    }
}