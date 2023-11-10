use std::{collections::HashMap, fmt::Display};

use crate::ast::Expression;

use super::{varibale::Variable, Expr, State};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Equality {
    pub sub_expr: [Expression; 2],
    pub simplified: bool,
}

impl Equality {
    pub fn new(left: Expression, right: Expression) -> Self {
        Self {
            sub_expr: [left, right],
            simplified: false,
        }
    }

    pub fn get_left_side(&self) -> Expression {
        self.sub_expr[0].clone()
    }

    pub fn get_right_side(&self) -> Expression {
        self.sub_expr[1].clone()
    }

    pub fn replace_left_side(&mut self, new_left_side: Expression) {
        self.sub_expr[0] = new_left_side;
    }

    pub fn replace_right_side(&mut self, new_right_side: Expression) {
        self.sub_expr[1] = new_right_side;
    }
}

impl Expr for Equality {
    fn equal(&self, other: &Equality) -> bool {
        self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
    }

    fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Equality(Box::new(self));
        }

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }
        self.simplified = true;

        match (self.sub_expr[0].clone(), self.sub_expr[1].clone()) {
            (left, right) if left.equal(&right) => {
                Expression::equality(Expression::number(0), Expression::number(0))
            }
            (Expression::Negation(neg_1), Expression::Negation(neg_2)) => {
                Expression::equality(neg_1.sub_expr, neg_2.sub_expr)
            }
            (Expression::Negation(neg), Expression::Number(num))
            | (Expression::Number(num), Expression::Negation(neg))
                if num.sub_expr < 0 =>
            {
                Expression::equality(neg.sub_expr, Expression::number(num.sub_expr.abs()))
            }
            (Expression::Number(num_1), Expression::Number(num_2)) if num_1.sub_expr < 0 && num_2.sub_expr < 0 => {
                Expression::equality(
                    Expression::number(num_1.sub_expr.abs()),
                    Expression::number(num_2.sub_expr.abs()),
                )
            }
            _ => Expression::Equality(Box::new(self)),
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
        self.sub_expr
            .iter()
            .map(|expr| expr.get_order())
            .max()
            .unwrap_or(0)
    }

    fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        println!("{}Equality :", current_span);
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
        let (pos_y, mut pos_x) = prev_state.get_pos();
        self.get_left_side()
            .calc_pos(position, prev_state, memoized);
        pos_x += self.get_left_side().get_length(memoized);
        position.push((" = ".to_string(), (pos_y, pos_x)));
        pos_x += 3;
        self.get_right_side()
            .calc_pos(position, State::Same(pos_y, pos_x), memoized);
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        self.get_left_side().get_length(memoized) + self.get_right_side().get_length(memoized)
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        self.get_left_side()
            .get_height(memoized)
            .max(self.get_right_side().get_height(memoized))
    }

    fn get_above_height(
        &self,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        self.get_left_side()
            .get_above_height(memoized)
            .max(self.get_right_side().get_above_height(memoized))
    }
}

impl Display for Equality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.get_left_side(), self.get_right_side())
    }
}
