use std::{collections::HashMap, fmt::Display};

use crate::ast::Expression;

use super::{varibale::Variable, Expr};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Number {
    pub sub_expr: i64,
}

impl Number {
    pub fn new(sub_expr: i64) -> Self {
        Self { sub_expr }
    }
}

impl Expr for Number {
    fn equal(&self, other: &Number) -> bool {
        self.sub_expr == other.sub_expr
    }

    fn simplify(self) -> Expression {
        Expression::Number(self)
    }

    fn contain_vars(&self) -> Option<HashMap<Variable, usize>> {
        None
    }

    fn contain_var(&self, _variable: &Variable) -> bool {
        false
    }

    fn get_order(&self) -> i64 {
        0
    }

    fn print_tree(&self, span: Option<&str>) {
        println!("{}{}", span.unwrap_or(""), self.sub_expr)
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        _memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) {
        position.push((self.sub_expr.to_string(), prev_state.get_pos()))
    }

    fn get_length(&self, _memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        self.sub_expr.to_string().len() as i8
    }

    fn get_height(&self, _memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        1
    }

    fn get_above_height(
        &self,
        _memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        1
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.sub_expr)
    }
}
