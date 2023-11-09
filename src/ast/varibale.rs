use std::{collections::HashMap, fmt::Display, fmt::Formatter};

use super::State;

use  super::{Expr, Expression};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Variable {
    pub sub_expr: String,
}

impl Variable {
    pub fn new(sub_expr: String) -> Self {
        Self { sub_expr }
    }
}

impl Expr for Variable {
    fn equal(&self, other: &Variable) -> bool {
        self.sub_expr == other.sub_expr
    }

    fn contain_vars(&self) -> Option<std::collections::HashMap<Variable, usize>> {
        let mut map: HashMap<Variable, usize> = HashMap::new();
        map.insert(self.clone(), 1);
        Some(map)
    }

    fn contain_var(&self, variable: &Variable) -> bool {
        self == variable
    }

    fn simplify(self) ->  Expression {
        Expression::Variable(self)
    }

    fn get_order(&self) -> i64 {
        1
    }

    fn print_tree(&self, span: Option<&str>) {
        println!("{}{}", span.unwrap_or(""), self.sub_expr)
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: State,
        _memoized: &mut std::collections::HashMap< Expression, (i8, i8, i8)>,
    ) {
        position.push((self.sub_expr.clone(), prev_state.get_pos()))

    }

    fn get_length(
        &self,
        _memoized: &mut std::collections::HashMap< Expression, (i8, i8, i8)>,
    ) -> i8 {
        self.sub_expr.len() as i8
    }

    fn get_height(
        &self,
        _memoized: &mut std::collections::HashMap< Expression, (i8, i8, i8)>,
    ) -> i8 {
        1
    }

    fn get_above_height(
        &self,
        _memoized: &mut std::collections::HashMap< Expression, (i8, i8, i8)>,
    ) -> i8 {
        1
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.sub_expr)
    }
}
