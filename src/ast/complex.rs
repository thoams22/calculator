use std::{collections::HashMap, fmt::Display};

use crate::ast::Expression;

use super::{
    function::{FunctionType, PredefinedFunction},
    varibale::Variable,
    Expr, State,
};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Complex {
    // a + bi
    pub sub_expr: [Expression; 2],
    pub simplified: bool,
}

impl Complex {
    pub fn new(real: Expression, imaginary: Expression) -> Self {
        Self {
            sub_expr: [real, imaginary],
            simplified: false,
        }
    }

    // a + bi
    
    // a
    pub fn get_real(&self) -> Expression {
        self.sub_expr[0].clone()
    }
    // b
    pub fn get_imaginary(&self) -> Expression {
        self.sub_expr[1].clone()
    }

    pub fn set_real(&mut self, real: Expression) {
        self.sub_expr[0]= real
    }

    pub fn set_imaginary(&mut self, imaginary: Expression) {
        self.sub_expr[1] = imaginary
    }

    // re^(i*theta)

    // r
    pub fn get_modulus(self) -> Expression {
        Expression::function(FunctionType::Predefined(
            PredefinedFunction::Sqrt,
            vec![Expression::addition(
                Expression::exponentiation(self.get_real(), Expression::number(2)),
                Expression::exponentiation(self.get_imaginary(), Expression::number(2)),
            )],
        ))
        .simplify()
    }

    // theta
    pub fn get_argument(self) -> Expression {
        Expression::function(FunctionType::Predefined(
            PredefinedFunction::Atan,
            vec![Expression::fraction(self.get_imaginary(), self.get_real())],
        ))
        .simplify()
    }
}

impl Expr for Complex {
    fn equal(&self, other: &Complex) -> bool {
        self.get_real().equal(&other.get_real())
            && self.get_imaginary().equal(&other.get_imaginary())
    }

    fn simplify(mut self) -> Expression {
        // TODO add imaginary_exponent simplification
        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }
        self.simplified = true;

        Expression::Complex(Box::new(self))
    }

    fn contain_vars(&self) -> Option<HashMap<Variable, usize>> {
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

    fn contain_var(&self, variable: &Variable) -> bool {
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
        println!("{}Complex :", current_span);
        print!("{}", current_span);
        self.get_real().print_tree(Some(&new_span));
        print!("{}", current_span);
        self.get_imaginary().print_tree(Some(&new_span));
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) {
        let (pos_y, mut pos_x) = prev_state.get_pos();
        self.get_real().calc_pos(position, prev_state, memoized);
        pos_x += self.get_real().get_length(memoized);
        position.push((" = ".to_string(), (pos_y, pos_x)));
        pos_x += 3;
        self.get_imaginary()
            .calc_pos(position, State::Same(pos_y, pos_x), memoized);
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        self.get_real().get_length(memoized) + self.get_imaginary().get_length(memoized) + 4
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        let mut max_height = 0;
        for expr in &self.sub_expr {
            max_height = max_height.max(expr.get_height(memoized));
        }
        max_height
    }

    fn get_above_height(
        &self,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        let mut max_height = 0;
        for expr in &self.sub_expr {
            max_height = max_height.max(expr.get_above_height(memoized));
        }
        max_height
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} + {}", self.get_real(), self.get_imaginary())
    }
}
