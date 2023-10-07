use std::fmt::Display;

use crate::ast::Expression;

#[derive(PartialEq, Debug, Clone)]
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
}

impl Equality {
    pub fn equal(&self, other: &Equality) -> bool {
        self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
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

impl Equality {
    pub fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Equality(Box::new(self));
        }

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }
        self.simplified = true;

        match (self.sub_expr[0].clone(), self.sub_expr[1].clone()) {
            (left, right) if left.equal(&right) => {
                Expression::equality(Expression::Number(0), Expression::Number(0))
            }
            (Expression::Negation(neg_1), Expression::Negation(neg_2)) => {
                Expression::equality(neg_1.sub_expr, neg_2.sub_expr)
            }
            (Expression::Negation(neg), Expression::Number(num))
            | (Expression::Number(num), Expression::Negation(neg))
                if num < 0 =>
            {
                Expression::equality(neg.sub_expr, Expression::Number(num.abs()))
            }
            (Expression::Number(num_1), Expression::Number(num_2))
                if num_1 < 0 &&  num_2 < 0 =>
            {
                Expression::equality(Expression::Number(num_1.abs()), Expression::Number(num_2.abs()))
            }
            _ => {
                Expression::Equality(Box::new(self))
            }
        }
    }
}

impl Display for Equality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.get_left_side(), self.get_right_side())
    }
}