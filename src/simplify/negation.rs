
use crate::simplify::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct Negation {
    sub_expr: Expression,
    simplified: bool,
}

impl Negation {
    pub fn new(sub_expr: Expression) -> Self {
        Self {
            sub_expr,
            simplified: false,
        }
    }
}

impl Negation {
    pub fn equal(&self, other: &Negation) -> bool {
        if self.sub_expr.equal(&other.sub_expr) {
            return true;
        }

        false
    }
}

impl Negation {
    pub fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Negation(Box::new(self));
        }

        self.simplified = true;
        self.sub_expr = self.sub_expr.clone().simplify();

        match self.sub_expr {
            Expression::Negation(expr) => expr.sub_expr,
            Expression::Number(num) => Expression::Number(-num),
            _ => Expression::Negation(Box::new(self)),
        }
    }
}
