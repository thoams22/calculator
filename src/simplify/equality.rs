use crate::simplify::Expression;

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
        if self.sub_expr[0].equal(&other.sub_expr[0]) && self.sub_expr[1].equal(&other.sub_expr[1])
        {
            return true;
        }

        false
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
            _ => Expression::Equality(Box::new(self)),
        }
    }
}
