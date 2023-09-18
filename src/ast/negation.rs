use crate::ast::Expression;

#[derive(PartialEq, Debug, Clone)]
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

impl Negation {
    pub fn equal(&self, other: &Negation) -> bool {
        self.sub_expr.equal(&other.sub_expr)
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
            Expression::Addition(add) => {
                let mut addition = add.clone();
                addition.simplified = false;
                addition.sub_expr = add.sub_expr.iter().map(|expr| Expression::negation(expr.clone()).simplify()).collect();
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
                fraction.set_numerator(Expression::negation(frac.get_numerator().clone()).simplify());
                fraction.simplify()
            }
            _ => Expression::Negation(Box::new(self)),
        }
    }
}
