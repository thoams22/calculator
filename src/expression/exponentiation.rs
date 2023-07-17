use super::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct Exponentiation {
    sub_expr: [Expression; 2],
}

impl Exponentiation {
    pub fn new(base: Expression, exponent: Expression) -> Self {
        Self { sub_expr: [base, exponent ] }
    }

    pub fn from_vec(sub_expr: [Expression;2]) -> Self {
        Self { sub_expr }
    }

    pub fn replace_base(&mut self, other: Expression) {
        self.sub_expr[0] = other;
    }
    pub fn replace_exponent(&mut self, other: Expression) {
        self.sub_expr[1] = other;
    }

    pub fn size(&self) -> usize {
        self.sub_expr.len()
    }
    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Exponentiation(_) = other {
            let mut count = 0;
            'i: for i in 0..2 {
                for j in 0..2 {
                    if self.sub_expr[i].equal(&other.get(j).unwrap()) {
                        count += 1;
                        continue 'i;
                    }
                }
                return false;
            }

            if count == 2 {
                return true;
            } else {
                return false;
            }
        }
        false
    }

    pub fn simplify( self) -> Expression {
        let simplified_first = self.sub_expr[0].clone().simplify();
        let simplified_second = self.sub_expr[1].clone().simplify();
        match (&simplified_first, &simplified_second) {
            (Expression::Number(num1), Expression::Number(num2)) => Expression::Number(num1.powf(*num2)),
            _ => Expression::Exponentiation(Box::new(Exponentiation::new(simplified_first, simplified_second))),
        }
    }
}