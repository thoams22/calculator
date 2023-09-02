use crate::tokenizer::CalcError;

use super::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct Equality {
    sub_expr: [Expression; 2],
    simplified: bool,
}

impl Equality {
    pub fn new(left_side: Expression, right_side: Expression) -> Self {
        Self {
            sub_expr: [left_side, right_side],
            simplified: false,
        }
    }

    pub fn replace_left_side(&mut self, other: Expression) {
        self.sub_expr[0] = other;
    }

    pub fn replace_right_side(&mut self, other: Expression) {
        self.sub_expr[1] = other;
    }

    pub fn get_left_side(&self) -> Expression {
        self.sub_expr[0].clone()
    }

    pub fn get_right_side(&self) -> Expression {
        self.sub_expr[1].clone()
    }

    pub fn len(&self) -> usize {
        self.sub_expr.len()
    }

    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Equality(second_equality) = other {
            if (self.get_left_side().equal(&second_equality.get_left_side())
                && self
                    .get_right_side()
                    .equal(&second_equality.get_right_side()))
                || (self
                    .get_right_side()
                    .equal(&second_equality.get_left_side())
                    && self
                        .get_left_side()
                        .equal(&second_equality.get_right_side()))
            {
                return true;
            }
        }
        false
    }

    pub fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Equality(Box::new(self));
        }

        for expression in self.sub_expr.iter_mut() {
            *expression = expression.clone().simplify();
        }

        Expression::Equality(Box::new(self))
    }

    pub fn to_string(&self) -> Result<String, CalcError> {
        let mut result: String = String::new();
        match self.sub_expr[0].to_string() {
            Ok(number) => result = number + " = ",
            Err(err) => return Err(err),
        }
        match self.sub_expr[1].to_string() {
            Ok(number) => result += &number,
            Err(err) => return Err(err),
        }
        Ok(result)
    }
}
