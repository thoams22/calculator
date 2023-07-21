use crate::tokenizer::CalcError;

use super::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct Division {
    sub_expr: [Expression; 2],
}

impl Division {
    pub fn new(numerator: Expression, denominator: Expression) -> Self {
        if let Expression::Number(denom) = denominator {
            if denom == 0.0 {
                panic!("0 Division");
            }
        }
        Self {
            sub_expr: [numerator, denominator],
        }
    }

    pub fn from_vec(sub_expr: [Expression; 2]) -> Self {
        Self { sub_expr }
    }

    pub fn len(&self) -> usize {
        self.sub_expr.len()
    }
    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn replace_numerator(&mut self, other: Expression) {
        self.sub_expr[0] = other;
    }
    pub fn replace_denominator(&mut self, other: Expression) {
        self.sub_expr[1] = other;
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Division(_) = other {
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

            return count == 2;
        }
        false
    }

    pub fn simplify(self) -> Expression {
        let simplified_numerator = self.sub_expr[0].clone().simplify();
        let simplified_denominator = self.sub_expr[1].clone().simplify();
        match (&simplified_numerator, &simplified_denominator) {
            (Expression::Number(numerator), Expression::Number(denominator)) => {
                Expression::Number(numerator / denominator)
            }
            (Expression::Variable(var1), Expression::Variable(var2)) => {
                if var1 == var2 {
                    Expression::Number(1.0)
                } else {
                    Expression::Division(Box::new(Division::new(
                        simplified_numerator,
                        simplified_denominator,
                    )))
                }
            }
            _ => Expression::Division(Box::new(Division::new(
                simplified_numerator,
                simplified_denominator,
            ))),
        }
    }

    pub fn evaluate(&self) -> Result<f64, CalcError> {
        let mut result: f64 = 0.0;
        match self.sub_expr[0].evaluate() {
            Ok(number) => result += number,
            Err(err) => return Err(err),
        }
        match self.sub_expr[1].evaluate() {
            Ok(number) => result /= number,
            Err(err) => return Err(err),
        }

        Ok(result)
    }

    pub fn to_string(&self) -> Result<String, CalcError> {
        let mut result: String = String::from("(");
        match self.sub_expr[0].to_string() {
            Ok(number) => result = result +  &number + "/",
            Err(err) => return Err(err),
        }
        match self.sub_expr[1].to_string() {
            Ok(number) => result += &number,
            Err(err) => return Err(err),
        }
        result += ")";
        Ok(result)
    }
}
