use crate::tokenizer::CalcError;

use super::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct Division {
    sub_expr: [Expression; 2],
    simplified: bool,
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
            simplified: false,
        }
    }

    pub fn from_vec(sub_expr: [Expression; 2]) -> Self {
        Self { sub_expr , simplified: false}
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
            if self.len() == other.len() {
                let len = self.len();
                let mut index: Vec<bool> = vec![false; len*2];
                'i: for i in 0..len {
                    for j in 0..len {
                        if self.sub_expr[i].equal(&other.get(j).unwrap()) && !(index[i] || index[j+len]) {
                                index[i] = true;
                                index[j+len] = true;
                                continue 'i;
                            }
                    }
                    return false;
                }
                return index.iter().all(|&x| x);
            }
        }
        false
    }

    pub fn simplify(mut self) -> Expression {
        let simplified_numerator = self.sub_expr[0].clone().simplify();
        let simplified_denominator = self.sub_expr[1].clone().simplify();
        self.simplified = true;
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
