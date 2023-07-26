use crate::tokenizer::CalcError;

use super::{Expression, math::multinomial_expansion};

#[derive(PartialEq, Debug, Clone)]
pub struct Exponentiation {
    sub_expr: [Expression; 2],
    simplified: bool,
}

impl Exponentiation {
    pub fn new(base: Expression, exponent: Expression) -> Self {
        Self {
            sub_expr: [base, exponent],
            simplified: false,
        }
    }

    pub fn from_vec(sub_expr: [Expression; 2]) -> Self {
        Self {
            sub_expr,
            simplified: false,
        }
    }

    pub fn replace_base(&mut self, other: Expression) {
        self.sub_expr[0] = other;
    }
    pub fn replace_exponent(&mut self, other: Expression) {
        self.sub_expr[1] = other;
    }

    pub fn get_base(&self) -> Expression{
        self.sub_expr[0].clone()
    }

    pub fn get_exponent(&self) -> Expression{
        self.sub_expr[1].clone()
    }

    pub fn len(&self) -> usize {
        self.sub_expr.len()
    }
    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Exponentiation(_) = other {
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
        if self.simplified {
            return Expression::Exponentiation(Box::new(self));
        }

        for expression in self.sub_expr.iter_mut() {
            *expression = expression.clone().simplify();
        }

        self.simplified = true;
        match (&self.sub_expr[0], &self.sub_expr[1]) {
            (Expression::Number(num1), Expression::Number(num2)) => {
                Expression::Number(num1.powf(*num2))
            }
            (_, _) => self.simplify_exponent_one_zero(),
        }
    }

    //
    fn simplify_exponent_one_zero(self) -> Expression {
        if let Expression::Number(num) = self.sub_expr[1] {
            if num == 0.0 {
                return Expression::Number(1.0);
            } else if num == 1.0 {
                return self.sub_expr[0].clone();
            } else {
                return self.identity();
            }
        }
        Expression::Exponentiation(Box::new(self))
    }

    fn identity(self) -> Expression {
        if let (Expression::Addition(add), Expression::Number(num)) =
            (self.sub_expr[0].clone(), self.sub_expr[1].clone())
        {
            if num.fract() == 0.0 {
                return multinomial_expansion(num as i64, *add);
            }
        }
        Expression::Exponentiation(Box::new(self))
    }

    pub fn evaluate(&self) -> Result<f64, CalcError> {
        let mut result: f64 = 0.0;
        match self.sub_expr[0].evaluate() {
            Ok(number) => result += number,
            Err(err) => return Err(err),
        }
        match self.sub_expr[1].evaluate() {
            Ok(number) => result = result.powf(number),
            Err(err) => return Err(err),
        }

        Ok(result)
    }

    pub fn to_string(&self) -> Result<String, CalcError> {
        let mut result: String = String::from("(");
        match self.sub_expr[0].to_string() {
            Ok(number) => result = result + &number + "^",
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
