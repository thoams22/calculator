use crate::tokenizer::CalcError;

use super::{
    function::{Function, Functions},
    Expression,
};

#[derive(PartialEq, Debug, Clone)]
pub struct Addition {
    sub_expr: Vec<Expression>,
    simplified: bool,
}

impl Addition {
    pub fn new(first: Expression, second: Expression) -> Self {
        let sub_expr: Vec<Expression> = vec![first, second];
        Self {
            sub_expr,
            simplified: false,
        }
    }

    pub fn from_vec(sub_expr: Vec<Expression>) -> Self {
        Self {
            sub_expr,
            simplified: false,
        }
    }

    pub fn len(&self) -> usize {
        self.sub_expr.len()
    }

    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn swap_remove(&mut self, index: usize) {
        self.sub_expr.swap_remove(index);
    }

    pub fn remove(&mut self, index: usize) {
        self.sub_expr.remove(index);
    }

    pub fn push(&mut self, other: Expression) {
        self.sub_expr.push(other)
    }

    pub fn extend(&mut self, other: Vec<Expression>) {
        self.sub_expr.extend(other)
    }

    pub fn insert(&mut self, index: usize, other: Expression) {
        self.sub_expr.insert(index, other)
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Addition(_) = other {
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

    pub fn iter(&self) -> AdditionIter {
        AdditionIter {
            addition: self,
            index: 0,
        }
    }

    pub fn simplify(self) -> Expression {
        if self.simplified {
            return Expression::Addition(Box::new(self));
        }
        self.simplify_nested_expression() // simplify sub expression
            .simplify_nested_addition() // Add(Add(4, 2), Add(8, 3)) -> Add(4, 2, 8, 3)
            .addition_variable() // Add(a, a, b) -> Add(Mult(2, a), b)
            .addition_number() // Add(4, 2, 8, 3) -> 17, Add(5, 0) -> 5, Add(a, 0) -> a
    }

    fn simplify_nested_expression(mut self) -> Addition {
        for expression in self.sub_expr.iter_mut() {
            *expression = expression.clone().simplify();
        }
        self
    }

    fn simplify_nested_addition(mut self) -> Addition {
        let mut i = 0;
        while i < self.sub_expr.len() {
            if let Expression::Addition(expr) = &self.sub_expr[i] {
                self.sub_expr.extend(expr.sub_expr.clone());
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self
    }

    fn addition_variable(mut self) -> Addition {
        let mut i = 0;
        while i < self.sub_expr.len() {
            let (mut coefficient, mut expr) =
                Self::extract_coefficient_and_expr(self.sub_expr[i].clone());

            let mut simplify: bool = false;
            let mut j = i + 1;
            while j < self.sub_expr.len() {
                let (second_coefficient, second_expr) =
                    Self::extract_coefficient_and_expr(self.sub_expr[j].clone());

                if expr.equal(&second_expr) {
                    coefficient += second_coefficient;
                    self.sub_expr.swap_remove(j);
                    simplify = true;
                } else if let (Expression::Function(f_expr), Expression::Function(f_sec_expr)) =
                    (expr.clone(), second_expr.clone())
                {
                    if let Some(result) = Self::simplify_function(*f_expr, *f_sec_expr) {
                        expr = result;
                        self.sub_expr.swap_remove(j);
                        simplify = true;
                    } else {
                        j += 1;
                    }
                } else {
                    j += 1;
                }
            }

            if simplify {
                if let Expression::Multiplication(mut mult) = expr {
                    mult.insert(0, Expression::Number(coefficient));
                    self.sub_expr
                        .push(Expression::Multiplication(mult).simplify());
                } else if let Expression::Variable(var) = expr {
                    self.sub_expr.push(
                        Expression::multiplication(
                            Expression::Number(coefficient),
                            Expression::Variable(var),
                        )
                        .simplify(),
                    );
                } else if let Expression::Number(num) = expr {
                    self.sub_expr.push(Expression::Number(num * coefficient));
                } else {
                    self.sub_expr.push(
                        Expression::multiplication(Expression::Number(coefficient), expr)
                            .simplify(),
                    );
                }
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self
    }

    fn simplify_function(expr_1: Function, expr_2: Function) -> Option<Expression> {
        match (expr_1.get_function(), expr_2.get_function()) {
            (Functions::Ln, Functions::Ln) => Some(
                Expression::ln(Expression::multiplication(
                    expr_1.get(0).unwrap().clone(),
                    expr_2.get(0).unwrap().clone(),
                ))
                .simplify(),
            ),

            (Functions::Log2, Functions::Log2) => Some(Expression::log2(
                Expression::multiplication(
                    expr_1.get(0).unwrap().clone(),
                    expr_2.get(0).unwrap().clone(),
                )
                .simplify(),
            )),

            (Functions::Log10, Functions::Log10) => Some(Expression::log10(
                Expression::multiplication(
                    expr_1.get(0).unwrap().clone(),
                    expr_2.get(0).unwrap().clone(),
                )
                .simplify(),
            )),

            (Functions::Log, Functions::Log) => {
                if expr_1.get(0).unwrap() == expr_2.get(0).unwrap() {
                    Some(
                        Expression::log(
                            expr_1.get(0).unwrap().clone(),
                            Expression::multiplication(
                                expr_1.get(1).unwrap().clone(),
                                expr_2.get(1).unwrap().clone(),
                            ),
                        )
                        .simplify(),
                    )
                } else {
                    None
                }
            }
            (_, _) => None,
        }
    }

    fn extract_coefficient_and_expr(expr: Expression) -> (f64, Expression) {
        match expr {
            Expression::Multiplication(mut mult) => {
                let mut coefficient = 1.0;
                for i in 0..mult.len() {
                    if let Some(Expression::Number(num)) = mult.get(i) {
                        coefficient = *num;
                        mult.swap_remove(i);
                        break;
                    }
                }
                (
                    coefficient,
                    if mult.len() == 1 {
                        match mult.get(0) {
                            Some(Expression::Variable(var)) => Expression::Variable(*var),
                            Some(Expression::Number(num)) => Expression::Number(*num),
                            _ => Expression::Multiplication(mult),
                        }
                    } else {
                        Expression::Multiplication(mult)
                    },
                )
            }
            _ => (1.0, expr),
        }
    }

    fn addition_number(mut self) -> Expression {
        let mut sum: f64 = 0.0;
        self.sub_expr.retain(|expr| {
            if let Expression::Number(num) = expr {
                sum += num;
                false
            } else {
                true
            }
        });
        if sum != 0.0 {
            self.sub_expr.push(Expression::Number(sum));
        }

        self.simplified = true;

        match self.sub_expr.len() {
            0 => Expression::Number(0.0),
            1 => self.sub_expr.get(0).unwrap().clone(),
            _ => Expression::Addition(Box::new(self)),
        }
    }

    pub fn evaluate(&self) -> Result<f64, CalcError> {
        let mut result: f64 = 0.0;
        for ele in &self.sub_expr {
            match ele.evaluate() {
                Ok(number) => result += number,
                Err(err) => return Err(err),
            }
        }
        Ok(result)
    }

    pub fn to_string(&self) -> Result<String, CalcError> {
        let mut result: Vec<String> = Vec::new();
        for ele in &self.sub_expr {
            match ele.to_string() {
                Ok(number) => result.push(number),
                Err(err) => return Err(err),
            }
        }
        let joined_result = result.join("+");
        Ok(format!("({})", joined_result))
    }
}

pub struct AdditionIter<'a> {
    addition: &'a Addition,
    index: usize,
}

impl<'a> Iterator for AdditionIter<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.index;
        self.index += 1;
        self.addition.get(current)
    }
}
