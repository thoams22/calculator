use crate::tokenizer::CalcError;

use super::{addition::Addition, exponentiation::Exponentiation, Expression};

#[derive(PartialEq, Debug, Clone)]
pub struct Multiplication {
    sub_expr: Vec<Expression>,
    simplified: bool,
}

impl Multiplication {
    pub fn new(first: Expression, second: Expression) -> Self {
        let sub_expr: Vec<Expression> = vec![first, second];
        Self { sub_expr , simplified : false}
    }

    pub fn from_vec(sub_expr: Vec<Expression>) -> Self {
        Self { sub_expr , simplified : false}
    }

    pub fn set_simplified(&mut self, state: bool) {
        self.simplified = state;
    }

    pub fn len(&self) -> usize {
        self.sub_expr.len()
    }
    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
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

    pub fn swap_remove(&mut self, index: usize) {
        self.sub_expr.swap_remove(index);
    }

    pub fn remove(&mut self, index: usize) {
        self.sub_expr.remove(index);
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Multiplication(_) = other {
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

    pub fn iter(&self) -> MultiplicationIter {
        MultiplicationIter {
            addition: self,
            index: 0,
        }
    }

    pub fn simplify(self) -> Expression {
        if self.simplified {
            return Expression::Multiplication(Box::new(self));
        }
        self.simplify_nested_expression() // simplify sub expression
            .simplify_nested_multiplication() // Mult(Mult(4, 2), Mult(8, 3)) -> Mult(4, 2, 8, 3)
            .multiplication() // Mult(a, a) -> Exp(a, 2)
            .multiplication_on_sum() // Mult(Add(4, 2), Add(8, 3)) -> 66
            .multiplication_number() // Mult(8, 3, 2, 4) -> 192, Mult(a, 1) -> a , Mult(a, 0) -> 0
    }

    fn simplify_nested_expression(mut self) -> Multiplication {
        for expression in self.sub_expr.iter_mut() {
            *expression = expression.clone().simplify();
        }
        self
    }

    fn simplify_nested_multiplication(mut self) -> Multiplication {
        let mut i = 0;
        while i < self.sub_expr.len() {
            if let Expression::Multiplication(expr) = &self.sub_expr[i] {
                self.sub_expr.extend(expr.sub_expr.clone());
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self
    }

    fn multiplication_on_sum(mut self) -> Multiplication {
        let mut remove_indices: Vec<usize> = Vec::new();
        'iloop:for i in 0..self.sub_expr.len() {
            for j in (i + 1)..self.sub_expr.len() {
                if let (Expression::Addition(add), a) | (a, Expression::Addition(add)) =
                    (self.sub_expr[i].clone(), self.sub_expr[j].clone())
                {
                    let mut temp_distributed: Addition = Addition::from_vec(Vec::new());
                    if let Expression::Addition(add_2) = a {
                        for k in 0..add_2.len() {
                            temp_distributed.extend(Self::distribution(add_2.get(k).unwrap(), &add));
                        }
                    } else {
                        temp_distributed.extend(Self::distribution(&a, &add));
                    }
                    self.sub_expr[j] = Expression::Addition(Box::new(temp_distributed));
                    remove_indices.push(i);
                    continue 'iloop;
                }
            }
        }

        remove_indices.iter().rev().for_each(|indice| {
            self.sub_expr.remove(*indice);
        });

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }

        self
    }

    fn distribution(distributor: &Expression, distributed: &Addition) -> Vec<Expression> {
        distributed
            .iter()
            .map(|expr| Expression::multiplication(distributor.clone(), expr.clone()))
            .collect()
    }

    fn multiplication(mut self) -> Multiplication {
        let mut i = 0;
        while i < self.sub_expr.len() {
            let expr = self.sub_expr[i].clone();

            let (base, mut exponent) = match &expr {
                Expression::Exponentiation(expo) => (
                    expo.get(0).cloned().unwrap(),
                    Addition::from_vec(vec![expo.get(1).cloned().unwrap()]),
                ),
                Expression::Number(_) => {
                    i += 1;
                    continue;
                }
                _ => (
                    expr.clone(),
                    Addition::from_vec(vec![Expression::Number(1.0)]),
                ),
            };

            let mut simplify: bool = false;
            let mut j = i + 1;
            while j < self.sub_expr.len() {
                let second_expr = self.sub_expr[j].clone();

                let (base_2, _exponent_2) = match &second_expr {
                    Expression::Exponentiation(expo) => (
                        expo.get(0).cloned().unwrap(),
                        Addition::from_vec(vec![expo.get(1).cloned().unwrap()]),
                    ),
                    Expression::Number(_) => {
                        j += 1;
                        continue;
                    }
                    _ => (
                        second_expr.clone(),
                        Addition::from_vec(vec![Expression::Number(1.0)]),
                    ),
                };

                if expr.equal(&second_expr) | base.equal(&base_2) {
                    simplify = true;
                    if let Expression::Exponentiation(expo) = &second_expr {
                        exponent.push(expo.get(1).cloned().unwrap());
                    } else {
                        exponent.push(Expression::Number(1.0));
                    }
                    self.sub_expr.swap_remove(j);
                } else {
                    j += 1;
                }
            }
            
            if simplify {
                let simplified_exponent = Expression::Addition(Box::new(exponent)).simplify();
                if let Expression::Exponentiation(mut expo) = expr {
                    expo.simplified(false);
                    expo.replace_exponent(simplified_exponent);
                    self.sub_expr.push(Expression::Exponentiation(expo).simplify());
                } else {
                    self.sub_expr
                        .push(Expression::exponentiation(base, simplified_exponent).simplify());
                }
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self
    }

    fn multiplication_number(mut self) -> Expression {
        let mut sum = 1.0;
        let mut non_numbers: Vec<Expression> = Vec::new();
        
        for expr in self.sub_expr.drain(..) {
            match expr {
                Expression::Number(num) => {
                    if num == 0.0 {
                        return Expression::Number(0.0);
                    }
                    sum *= num;
                }
                _ => non_numbers.push(expr),
            }
        }

        if sum != 1.0 {
            non_numbers.push(Expression::Number(sum));
        }
        
        self.simplified = true;
        
        match non_numbers.len() {
            0 => {
                if sum == 1.0 {
                    return Expression::Number(1.0);
                }
                Expression::Number(0.0)},
            1 => non_numbers.pop().unwrap(),
            _ => Expression::Multiplication(Box::new(Self::from_vec(non_numbers))),
        }
    }

    pub fn evaluate(&self) -> Result<f64, CalcError> {
        let mut result: f64 = 1.0;
        for ele in &self.sub_expr {
            match ele.evaluate() {
                Ok(number) => result *= number,
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
        let joined_result = result.join("*");
        Ok(format!("({})", joined_result))
    }
}

pub struct MultiplicationIter<'a> {
    addition: &'a Multiplication,
    index: usize,
}

impl<'a> Iterator for MultiplicationIter<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.index;
        self.index += 1;
        self.addition.get(current)
    }
}
