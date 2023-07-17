use core::num;

use super::{
    addition::Addition,
    exponentiation::{self, Exponentiation},
    Expression,
};

#[derive(PartialEq, Debug, Clone)]
pub struct Multiplication {
    sub_expr: Vec<Expression>,
}

impl Multiplication {
    pub fn new(first: Expression, second: Expression) -> Self {
        let sub_expr: Vec<Expression> = vec![first, second];
        Self { sub_expr }
    }

    pub fn from_vec(sub_expr: Vec<Expression>) -> Self {
        Self { sub_expr }
    }

    pub fn size(&self) -> usize {
        self.sub_expr.len()
    }
    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.sub_expr.get(index)
    }

    pub fn add(&mut self, other: Expression) {
        self.sub_expr.push(other)
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
        if let Expression::Multiplication(multiplication) = other {
            let mut count = 0;
            'i: for i in 0..self.size() {
                for j in 0..other.size() {
                    if self.sub_expr[i].equal(&other.get(j).unwrap()) {
                        count += 1;
                        continue 'i;
                    }
                }
                return false;
            }

            if count == self.size() {
                return true;
            } else {
                return false;
            }
        }
        false
    }

    pub fn simplify(self) -> Expression {
        self.simplify_nested_expression()
            .simplify_nested_multiplication()
            .multiplication_number()
            .multiplication()
            .one_multiplication()
    }

    fn simplify_nested_expression(mut self) -> Multiplication {
        for i in 0..self.sub_expr.len() {
            let simplified = self.sub_expr[i].clone().simplify();
            self.sub_expr.remove(i);
            self.sub_expr.insert(i, simplified);
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

    fn multiplication(mut self) -> Multiplication {
        let mut i = 0;
        while i < self.sub_expr.len() {
            let mut coefficient: f64 = 1.0;
            let mut base: Expression;
            let mut exponent: Addition = Addition::from_vec(vec![]);
            let mut expr: Expression = self.sub_expr.get(i).unwrap().clone();

            if let Expression::Exponentiation(expo) = expr {
                base = expo.get(0).unwrap().clone();
                exponent.add(expo.get(1).unwrap().clone());
                expr = Expression::Exponentiation(expo);
            } else {
                base = expr.clone();
                exponent.add(Expression::Number(1.0));
            }

            let mut simplify: bool = false;
            let mut j: usize = i + 1;
            while j < self.sub_expr.len() {
                let mut second_expr = self.sub_expr.get(j).unwrap().clone();

                if expr.equal(&second_expr) {
                    if let Expression::Exponentiation(expo) = second_expr {
                        exponent.add(expo.get(1).unwrap().clone());
                    } else {
                        exponent.add(Expression::Number(1.0));
                    }
                    self.sub_expr.swap_remove(j);
                    simplify = true;
                } else {
                    j += 1
                }
            }
            if simplify {
                if let Expression::Exponentiation(mut expo) = expr {
                    expo.replace_exponent(Expression::Addition(Box::new(exponent)).simplify());
                    self.sub_expr.push(Expression::Exponentiation(expo));
                } else {
                    // println!("Mult exponent : {:?}", exponent);
                    self.sub_expr
                        .push(Expression::Exponentiation(Box::new(Exponentiation::new(
                            base,
                            Expression::Addition(Box::new(exponent)).simplify(),
                        ))));
                }
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self
    }

    fn multiplication_number(mut self) -> Multiplication {
        let mut sum = 1.0;
        let mut i = 0;
        while i < self.sub_expr.len() {
            if let Some(Expression::Number(expr)) = self.sub_expr.get(i) {
                sum *= expr;
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        if sum != 1.0 {
            self.sub_expr.push(Expression::Number(sum));
        }
        self
    }

    fn one_multiplication(mut self) -> Expression {
        let mut i = 0;
        while i < self.sub_expr.len() {
            if let Some(Expression::Number(num)) = self.sub_expr.get(i) {
                if num == &1.0 {
                    self.sub_expr.swap_remove(i);
                } else if num == &0.0 {
                    return Expression::Number(0.0);
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
        }

        match self.sub_expr.len() {
            0 => Expression::Number(0.0),
            1 => self.sub_expr.get(0).unwrap().clone(),
            _ => Expression::Multiplication(Box::new(self)),
        }
    }
}
