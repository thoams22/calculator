use std::slice::SliceIndex;

use super::{multiplication::Multiplication, Expression};

#[derive(PartialEq, Debug, Clone)]
pub struct Addition {
    sub_expr: Vec<Expression>,
}

impl Addition {
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

    pub fn swap_remove(&mut self, index: usize) {
        self.sub_expr.swap_remove(index);
    }

    pub fn remove(&mut self, index: usize) {
        self.sub_expr.remove(index);
    }

    pub fn add(&mut self, other: Expression) {
        self.sub_expr.push(other)
    }

    pub fn insert(&mut self, index: usize, other: Expression) {
        self.sub_expr.insert(index, other)
    }

    pub fn equal(&self, other: &Expression) -> bool {
        if let Expression::Addition(_) = other {
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

    pub fn simplify(mut self) -> Expression {
        self.simplify_nested_expression()
            .simplify_nested_addition()
            .addition_variable()
            .addition_number()
            .null_addition()
    }

    fn simplify_nested_expression(mut self) -> Addition {
        for i in 0..self.sub_expr.len() {
            let simplified = self.sub_expr[i].clone().simplify();
            self.sub_expr.remove(i);
            self.sub_expr.insert(i, simplified);
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
            let mut coefficient: f64 = 1.0;
            let mut expr = self.sub_expr.get(i).unwrap().clone();

            if let Expression::Multiplication(mut mult) = expr {
                for j in 0..mult.size() {
                    if let Some(Expression::Number(num)) = mult.get(j) {
                        coefficient = *num;
                        mult.swap_remove(j);
                        break;
                    }
                }

                expr = if mult.size() == 1 {
                    match mult.get(0) {
                        Some(Expression::Variable(var)) => Expression::Variable(*var),
                        Some(Expression::Number(num)) => Expression::Number(*num),
                        _ => Expression::Multiplication(mult),
                    }
                } else {
                    Expression::Multiplication(mult)
                }
            }

            let mut simplify: bool = false;
            let mut j = i + 1;
            while j < self.sub_expr.len() {
                let mut second_coefficient: f64 = 1.0;
                let mut second_expr = self.sub_expr.get(j).unwrap().clone();

                if let Expression::Multiplication(mut mult_2) = second_expr {
                    for k in 0..mult_2.size() {
                        if let Some(Expression::Number(num_2)) = mult_2.get(k) {
                            second_coefficient = *num_2;
                            mult_2.swap_remove(k);
                            break;
                        }
                    }
                    second_expr = if mult_2.size() == 1 {
                        match mult_2.get(0) {
                            Some(Expression::Variable(var)) => Expression::Variable(*var),
                            Some(Expression::Number(num)) => Expression::Number(*num),
                            _ => Expression::Multiplication(mult_2),
                        }
                    } else {
                        Expression::Multiplication(mult_2)
                    }
                }

                if expr.equal(&second_expr) {
                    coefficient += second_coefficient;
                    self.sub_expr.swap_remove(j);
                    // println!("add equal : {:?}, coef : {:?}", expr, coefficient);
                    simplify = true;
                } else {
                    j += 1
                }
            }
            if simplify {
                if let Expression::Multiplication(mut mult) = expr {
                    mult.insert(0, Expression::Number(coefficient));
                    self.sub_expr.push(Expression::Multiplication(mult));
                } else if let Expression::Variable(var) = expr {
                    self.sub_expr
                        .push(Expression::Multiplication(Box::new(Multiplication::new(
                            Expression::Number(coefficient),
                            Expression::Variable(var),
                        ))));
                } else if let Expression::Number(num) = expr {
                    self.sub_expr.push(Expression::Number(num * coefficient));
                }
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self
    }

    fn addition_number(mut self) -> Addition {
        let mut sum = 0.0;
        let mut i = 0;
        // println!("add_num {:?}", self);
        while i < self.sub_expr.len() {
            if let Some(Expression::Number(expr)) = self.sub_expr.get(i) {
                sum += expr;
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self.sub_expr.push(Expression::Number(sum));
        self
    }

    fn null_addition(mut self) -> Expression {
        // println!("null add : {:?}", self);
        let mut i = 0;
        while i < self.sub_expr.len() {
            if let Some(Expression::Number(0.0)) = self.sub_expr.get(i) {
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }

        match self.sub_expr.len() {
            0 => Expression::Number(0.0),
            1 => self.sub_expr.get(0).unwrap().clone(),
            _ => Expression::Addition(Box::new(self)),
        }
    }
}
