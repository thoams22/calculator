use std::{arch::x86_64::_rdrand16_step, collections::HashMap};

use crate::ast::Expression;

use super::{addition::Addition, varibale::Variable, Expr, State};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Multiplication {
    pub sub_expr: Vec<Expression>,
    pub simplified: bool,
}

impl Multiplication {
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
}

impl Expr for Multiplication {
    fn equal(&self, other: &Multiplication) -> bool {
        if self.sub_expr.len() != other.sub_expr.len() {
            return false;
        }

        let len = self.sub_expr.len();
        let mut index: Vec<bool> = vec![false; len * 2];
        'i: for i in 0..len {
            for j in 0..len {
                if !(index[i] || index[j + len])
                    && self.sub_expr[i].equal(other.sub_expr.get(j).unwrap())
                {
                    index[i] = true;
                    index[j + len] = true;
                    continue 'i;
                }
            }
            return false;
        }
        return index.iter().all(|&x| x);
    }

    fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Multiplication(Box::new(self));
        }

        for expression in self.sub_expr.iter_mut() {
            *expression = expression.clone().simplify();
        }

        self.regroup_nested_multiplication()
            .multiplication() // Mult(a, a) -> Exp(a, 2)
            .multiplication_fraction() // Mult(1/2, 2) -> 1, Mult(1/2, 2/3) -> 1/3
            .multiplication_on_sum() // Mult(Add(4, 2), Add(8, 3)) -> 66
            .multiplication_number() // Mult(8, 3, 2, 4) -> 192, Mult(a, 1) -> a , Mult(a, 0) -> 0
    }

    fn contain_vars(&self) -> Option<std::collections::HashMap<super::varibale::Variable, usize>> {
        let mut map: HashMap<Variable, usize> = HashMap::new();
        for expr in self.sub_expr.iter() {
            if let Some(mut sub_map) = expr.contain_vars() {
                for (key, value) in sub_map.iter_mut() {
                    if let Some(occurence) = map.get_mut(key) {
                        *occurence += *value;
                    } else {
                        map.insert(key.clone(), *value);
                    }
                }
            }
        }
        if map.is_empty() {
            None
        } else {
            Some(map)
        }
    }

    fn contain_var(&self, variable: &super::varibale::Variable) -> bool {
        self.sub_expr.iter().any(|expr| expr.contain_var(variable))
    }

    fn get_order(&self) -> i64 {
        self.sub_expr
            .iter()
            .map(|expr| expr.get_order())
            .max()
            .unwrap_or(0)
    }

    fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        println!("{}Multiplication :", current_span);
        for expr in self.sub_expr.iter() {
            print!("{}", current_span);
            expr.print_tree(Some(&new_span));
        }
    }

    fn calc_pos(
        &self,
        position: &mut Vec<(String, (i8, i8))>,
        prev_state: super::State,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) {
        let mut max_height = 1;
        let mut max_height_index = 0;
        for (i, expr) in self.sub_expr.iter().enumerate() {
            let height = expr.get_height(memoized);
            if height > max_height {
                max_height = height;
                max_height_index = i;
            }
        }

        let len = position.len();
        let mut position_clone = position.clone();

        self.sub_expr[max_height_index].calc_pos(&mut position_clone, prev_state, memoized);

        let mut pos_y = position_clone[len].1 .0;
        let mut pos_x = prev_state.get_pos_x();

        for (i, expr) in self.sub_expr.iter().enumerate() {
            if i > 0 {
                position.push((" * ".to_string(), (pos_y, pos_x)));
                pos_x += 3;
            }
            if let Expression::Addition(add) = expr {
                add.make_parenthesis(&mut pos_y, &mut pos_x, position, false, memoized);
                expr.calc_pos(position, State::Same(pos_y, pos_x), memoized);
                pos_x += expr.get_length(memoized);
                add.make_parenthesis(&mut pos_y, &mut pos_x, position, true, memoized);
            } else {
                expr.calc_pos(position, State::Same(pos_y, pos_x), memoized);
                pos_x += expr.get_length(memoized);
            }
        }
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        let mut length = 0;
        for expr in self.sub_expr.iter() {
            if let Expression::Addition(_) = expr {
                length += expr.get_length(memoized) + 5; // add parenthesis
            } else {
                length += expr.get_length(memoized) + 3;
            }
        }
        length -= 3;
        length
    }

    fn get_height(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        let mut max_height = 0;
        for expr in &self.sub_expr {
            max_height = max_height.max(expr.get_height(memoized));
        }
        max_height
    }

    fn get_above_height(
        &self,
        memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>,
    ) -> i8 {
        let mut max_height = 0;
        for expr in &self.sub_expr {
            max_height = max_height.max(expr.get_above_height(memoized));
        }
        max_height
    }
}

impl Multiplication {
    fn regroup_nested_multiplication(mut self) -> Multiplication {
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
        'iloop: for i in 0..self.sub_expr.len() {
            for j in (i + 1)..self.sub_expr.len() {
                if let (Expression::Addition(add), a) | (a, Expression::Addition(add)) =
                    (self.sub_expr[i].clone(), self.sub_expr[j].clone())
                {
                    let mut temp_distributed: Addition = Addition::from_vec(Vec::new());
                    if let Expression::Addition(add_2) = a {
                        for k in 0..add_2.sub_expr.len() {
                            temp_distributed
                                .sub_expr
                                .extend(Self::distribution(add_2.sub_expr.get(k).unwrap(), &add));
                        }
                    } else {
                        temp_distributed
                            .sub_expr
                            .extend(Self::distribution(&a, &add));
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
            .sub_expr
            .iter()
            .map(|expr| Expression::multiplication(distributor.clone(), expr.clone()))
            .collect()
    }

    fn multiplication_fraction(mut self) -> Multiplication {
        let mut i = 0;
        while i < self.sub_expr.len() {
            if let Expression::Fraction(frac) = &self.sub_expr[i] {
                let numerator = frac.sub_expr[0].clone();
                let denominator = frac.sub_expr[1].clone();

                self.sub_expr.swap_remove(i);

                return Multiplication::from_vec(vec![Expression::fraction(
                    Expression::multiplication(
                        Expression::multiplication_from_vec(self.sub_expr.clone()),
                        numerator,
                    ),
                    denominator,
                )
                .simplify()]);
            } else {
                i += 1;
            }
        }
        self
    }

    fn multiplication(mut self) -> Multiplication {
        let mut i = 0;
        while i < self.sub_expr.len() {
            let expr = self.sub_expr[i].clone();

            let (base, mut exponent) = match &expr {
                Expression::Exponentiation(expo) => (
                    expo.get_base(),
                    Addition::from_vec(vec![expo.get_exponent()]),
                ),
                Expression::Number(_) => {
                    i += 1;
                    continue;
                }
                _ => (
                    expr.clone(),
                    Addition::from_vec(vec![Expression::number(1)]),
                ),
            };

            let mut simplify: bool = false;
            let mut j = i + 1;
            while j < self.sub_expr.len() {
                let second_expr = self.sub_expr[j].clone();

                let (base_2, _exponent_2) = match &second_expr {
                    Expression::Exponentiation(expo) => (
                        expo.get_base(),
                        Addition::from_vec(vec![expo.get_exponent()]),
                    ),
                    Expression::Number(_) => {
                        j += 1;
                        continue;
                    }
                    _ => (
                        second_expr.clone(),
                        Addition::from_vec(vec![Expression::number(1)]),
                    ),
                };

                if expr.equal(&second_expr) | base.equal(&base_2) {
                    simplify = true;
                    if let Expression::Exponentiation(expo) = &second_expr {
                        exponent.sub_expr.push(expo.get_exponent());
                    } else {
                        exponent.sub_expr.push(Expression::number(1));
                    }
                    self.sub_expr.swap_remove(j);
                } else {
                    j += 1;
                }
            }

            if simplify {
                let simplified_exponent = Expression::Addition(Box::new(exponent)).simplify();
                if let Expression::Exponentiation(mut expo) = expr {
                    expo.simplified = false;
                    expo.set_exponent(simplified_exponent);
                    self.sub_expr
                        .push(Expression::Exponentiation(expo).simplify());
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
        let mut sum = 1;
        let mut non_numbers: Vec<Expression> = Vec::new();

        for expr in self.sub_expr.drain(..) {
            match expr {
                Expression::Number(num) => {
                    if num.sub_expr == 0 {
                        return Expression::number(0);
                    }
                    sum *= num.sub_expr;
                }
                _ => non_numbers.push(expr),
            }
        }

        if sum != 1 {
            non_numbers.push(Expression::number(sum));
        }

        self.simplified = true;

        match non_numbers.len() {
            0 => {
                if sum == 1 {
                    return Expression::number(1);
                }
                Expression::number(0)
            }
            1 => non_numbers.pop().unwrap(),
            _ => Expression::Multiplication(Box::new(Self::from_vec(non_numbers).order())),
        }
    }

    fn order(mut self) -> Multiplication {
        self.sub_expr.sort_by_key(|a| a.get_order());
        self
    }

    pub fn iter(&self) -> MultiplicationIter {
        MultiplicationIter {
            addition: self,
            index: 0,
        }
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
        self.addition.sub_expr.get(current)
    }
}
