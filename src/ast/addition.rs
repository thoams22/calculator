use std::collections::HashMap;

use crate::ast::Expression;

use super::{
    function::{FunctionType, PredefinedFunction},
    varibale::Variable,
    Expr, State,
};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Addition {
    pub sub_expr: Vec<Expression>,
    pub simplified: bool,
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
}

impl Expr for Addition {
    fn equal(&self, other: &Addition) -> bool {
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
            return Expression::Addition(Box::new(self));
        }

        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }

        self.regroup_nested_addition()
            .addition_variable()
            .addition_fraction()
            .addition_number()
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
        println!("{}Addition :", current_span);
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
        let pos_y = position_clone[len].1 .0;
        let mut pos_x = prev_state.get_pos_x();

        for (i, expr) in self.sub_expr.iter().enumerate() {
            if i > 0 {
                position.push((" + ".to_string(), (pos_y, pos_x)));
                pos_x += 3;
            }
            
            expr.calc_pos(position, State::Same(pos_y, pos_x), memoized);
            pos_x += expr.get_length(memoized);
        }
    }

    fn get_length(&self, memoized: &mut std::collections::HashMap<Expression, (i8, i8, i8)>) -> i8 {
        let mut length = 0;
        for expr in self.sub_expr.iter() {
            length += expr.get_length(memoized) + 3;
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

impl Addition {
    pub fn regroup_nested_addition(mut self) -> Addition {
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
                match (expr.clone(), second_expr) {
                    (Expression::Function(f_expr), Expression::Function(f_sec_expr)) => {
                        if let Some(fun) = Self::simplify_function(
                            (*f_expr, &mut coefficient),
                            (*f_sec_expr, second_coefficient),
                        ) {
                            expr = fun;
                            self.sub_expr.swap_remove(j);
                            simplify = true;
                        } else {
                            j += 1;
                        }
                    }
                    (first, second) if first.equal(&second) => {
                        coefficient += second_coefficient;
                        self.sub_expr.swap_remove(j);
                        simplify = true;
                    }
                    (_, _) => j += 1,
                }
            }

            if simplify {
                match expr {
                    Expression::Multiplication(mut mult) => {
                        mult.sub_expr.insert(0, Expression::number(coefficient));
                        self.sub_expr
                            .push(Expression::Multiplication(mult).simplify());
                    }
                    Expression::Number(num) => {
                        self.sub_expr.push(Expression::number(num.sub_expr * coefficient));
                    }
                    _ => {
                        self.sub_expr.push(
                            Expression::multiplication(Expression::number(coefficient), expr)
                                .simplify(),
                        );
                    }
                }
                self.sub_expr.swap_remove(i);
            } else {
                i += 1;
            }
        }
        self
    }

    fn addition_fraction(mut self) -> Addition {
        let mut i = 0;
        while i < self.sub_expr.len() {
            if let Expression::Fraction(frac) = &self.sub_expr[i] {
                let numerator = frac.sub_expr[0].clone();
                let denominator = frac.sub_expr[1].clone();

                self.sub_expr.swap_remove(i);

                return Addition::new(
                    Expression::number(0),
                    Expression::fraction(
                        Expression::addition(
                            numerator,
                            Expression::multiplication(
                                Expression::addition_from_vec(self.sub_expr.clone()),
                                denominator.clone(),
                            ),
                        ),
                        denominator,
                    )
                    .simplify(),
                );
            } else {
                i += 1;
            }
        }
        self
    }

    // TODO add coeff for logarithme simplification exponent the args before multiply
    fn simplify_function(
        (func_1, coeff_1): (FunctionType, &mut i64),
        (func_2, coeff_2): (FunctionType, i64),
    ) -> Option<Expression> {
        match (func_1.clone(), func_2.clone()) {
            (
                FunctionType::Predefined(type_1, args_1),
                FunctionType::Predefined(type_2, args_2),
            ) => match (&type_1, type_2) {
                (PredefinedFunction::Sqrt, PredefinedFunction::Sqrt)
                | (PredefinedFunction::Acos, PredefinedFunction::Acos)
                | (PredefinedFunction::Asin, PredefinedFunction::Asin)
                | (PredefinedFunction::Atan, PredefinedFunction::Atan)
                | (PredefinedFunction::Sin, PredefinedFunction::Sin)
                | (PredefinedFunction::Tan, PredefinedFunction::Tan)
                | (PredefinedFunction::Cos, PredefinedFunction::Cos) => {
                    if args_1[0].equal(&args_2[0]) {
                        *coeff_1 += coeff_2;
                        Some(Expression::function(func_1))
                    } else {
                        None
                    }
                }
                (PredefinedFunction::Ln, PredefinedFunction::Ln) => {
                    if args_1[0].equal(&args_2[0]) {
                        *coeff_1 += coeff_2;
                        Some(Expression::function(func_1))
                    } else {
                        let coefficient = *coeff_1;
                        *coeff_1 = 1;
                        Some(Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Ln,
                            vec![Expression::multiplication(
                                Expression::exponentiation(
                                    args_1[0].clone(),
                                    Expression::number(coefficient),
                                ),
                                Expression::exponentiation(
                                    args_2[0].clone(),
                                    Expression::number(coeff_2),
                                ),
                            )],
                        )))
                    }
                }
                (PredefinedFunction::Log, PredefinedFunction::Log) => {
                    if args_1[0].equal(&args_2[0]) {
                        *coeff_1 += coeff_2;
                        Some(Expression::function(func_1))
                    } else {
                        let coefficient = *coeff_1;
                        *coeff_1 = 1;
                        Some(Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Log,
                            vec![Expression::multiplication(
                                Expression::exponentiation(
                                    args_1[0].clone(),
                                    Expression::number(coefficient),
                                ),
                                Expression::exponentiation(
                                    args_2[0].clone(),
                                    Expression::number(coeff_2),
                                ),
                            )],
                        )))
                    }
                }
                (_, _) => None,
            },
            (FunctionType::UserDefined(name1, args1), FunctionType::UserDefined(_, _))
                if func_1.equal(&func_2) =>
            {
                *coeff_1 += coeff_2;
                Some(Expression::function(FunctionType::UserDefined(
                    name1, args1,
                )))
            }
            (_, _) => None,
        }
    }

    pub fn extract_coefficient_and_expr(expr: Expression) -> (i64, Expression) {
        match expr {
            Expression::Multiplication(mut mult) => {
                let mut coefficient = 1;
                for i in 0..mult.sub_expr.len() {
                    if let Some(Expression::Number(num)) = mult.sub_expr.get(i) {
                        coefficient = num.sub_expr;
                        mult.sub_expr.swap_remove(i);
                        break;
                    }
                }
                (
                    coefficient,
                    if mult.sub_expr.len() == 1 {
                        mult.sub_expr[0].clone()
                    } else {
                        Expression::Multiplication(mult)
                    },
                )
            }
            Expression::Negation(neg) => {
                let mut extracted = Self::extract_coefficient_and_expr(neg.sub_expr);
                extracted.0 = -extracted.0;
                extracted
            }
            _ => (1, expr),
        }
    }

    fn addition_number(mut self) -> Expression {
        let mut sum: i64 = 0;
        self.sub_expr.retain(|expr| {
            if let Expression::Number(num) = expr {
                sum += num.sub_expr;
                false
            } else {
                true
            }
        });
        if sum != 0 {
            self.sub_expr.push(Expression::number(sum));
        }

        self.simplified = true;

        match self.sub_expr.len() {
            0 => Expression::number(0),
            1 => self.sub_expr.get(0).unwrap().clone(),
            _ => Expression::Addition(Box::new(self.order())),
        }
    }

    pub fn order(mut self) -> Addition {
        self.sub_expr
            .sort_by_key(|b| std::cmp::Reverse(b.get_order()));
        self
    }
}
