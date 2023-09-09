use crate::{ast::function::PredefinedFunction, simplify::Expression};

use super::function::FunctionType;

#[derive(PartialEq, Debug, Clone)]
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

impl Addition {
    pub fn equal(&self, other: &Addition) -> bool {
        if self.sub_expr.len() != other.sub_expr.len() {
            return false;
        }

        for expr in self.sub_expr.iter() {
            if !other.sub_expr.contains(expr) {
                return false;
            }
        }

        true
    }
}

impl Addition {
    pub fn simplify(mut self) -> Expression {
        if self.simplified {
            return Expression::Addition(Box::new(self));
        }

        self.simplified = true;
        for expr in self.sub_expr.iter_mut() {
            *expr = expr.clone().simplify();
        }

        self.regroup_nested_addition()
            .addition_variable()
            .addition_number()
    }

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
                    mult.sub_expr.insert(0, Expression::Number(coefficient));
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

    fn extract_coefficient_and_expr(expr: Expression) -> (i64, Expression) {
        match expr {
            Expression::Multiplication(mut mult) => {
                let mut coefficient = 1;
                for i in 0..mult.sub_expr.len() {
                    if let Some(Expression::Number(num)) = mult.sub_expr.get(i) {
                        coefficient = *num;
                        mult.sub_expr.swap_remove(i);
                        break;
                    }
                }
                (
                    coefficient,
                    if mult.sub_expr.len() == 1 {
                        match mult.sub_expr.get(0) {
                            Some(Expression::Variable(var)) => Expression::Variable(*var),
                            Some(Expression::Number(num)) => Expression::Number(*num),
                            _ => Expression::Multiplication(mult),
                        }
                    } else {
                        Expression::Multiplication(mult)
                    },
                )
            }
            _ => (1, expr),
        }
    }

    fn simplify_function(func_1: FunctionType, func_2: FunctionType) -> Option<Expression> {
        match (func_1, func_2) {
            (
                FunctionType::Predefined(type_1, mut args_1),
                FunctionType::Predefined(type_2, mut args_2),
            ) => match (&type_1, type_2) {
                (PredefinedFunction::Sqrt, PredefinedFunction::Sqrt)
                | (PredefinedFunction::Acos, PredefinedFunction::Acos)
                | (PredefinedFunction::Asin, PredefinedFunction::Asin)
                | (PredefinedFunction::Atan, PredefinedFunction::Atan)
                | (PredefinedFunction::Sin, PredefinedFunction::Sin)
                | (PredefinedFunction::Tan, PredefinedFunction::Tan)
                | (PredefinedFunction::Cos, PredefinedFunction::Cos) => {
                    if args_1[0].equal(&args_2[0]) {
                        Some(Expression::multiplication(
                            Expression::Number(2),
                            Expression::function(FunctionType::Predefined(type_1, args_1)),
                        ))
                    } else {
                        None
                    }
                }
                (PredefinedFunction::Ln, PredefinedFunction::Ln) => Some(
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Ln,
                        vec![Expression::multiplication_from_vec(
                            [args_1, args_2].concat(),
                        )],
                    ))
                    .simplify(),
                ),
                (PredefinedFunction::Log, PredefinedFunction::Log) => Some(
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Log,
                        vec![
                            args_1[0].clone(),
                            Expression::multiplication_from_vec(
                                [args_1.split_off(1), args_2.split_off(1)].concat(),
                            ),
                        ],
                    ))
                    .simplify(),
                ),
                (_, _) => None,
            },
            (FunctionType::UserDefined(_, _), FunctionType::UserDefined(_, _)) => todo!(),
            (_, _) => None,
        }
    }

    fn addition_number(mut self) -> Expression {
        let mut sum: i64 = 0;
        self.sub_expr.retain(|expr| {
            if let Expression::Number(num) = expr {
                sum += num;
                false
            } else {
                true
            }
        });
        if sum != 0 {
            self.sub_expr.push(Expression::Number(sum));
        }

        self.simplified = true;

        match self.sub_expr.len() {
            0 => Expression::Number(0),
            1 => self.sub_expr.get(0).unwrap().clone(),
            _ => Expression::Addition(Box::new(self)),
        }
    }
}
