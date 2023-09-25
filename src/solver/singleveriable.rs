use std::{mem::MaybeUninit, path::Components};

use crate::ast::{
    addition::{self, Addition},
    equality::Equality,
    function::{FunctionType, PredefinedFunction},
    math::{
        extract_coefficient_expression_exponent, prime_factor, ExpressionExponent, PrimeFactor,
    },
    multiplication::Multiplication,
    ConstantKind, Expression,
};

pub fn solve_one_var_one_occurence(mut equality: Equality) -> Expression {
    let mut solved = Equality::new(equality.get_right_side(), equality.get_left_side());

    while !solved.equal(&equality) {
        solved = equality.clone();

        addition_to_right(&mut equality);

        multiplication_to_right(&mut equality);

        fraction_to_right(&mut equality);

        function_to_right(&mut equality);

        exponent_to_right(&mut equality);
    }

    Expression::Equality(Box::new(equality))
}

pub fn solve_one_var_multiple_occurence(mut equality: Equality, variable: char) -> Vec<Expression> {
    if let Expression::Addition(addition) = equality.get_left_side() {
        let mut components: Vec<(Vec<PrimeFactor>, Vec<ExpressionExponent>)> = Vec::new();
        for expr in addition.sub_expr {
            components.push(extract_coefficient_expression_exponent(expr));
        }

        let mut polynomial: Vec<Monome> = Vec::new();

        // x^2 + 4yx + 3 => (2, 1), (1, 4y) (0, 3)
        // [((1, 1), (x, 2)),  ((2, 2), [(y, 1), (x, 1)]),  ((3, 1), ())]
        components.iter().for_each(|comp| {
            let mut coefficient = Multiplication::from_vec(Vec::new());
            let mut degree: i64 = 0;
            if !comp.1.is_empty() {
                if comp.1.iter().all(|expr_expo| {
                    if expr_expo.expression == Expression::Variable(variable) {
                        if let Expression::Number(num) = expr_expo.exponent {
                            degree = num;
                            true
                        } else {
                            false
                        }
                    } else {
                        coefficient.sub_expr.push(expr_expo.expression.clone());
                        true
                    }
                }) {
                    comp.0.iter().for_each(|prime_factor| {
                        if let Expression::Number(num) = prime_factor.exponent {
                            coefficient.sub_expr.push(Expression::exponentiation(
                                Expression::Number(prime_factor.prime),
                                Expression::Number(num),
                            ));
                        }
                    });

                    polynomial.push(Monome::new(
                        degree,
                        Expression::Multiplication(Box::new(coefficient)).simplify(),
                    ));
                }
            } else {
                if comp.0.iter().all(|prime_factor| {
                    if let Expression::Number(num) = prime_factor.exponent {
                        coefficient.sub_expr.push(Expression::exponentiation(
                            Expression::Number(prime_factor.prime),
                            Expression::Number(num),
                        ));
                        true
                    } else {
                        false
                    }
                }) {
                    polynomial.push(Monome::new(
                        0,
                        Expression::Multiplication(Box::new(coefficient)).simplify(),
                    ));
                }
            }
        });
        if polynomial.len() == components.len() {
            // println!("{:?}", polynomial);
            println!("Maybe a polynomial");
            if polynomial[0].degree == 2 && polynomial[1].degree == 1 {
                println!("Second degree");
                let solutions = solve_second_degree(polynomial);
                return solutions.iter().map(|solution| {
                    Expression::equality(
                        Expression::Variable(variable),
                        solution.clone(),
                    )
                }).collect();
            } else {
                is_second_degree_multiple(polynomial);
            }
        } else {
            println!("Not a polynomial");
        }
    }

    vec![Expression::Equality(Box::new(equality))]
}

fn is_second_degree_multiple(polynomial: Vec<Monome>) -> bool {
    if (polynomial.len() == 2 && polynomial[1].degree != 0)
        || (polynomial.len() == 3 && polynomial[2].degree == 0)
    {
        let decomp_max_deg = prime_factor(polynomial[0].degree);
        if decomp_max_deg[0].0 == 2 {
            let decomp_min_deg = prime_factor(polynomial[1].degree);
            if decomp_min_deg[0].0 == 2 && (polynomial[0].degree - polynomial[1].degree) == 2 {
                println!(
                    "possible variable swap: {} {}",
                    polynomial[0].degree, polynomial[1].degree
                );
                return true;
            } else if (polynomial[0].degree / polynomial[1].degree) == 2 {
                println!(
                    "possible variable swap: {} {}",
                    polynomial[0].degree, polynomial[1].degree
                );
            }
        }
    }

    false
}

fn solve_second_degree(polynomial: Vec<Monome>) -> Vec<Expression>{
    let mut a = Expression::Number(0);
    let mut b = Expression::Number(0);
    let mut c = Expression::Number(0);

    polynomial.into_iter().for_each(|monome| {
        if monome.degree == 2 {
            a = monome.coefficient;
        } else if monome.degree == 1 {
            b = monome.coefficient;
        } else if monome.degree == 0 {
            c = monome.coefficient;
        } else {
            panic!("Not a second degree polynomial")
        }
    });

    let discriminant = Expression::addition(
        Expression::exponentiation(b.clone(), Expression::Number(2)),
        Expression::multiplication_from_vec(vec![Expression::Number(-4), a.clone(), c.clone()]),
    )
    .simplify();

    if let Expression::Number(num) = discriminant {
        if num < 0 {
            println!("Complex solution");
            vec![]
        } else if num == 0 {
            println!("One solution");
            vec![Expression::fraction(
                Expression::negation(b.clone()),
                Expression::multiplication(Expression::Number(2), a.clone()),
            ).simplify()]
        } else {
            println!("Two solutions");
            vec![Expression::fraction(
                Expression::addition(
                    Expression::negation(b.clone()),
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Sqrt,
                        vec![discriminant.clone()],
                    )),
                ),
                Expression::multiplication(Expression::Number(2), a.clone()),
            ).simplify(), Expression::fraction(
                Expression::addition(
                    Expression::negation(b.clone()),
                    Expression::negation(Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Sqrt,
                        vec![discriminant.clone()],
                    ))),
                ),
                Expression::multiplication(Expression::Number(2), a.clone()),
            ).simplify()]
        }
    } else {
        vec![Expression::fraction(
            Expression::addition(
                Expression::negation(b.clone()),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sqrt,
                    vec![discriminant.clone()],
                )),
            ),
            Expression::multiplication(Expression::Number(2), a.clone()),
        ).simplify(), Expression::fraction(
            Expression::addition(
                Expression::negation(b.clone()),
                Expression::negation(Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sqrt,
                    vec![discriminant.clone()],
                ))),
            ),
            Expression::multiplication(Expression::Number(2), a.clone()),
        ).simplify()]
    }
}

#[derive(Debug, Clone)]
pub struct Monome {
    pub degree: i64,
    pub coefficient: Expression,
}

impl Monome {
    pub fn new(degree: i64, coefficient: Expression) -> Self {
        Self {
            degree,
            coefficient,
        }
    }

    pub fn get_all(&self) -> (i64, Expression) {
        (self.degree, self.coefficient.clone())
    }
}

fn exponent_to_right(equality: &mut Equality) {
    if let Expression::Exponentiation(expo) = equality.get_left_side() {
        let var_base: Option<_> = expo.get_base().contain_vars();
        let var_exponent: Option<_> = expo.get_exponent().contain_vars();

        if var_base.is_some() && var_exponent.is_none() {
            equality.simplified = false;
            equality.replace_left_side(expo.get_base());
            equality.replace_right_side(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Root,
                    vec![expo.get_exponent(), equality.get_right_side()],
                ))
                .simplify(),
            );
        } else if var_base.is_none() && var_exponent.is_some() {
            equality.simplified = false;
            equality.replace_left_side(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Log,
                    vec![expo.get_base(), Expression::Exponentiation(expo.clone())],
                ))
                .simplify(),
            );
            equality.replace_right_side(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Log,
                    vec![expo.get_base(), equality.get_right_side()],
                ))
                .simplify(),
            );
        }
    }
}

fn addition_to_right(equality: &mut Equality) {
    if let Expression::Addition(mut add) = equality.get_left_side() {
        let mut remove_indices: Vec<usize> = Vec::new();
        let mut new_right = Addition::from_vec(Vec::new());

        for i in 0..add.sub_expr.len() {
            if add.sub_expr[i].contain_vars().is_none() {
                new_right.sub_expr.push(add.sub_expr[i].clone());
                remove_indices.push(i);
            }
        }
        remove_indices.iter().rev().for_each(|indice| {
            add.sub_expr.remove(*indice);
        });

        add.simplified = false;
        equality.replace_left_side(Expression::Addition(add).simplify());
        equality.replace_right_side(
            Expression::addition(
                equality.get_right_side(),
                Expression::negation(Expression::Addition(Box::new(new_right))),
            )
            .simplify(),
        );
    }
}

fn multiplication_to_right(equality: &mut Equality) {
    if let Expression::Multiplication(mut mult) = equality.get_left_side() {
        let mut remove_indices: Vec<usize> = Vec::new();
        let mut new_right = Multiplication::from_vec(Vec::new());

        for i in 0..mult.sub_expr.len() {
            if mult.sub_expr[i].contain_vars().is_none() {
                new_right.sub_expr.push(mult.sub_expr[i].clone());
                remove_indices.push(i);
            }
        }

        remove_indices.iter().rev().for_each(|indice| {
            mult.sub_expr.remove(*indice);
        });

        mult.simplified = false;
        equality.replace_left_side(Expression::Multiplication(mult).simplify());
        equality.replace_right_side(
            Expression::fraction(
                equality.get_right_side(),
                Expression::Multiplication(Box::new(new_right)),
            )
            .simplify(),
        );
    }
}

fn fraction_to_right(equality: &mut Equality) {
    if let Expression::Fraction(frac) = equality.get_left_side() {
        match (
            frac.get_denominator().contain_vars(),
            frac.get_numerator().contain_vars(),
        ) {
            (Some(_), Some(_)) => {
                equality.simplified = false;
                equality.replace_left_side(Expression::Number(1));
                equality.replace_right_side(
                    Expression::multiplication(
                        equality.get_right_side(),
                        Expression::fraction(frac.get_numerator(), frac.get_denominator()),
                    )
                    .simplify(),
                );
            }
            (None, Some(_)) => {
                equality.simplified = false;
                equality.replace_left_side(frac.get_numerator());
                equality.replace_right_side(
                    Expression::multiplication(equality.get_right_side(), frac.get_denominator())
                        .simplify(),
                );
            }
            (Some(_), None) => {
                equality.simplified = false;
                equality.replace_left_side(frac.get_denominator());
                equality.replace_right_side(
                    Expression::fraction(frac.get_numerator(), equality.get_right_side())
                        .simplify(),
                );
            }
            (None, None) => {}
        }
    }
}

fn function_to_right(equality: &mut Equality) {
    if let Expression::Function(fun) = equality.get_left_side() {
        if let FunctionType::Predefined(name, args) = *fun {
            match name {
                PredefinedFunction::Sin => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Asin,
                            vec![equality.get_right_side()],
                        ))
                        .simplify(),
                    );
                }
                PredefinedFunction::Cos => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Acos,
                            vec![equality.get_right_side()],
                        ))
                        .simplify(),
                    );
                }
                PredefinedFunction::Tan => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Atan,
                            vec![equality.get_right_side()],
                        ))
                        .simplify(),
                    );
                }
                PredefinedFunction::Asin => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Sin,
                            vec![equality.get_right_side()],
                        ))
                        .simplify(),
                    );
                }
                PredefinedFunction::Acos => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Cos,
                            vec![equality.get_right_side()],
                        ))
                        .simplify(),
                    );
                }
                PredefinedFunction::Atan => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::function(FunctionType::Predefined(
                            PredefinedFunction::Tan,
                            vec![equality.get_right_side()],
                        ))
                        .simplify(),
                    );
                }
                PredefinedFunction::Sqrt => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::exponentiation(
                            equality.get_right_side(),
                            Expression::Number(2),
                        )
                        .simplify(),
                    )
                }
                PredefinedFunction::Root => {
                    equality.simplified = false;
                    equality.replace_left_side(args[1].clone());
                    equality.replace_right_side(
                        Expression::exponentiation(equality.get_right_side(), args[0].clone())
                            .simplify(),
                    )
                }
                PredefinedFunction::Ln => {
                    equality.simplified = false;
                    equality.replace_left_side(args[0].clone());
                    equality.replace_right_side(
                        Expression::exponentiation(
                            Expression::Constant(ConstantKind::E),
                            equality.get_right_side(),
                        )
                        .simplify(),
                    );
                }
                PredefinedFunction::Log => {
                    equality.simplified = false;
                    equality.replace_left_side(args[1].clone());
                    equality.replace_right_side(
                        Expression::exponentiation(args[0].clone(), equality.get_right_side())
                            .simplify(),
                    );
                }
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod test_single_variable_solve {
    use crate::ast::{
        function::{FunctionType, PredefinedFunction},
        ConstantKind,
    };

    use super::*;

    #[test]
    fn test_addition_to_right() {
        // x + 2 + y + 3 + z + 4 + w + 5 = 1
        let mut equality = Equality::new(
            Expression::addition_from_vec(vec![
                Expression::Variable('x'),
                Expression::Number(2),
                Expression::Variable('y'),
                Expression::Number(3),
                Expression::Variable('z'),
                Expression::Number(4),
                Expression::Variable('w'),
                Expression::Number(5),
            ]),
            Expression::Number(1),
        );

        addition_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::addition_from_vec(vec![
                Expression::Variable('x'),
                Expression::Variable('y'),
                Expression::Variable('z'),
                Expression::Variable('w'),
            ]),
            Expression::Number(-13),
        )));

        // x + sin(pi) = 1
        let mut equality = Equality::new(
            Expression::addition_from_vec(vec![
                Expression::Variable('x'),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )),
            ]),
            Expression::Number(1),
        );

        addition_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::addition(
                Expression::Number(1),
                Expression::negation(Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )))
            )
        )));
    }

    #[test]
    fn test_multiplication_to_right() {
        // x * 2 * y * 3 * z * 4 * w * 5 = 1
        let mut equality = Equality::new(
            Expression::multiplication_from_vec(vec![
                Expression::Variable('x'),
                Expression::Number(2),
                Expression::Variable('y'),
                Expression::Number(3),
                Expression::Variable('z'),
                Expression::Number(4),
                Expression::Variable('w'),
                Expression::Number(5),
            ]),
            Expression::Number(1),
        );

        multiplication_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::multiplication_from_vec(vec![
                Expression::Variable('x'),
                Expression::Variable('y'),
                Expression::Variable('z'),
                Expression::Variable('w'),
            ]),
            Expression::fraction(Expression::Number(1), Expression::Number(120))
        )));

        // x * sin(pi) = 1
        let mut equality = Equality::new(
            Expression::multiplication_from_vec(vec![
                Expression::Variable('x'),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )),
            ]),
            Expression::Number(1),
        );

        multiplication_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::fraction(
                Expression::Number(1),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )),
            )
        )));
    }

    #[test]
    fn test_fraction_to_right() {
        // x / 2 = 1
        let mut equality = Equality::new(
            Expression::fraction(Expression::Variable('x'), Expression::Number(2)),
            Expression::Number(1),
        );

        fraction_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Number(2),
        )));

        // 2 / x = 1
        let mut equality = Equality::new(
            Expression::fraction(Expression::Number(2), Expression::Variable('x')),
            Expression::Number(1),
        );

        fraction_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Number(2),
        )));

        // x / sin(pi) = 1
        let mut equality = Equality::new(
            Expression::fraction(
                Expression::Variable('x'),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )),
            ),
            Expression::Number(1),
        );

        fraction_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Constant(ConstantKind::Pi)],
            ))
        )));
    }

    #[test]
    fn test_exponentiation_to_right() {
        // x ^ 2 = 25
        let mut equality = Equality::new(
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
            Expression::Number(25),
        );

        exponent_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Number(5)
        )));

        // 2 ^ x = 32
        let mut equality = Equality::new(
            Expression::exponentiation(Expression::Number(2), Expression::Variable('x')),
            Expression::Number(32),
        );

        exponent_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Number(5)
        )));
    }

    #[test]
    fn test_function_to_right() {
        // sin(x) = 1
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')],
            )),
            Expression::Number(1),
        );

        function_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Asin,
                vec![Expression::Number(1)],
            )),
        )));

        // asin(x) = 1
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Asin,
                vec![Expression::Variable('x')],
            )),
            Expression::Number(1),
        );

        function_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Number(1)],
            )),
        )));

        // ln(x) = 1
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')],
            )),
            Expression::Number(1),
        );

        function_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Constant(ConstantKind::E),
        )));

        // log(2, x) = 2
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::Number(2), Expression::Variable('x')],
            )),
            Expression::Number(2),
        );

        function_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Number(4),
        ),));

        // sqrt(x) = 5
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sqrt,
                vec![Expression::Variable('x')],
            )),
            Expression::Number(5),
        );

        function_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Number(25),
        )));

        // root(3, x) = 5
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Root,
                vec![Expression::Number(3), Expression::Variable('x')],
            )),
            Expression::Number(5),
        );

        function_to_right(&mut equality);

        assert!(equality.equal(&Equality::new(
            Expression::Variable('x'),
            Expression::Number(125)
        )));
    }
}
