use crate::ast::{
    addition::Addition,
    equality::Equality,
    function::{FunctionType, PredefinedFunction},
    multiplication::Multiplication,
    Expression, varibale::Variable, Expr, constant::ConstantKind,
};

use crate::utils::{
    extract_coefficient_expression_exponent, prime_factor, ExpressionExponent, PrimeFactor,
};

pub fn solve_one_var_one_occurence(mut equality: Equality, variable: Variable) -> Expression {
    let mut solved = Equality::new(equality.get_right_side(), equality.get_left_side());
    while !solved.equal(&equality) {
        solved = equality.clone();

        addition_to_right(&mut equality, &variable);

        multiplication_to_right(&mut equality, &variable);

        fraction_to_right(&mut equality, &variable);

        function_to_right(&mut equality, &variable);

        exponent_to_right(&mut equality, &variable);
    }

    Expression::Equality(Box::new(equality)).simplify()
}

pub fn solve_one_var_multiple_occurence(equality: Equality, variable: Variable) -> Vec<Expression> {
    if let Expression::Addition(addition) = equality.get_left_side() {
        let mut components: Vec<(Vec<PrimeFactor>, Vec<ExpressionExponent>)> = Vec::new();
        for expr in addition.sub_expr {
            components.push(extract_coefficient_expression_exponent(expr));
        }

        let mut polynomial: Vec<Monome> = Vec::new();
        // TODO add support for negative numbers for the coefficient degree 0

        // x^2 + 4yx + 3 => (2, 1), (1, 4y) (0, 3)
        // [((1, 1), (x, 2)),  ((2, 2), [(y, 1), (x, 1)]),  ((3, 1), ())]
        components.iter().for_each(|comp| {
            let mut coefficient = Multiplication::from_vec(Vec::new());
            let mut degree: i64 = 0;
            if !comp.1.is_empty() {
                if comp.1.iter().all(|expr_expo| {
                    if expr_expo.expression == Expression::Variable(variable.clone()) {
                        if let Expression::Number(num) = &expr_expo.exponent {
                            degree = num.sub_expr;
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
                        if let Expression::Number(num) = &prime_factor.exponent {
                            coefficient.sub_expr.push(Expression::exponentiation(
                                Expression::number(prime_factor.prime),
                                Expression::number(num.sub_expr),
                            ));
                        }
                    });
                    
                    polynomial.push(Monome::new(
                        degree,
                        Expression::Multiplication(Box::new(coefficient)).simplify(),
                    ));
                }
            } else if comp.0.iter().all(|prime_factor| {
                if let Expression::Number(num) = &prime_factor.exponent {
                    coefficient.sub_expr.push(Expression::exponentiation(
                        Expression::number(prime_factor.prime),
                        Expression::number(num.sub_expr),
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
                println!("Constant: {:?} {:?}", polynomial, comp.0);
            }
        });
        if polynomial.len() == components.len() {
            polynomial.sort_by_key(|monome| std::cmp::Reverse(monome.degree));
            println!("Maybe a polynomial");
            if polynomial[0].degree == 2 && polynomial[1].degree == 1 {
                let solutions = solve_second_degree(polynomial);
                return solutions
                    .iter()
                    .map(|solution| {
                        Expression::equality(Expression::Variable(variable.clone()), solution.clone())
                    })
                    .collect();
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
                    "possible variable swap: {} {} , bcs 2 is a factor",
                    polynomial[0].degree, polynomial[1].degree
                );

                return true;
            } else if (polynomial[0].degree / polynomial[1].degree) == 2 {
                println!(
                    "possible variable swap: {} {} , bcs rapport of 2 between the two",
                    polynomial[0].degree, polynomial[1].degree
                );
                return true;
            }
        }
    }
    false
}

fn solve_second_degree(polynomial: Vec<Monome>) -> Vec<Expression> {
    let mut a = Expression::number(0);
    let mut b = Expression::number(0);
    let mut c = Expression::number(0);

    println!("{:?}", polynomial);

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
        Expression::exponentiation(b.clone(), Expression::number(2)),
        Expression::multiplication_from_vec(vec![Expression::number(-4), a.clone(), c.clone()]),
    )
    .simplify();

    println!("{}{}{}:{}", a, b, c, discriminant);

    if let Expression::Number(num) = discriminant.clone() {
        match num.sub_expr {
            0 => {
                println!("One solution");
                vec![Expression::fraction(
                    Expression::negation(b.clone()),
                    Expression::multiplication(Expression::number(2), a.clone()),
                )
                .simplify()]
            }
            _x if num.sub_expr < 0 => {
                println!("Complex solution");
                vec![]
            }
            _ => {
                println!("Two solutions");
                vec![
                    Expression::fraction(
                        Expression::addition(
                            Expression::negation(b.clone()),
                            Expression::function(FunctionType::Predefined(
                                PredefinedFunction::Sqrt,
                                vec![discriminant.clone()],
                            )),
                        ),
                        Expression::multiplication(Expression::number(2), a.clone()),
                    )
                    .simplify(),
                    Expression::fraction(
                        Expression::addition(
                            Expression::negation(b.clone()),
                            Expression::negation(Expression::function(FunctionType::Predefined(
                                PredefinedFunction::Sqrt,
                                vec![discriminant.clone()],
                            ))),
                        ),
                        Expression::multiplication(Expression::number(2), a.clone()),
                    )
                    .simplify(),
                ]
            }
        }
    } else {
        vec![
            Expression::fraction(
                Expression::addition(
                    Expression::negation(b.clone()),
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Sqrt,
                        vec![discriminant.clone()],
                    )),
                ),
                Expression::multiplication(Expression::number(2), a.clone()),
            )
            .simplify(),
            Expression::fraction(
                Expression::addition(
                    Expression::negation(b.clone()),
                    Expression::negation(Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Sqrt,
                        vec![discriminant.clone()],
                    ))),
                ),
                Expression::multiplication(Expression::number(2), a.clone()),
            )
            .simplify(),
        ]
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

    // pub fn get_all(&self) -> (i64, Expression) {
    //     (self.degree, self.coefficient.clone())
    // }
}

fn exponent_to_right(equality: &mut Equality, variable: &Variable) {
    if let Expression::Exponentiation(expo) = equality.get_left_side() {
        let var_base: bool = expo.get_base().contain_var(variable);
        let var_exponent: bool = expo.get_exponent().contain_var(variable);

        if var_base && !var_exponent {
            equality.simplified = false;
            equality.replace_left_side(expo.get_base());
            equality.replace_right_side(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Root,
                    vec![expo.get_exponent(), equality.get_right_side()],
                ))
                .simplify(),
            );
        } else if !var_base && var_exponent {
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

fn addition_to_right(equality: &mut Equality, variable: &Variable) {
    if let Expression::Addition(mut add) = equality.get_left_side() {
        let mut remove_indices: Vec<usize> = Vec::new();
        let mut new_right = Addition::from_vec(Vec::new());

        for i in 0..add.sub_expr.len() {
            if !add.sub_expr[i].contain_var(variable) {
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

fn multiplication_to_right(equality: &mut Equality, variable: &Variable) {
    if let Expression::Multiplication(mut mult) = equality.get_left_side() {
        let mut remove_indices: Vec<usize> = Vec::new();
        let mut new_right = Multiplication::from_vec(Vec::new());

        for i in 0..mult.sub_expr.len() {
            if !mult.sub_expr[i].contain_var(variable) {
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

fn fraction_to_right(equality: &mut Equality, variable: &Variable) {
    if let Expression::Fraction(frac) = equality.get_left_side() {
        match (
            frac.get_denominator().contain_var(variable),
            frac.get_numerator().contain_var(variable),
        ) {
            (true, true) => {
                equality.simplified = false;
                equality.replace_left_side(Expression::number(1));
                equality.replace_right_side(
                    Expression::multiplication(
                        equality.get_right_side(),
                        Expression::fraction(frac.get_numerator(), frac.get_denominator()),
                    )
                    .simplify(),
                );
            }
            (false, true) => {
                equality.simplified = false;
                equality.replace_left_side(frac.get_numerator());
                equality.replace_right_side(
                    Expression::multiplication(equality.get_right_side(), frac.get_denominator())
                        .simplify(),
                );
            }
            (true, false) => {
                equality.simplified = false;
                equality.replace_left_side(frac.get_denominator());
                equality.replace_right_side(
                    Expression::fraction(frac.get_numerator(), equality.get_right_side())
                        .simplify(),
                );
            }
            (false, false) => {}
        }
    }
}

fn function_to_right(equality: &mut Equality, _variable: &Variable) {
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
                            Expression::number(2),
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
        Expr,
    };

    use super::*;

    #[test]
    fn test_addition_to_right() {
        // x + 2 + y + 3 + z + 4 + w + 5 = 1
        // x + y + z + 13 = -w
        let mut equality = Equality::new(
            Expression::addition_from_vec(vec![
                Expression::variable("x".to_string()),
                Expression::number(2),
                Expression::variable("y".to_string()),
                Expression::number(3),
                Expression::variable("z".to_string()),
                Expression::number(4),
                Expression::variable("w".to_string()),
                Expression::number(5),
            ]),
            Expression::number(0),
        );

        addition_to_right(&mut equality, &Variable::new("w".to_string()));

        assert!(equality.equal(&Equality::new(
            Expression::variable("w".to_string()),
            Expression::negation(Expression::addition_from_vec(vec![
                Expression::variable("x".to_string()),
                Expression::variable("y".to_string()),
                Expression::variable("z".to_string()),
                Expression::number(14),
            ]))
            .simplify(),
        )));

        // x + sin(pi) = 1
        // x = 1 - sin(pi)
        let mut equality = Equality::new(
            Expression::addition_from_vec(vec![
                Expression::variable("x".to_string()),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )),
            ]),
            Expression::number(1),
        );

        addition_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(equality.equal(&Equality::new(
            Expression::variable("x".to_string()),
            Expression::addition(
                Expression::number(1),
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
        // x = 1 / (120 * y * z * w)
        let mut equality = Equality::new(
            Expression::multiplication_from_vec(vec![
                Expression::variable("x".to_string()),
                Expression::number(2),
                Expression::variable("y".to_string()),
                Expression::number(3),
                Expression::variable("z".to_string()),
                Expression::number(4),
                Expression::variable("w".to_string()),
                Expression::number(5),
            ]),
            Expression::number(1),
        );

        multiplication_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::fraction(
                    Expression::number(1),
                    Expression::multiplication_from_vec(vec![
                        Expression::number(120),
                        Expression::variable("y".to_string()),
                        Expression::variable("z".to_string()),
                        Expression::variable("w".to_string()),
                    ])
                )
            )),
            "Expected: x = 1 / (120 * y * z * w)\nGot: x = {}",
            equality
        );

        // x * sin(pi) = 1
        // x = 1 / sin(pi)
        let mut equality = Equality::new(
            Expression::multiplication_from_vec(vec![
                Expression::variable("x".to_string()),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )),
            ]),
            Expression::number(1),
        );

        multiplication_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::fraction(
                    Expression::number(1),
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Sin,
                        vec![Expression::Constant(ConstantKind::Pi)],
                    ))
                )
            )),
            "Expected: x = 1 / sin(pi)\nGot: x = {}",
            equality
        );
    }

    #[test]
    fn test_fraction_to_right() {
        // x / 2 = 1
        // x = 2
        let mut equality = Equality::new(
            Expression::fraction(Expression::variable("x".to_string()), Expression::number(2)),
            Expression::number(1),
        );

        fraction_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::number(2),
            )),
            "Expected: x = 2\nGot: x = {}",
            equality
        );

        // 2 / x = 1
        // x = 2
        let mut equality = Equality::new(
            Expression::fraction(Expression::number(2), Expression::variable("x".to_string())),
            Expression::number(1),
        );

        fraction_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::number(2),
            )),
            "Expected: x = 2\nGot: {}",
            equality
        );

        // x / sin(pi) = 1
        // x = sin(pi)
        let mut equality = Equality::new(
            Expression::fraction(
                Expression::variable("x".to_string()),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                )),
            ),
            Expression::number(1),
        );

        fraction_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Constant(ConstantKind::Pi)],
                ))
            )),
            "Expected: x = sin(pi)\nGot: x = {}",
            equality
        );
    }

    #[test]
    fn test_exponentiation_to_right() {
        // x ^ 2 = 25
        // x = 5
        let mut equality = Equality::new(
            Expression::exponentiation(Expression::variable("x".to_string()), Expression::number(2)),
            Expression::number(25),
        );

        exponent_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::number(5)
            )),
            "Expected: x = 5\nGot: x = {}",
            equality
        );

        // 2 ^ x = 32
        // x = 5
        let mut equality = Equality::new(
            Expression::exponentiation(Expression::number(2), Expression::variable("x".to_string())),
            Expression::number(32),
        );

        exponent_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::number(5)
            )),
            "Expected: x = 5\nGot: x = {}",
            equality
        );
    }

    #[test]
    fn test_function_to_right() {
        // sin(x) = 1
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::variable("x".to_string())],
            )),
            Expression::number(1),
        );

        function_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Asin,
                    vec![Expression::number(1)],
                )),
            )),
            "Expected: x = asin(1)\nGot: x = {}",
            equality
        );

        // asin(x) = 1
        // x = sin(1)
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Asin,
                vec![Expression::variable("x".to_string())],
            )),
            Expression::number(1),
        );

        function_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::number(1)],
                )),
            )),
            "Expected: x = sin(1)\nGot: x = {}",
            equality
        );

        // ln(x) = 1
        // x = e
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::variable("x".to_string())],
            )),
            Expression::number(1),
        );

        function_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::Constant(ConstantKind::E),
            )),
            "Expected: x = e\nGot: x = {}",
            equality
        );

        // log(2, x) = 2
        // x = 4
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::number(2), Expression::variable("x".to_string())],
            )),
            Expression::number(2),
        );

        function_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::number(4),
            )),
            "Expected: x = 4\nGot: x = {}",
            equality
        );

        // sqrt(x) = 5
        // x = 25
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sqrt,
                vec![Expression::variable("x".to_string())],
            )),
            Expression::number(5),
        );

        function_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::number(25),
            )),
            "Expected: x = 25\nGot: x = {}",
            equality
        );

        // root(3, x) = 5
        // x = 125
        let mut equality = Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Root,
                vec![Expression::number(3), Expression::variable("x".to_string())],
            )),
            Expression::number(5),
        );

        function_to_right(&mut equality, &Variable::new("x".to_string()));

        assert!(
            equality.equal(&Equality::new(
                Expression::variable("x".to_string()),
                Expression::number(125)
            )),
            "Expected: x = 125\nGot: x = {}",
            equality
        );
    }
}
