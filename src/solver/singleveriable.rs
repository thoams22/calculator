use crate::ast::{
    addition::Addition,
    equality::Equality,
    fraction::Fraction,
    function::{FunctionType, PredefinedFunction},
    multiplication::Multiplication,
    ConstantKind, Expression,
};

pub fn solve_one_var_one_occurence(mut equality: Equality) -> Equality {
    let mut solved = Equality::new(equality.get_right_side(), equality.get_left_side());

    while !solved.equal(&equality) {
        solved = equality.clone();

        addition_to_right(&mut equality);

        multiplication_to_right(&mut equality);

        fraction_to_right(&mut equality);

        function_to_right(&mut equality);

        exponent_to_right(&mut equality);
    }

    equality
}

fn exponent_to_right(equality: &mut Equality) {
    if let Expression::Exponentiation(expo) = equality.get_left_side() {
        let var_base: Option<_> = expo.get_base().contain_var();
        let var_exponent: Option<_> = expo.get_exponent().contain_var();

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
            if add.sub_expr[i].contain_var().is_none() {
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
            if mult.sub_expr[i].contain_var().is_none() {
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
            frac.get_denominator().contain_var(),
            frac.get_numerator().contain_var(),
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
