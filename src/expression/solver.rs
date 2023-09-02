use crate::expression::{equality::Equality, Expression};

use super::{addition::Addition, function::FunctionsType, multiplication::Multiplication};

pub fn solve(expression: Expression) -> Expression {
    solver(if let Expression::Equality(equality) = &expression {
        all_to_left_side(*equality.clone())
    } else {
        Equality::new(expression.simplify(), Expression::Number(0.0))
    })
}

fn all_to_left_side(mut equality: Equality) -> Equality {
    if let Expression::Multiplication(_) = equality.get_right_side() {
        equality.replace_left_side(
            Expression::division(equality.get_left_side(), equality.get_right_side()).simplify(),
        )
    } else {
        equality.replace_left_side(
            Expression::subtraction(equality.get_left_side(), equality.get_right_side()).simplify(),
        )
    }
    equality.replace_right_side(Expression::Number(0.0));
    equality
}

fn solver(mut equality: Equality) -> Expression {
    if let Some(variables) = equality.get_left_side().contain_var() {
        match variables.len() {
            // 0 => equality.get_left_side().simplify();,
            1 => {
                if variables.values().all(|occurence| occurence == &1) {
                    equality = solve_one_var_one_occurence(equality);
                }
                // solve for one variable
                // if variable occur 1 => simple isolation
                // else regroup the variable
            }
            _ => {
                // solve for multiple var
            }
        }
    }

    Expression::Equality(Box::new(equality))
}

// fn solve_one_var_multiple_occurence(mut equality: Equality) -> Equality {

// }

fn solve_one_var_one_occurence(mut equality: Equality) -> Equality {
    let mut solved = Equality::new(equality.get_right_side(), equality.get_left_side());

    while solved != equality {
        solved = equality.clone();

        addition_to_right(&mut equality);

        multiplication_to_right(&mut equality);
        
        function_to_right(&mut equality);
        
        exponent_to_right(&mut equality);
    }

    equality
}

fn function_to_right(equality: &mut Equality) {
    if let Expression::Function(fun) = equality.get_left_side() {
        match fun.get_type() {
            FunctionsType::Logarithm => {
                let base = fun.get_base().unwrap();

                equality.replace_left_side(
                    Expression::exponentiation(base.clone(), equality.get_left_side()).simplify(),
                );
                equality.replace_right_side(
                    Expression::exponentiation(base, equality.get_right_side()).simplify(),
                );
            }
            FunctionsType::Root => {
                let base = fun.get_base().unwrap();
                let expression = fun.get_expression();

                equality.replace_left_side(expression);
                equality.replace_right_side(
                    Expression::exponentiation(equality.get_right_side(), base).simplify(),
                );
            }
            FunctionsType::Trigonometric => {
                let expression = fun.get_expression();
                match fun.get_function() {
                    super::function::Functions::Sin => {
                        equality.replace_left_side(expression.simplify());
                        equality.replace_right_side(
                            Expression::asin(equality.get_right_side()).simplify(),
                        );
                    }
                    super::function::Functions::Cos => {
                        equality.replace_left_side(expression.simplify());
                        equality.replace_right_side(
                            Expression::acos(equality.get_right_side()).simplify(),
                        );
                    }
                    super::function::Functions::Tan => {
                        equality.replace_left_side(expression.simplify());
                        equality.replace_right_side(
                            Expression::atan(equality.get_right_side()).simplify(),
                        );
                    }
                    super::function::Functions::Asin => {
                        equality.replace_left_side(expression.simplify());
                        equality.replace_right_side(
                            Expression::sin(equality.get_right_side()).simplify(),
                        );
                    }
                    super::function::Functions::Acos => {
                        equality.replace_left_side(expression.simplify());
                        equality.replace_right_side(
                            Expression::cos(equality.get_right_side()).simplify(),
                        );
                    }
                    super::function::Functions::Atan => {
                        equality.replace_left_side(expression.simplify());
                        equality.replace_right_side(
                            Expression::tan(equality.get_right_side()).simplify(),
                        );
                    }
                    _ => {}
                }
            }
        }
    }
}

fn exponent_to_right(equality: &mut Equality) {
    if let Expression::Exponentiation(expo) = equality.get_left_side() {
        let base = expo.get_base();
        let exponent = expo.get_exponent();

        equality.replace_left_side(base);
        equality.replace_right_side(
            Expression::exponentiation(
                equality.get_right_side(),
                Expression::division(Expression::Number(1.0), exponent),
            )
            .simplify(),
        );
    }
}

fn addition_to_right(equality: &mut Equality) {
    if let Expression::Addition(mut add) = equality.get_left_side() {
        let mut remove_indices: Vec<usize> = Vec::new();
        let mut new_right = Addition::from_vec(Vec::new());

        for i in 0..add.len() {
            if let None = add.get(i).unwrap().contain_var() {
                new_right.push(add.get(i).unwrap().clone());
                remove_indices.push(i);
            }
        }
        remove_indices.iter().rev().for_each(|indice| {
            add.remove(*indice);
        });

        add.set_simplified(false);
        equality.replace_left_side(Expression::Addition(add).simplify());
        equality.replace_right_side(
            Expression::subtraction(
                equality.get_right_side(),
                Expression::Addition(Box::new(new_right)),
            )
            .simplify(),
        );
    }
}

fn multiplication_to_right(equality: &mut Equality) {
    if let Expression::Multiplication(mut mult) = equality.get_left_side() {
        let mut remove_indices: Vec<usize> = Vec::new();
        let mut new_right = Multiplication::from_vec(Vec::new());

        for i in 0..mult.len() {
            if let None = mult.get(i).unwrap().contain_var() {
                new_right.push(mult.get(i).unwrap().clone());
                remove_indices.push(i);
            }
        }

        remove_indices.iter().rev().for_each(|indice| {
            mult.remove(*indice);
        });

        mult.set_simplified(false);
        equality.replace_left_side(Expression::Multiplication(mult).simplify());
        equality.replace_right_side(
            Expression::division(
                equality.get_right_side(),
                Expression::Multiplication(Box::new(new_right)),
            )
            .simplify(),
        );
    }
}

#[cfg(test)]
mod tests_solver {
    use crate::expression::{solver::solve, Expression};

    #[test]
    fn solve_one_var_one_occurence() {
        // x = 1
        let mut result = solve(Expression::equality(
            Expression::Variable('x'),
            Expression::Number(1.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(1.0)
            )),
            "solve_one_var_one_occurence 1\n{:?}",
            result
        );

        // 2x = 1
        result = solve(Expression::equality(
            Expression::multiplication(Expression::Variable('x'), Expression::Number(2.0)),
            Expression::Number(1.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(0.5)
            )),
            "solve_one_var_one_occurence 2\n{:?}",
            result
        );

        // 2x = 1
        result = solve(Expression::equality(
            Expression::division(Expression::Variable('x'), Expression::Number(2.0)),
            Expression::Number(1.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(2.0)
            )),
            "solve_one_var_one_occurence 3\n{:?}",
            result
        );

        // 2x - 1 = 0
        result = solve(Expression::subtraction(
            Expression::multiplication(Expression::Variable('x'), Expression::Number(2.0)),
            Expression::Number(1.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(0.5)
            )),
            "solve_one_var_one_occurence 4\n{:?}",
            result
        );

        // 2x = 0
        result = solve(Expression::multiplication(
            Expression::Variable('x'),
            Expression::Number(2.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(0.0)
            )),
            "solve_one_var_one_occurence 5\n{:?}",
            result
        );

        // x^2 = 0
        result = solve(Expression::exponentiation(
            Expression::Variable('x'),
            Expression::Number(2.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(0.0)
            )),
            "solve_one_var_one_occurence 6\n{:?}",
            result
        );

        // x^2 = 1
        result = solve(Expression::equality(
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2.0)),
            Expression::Number(1.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(1.0)
            )),
            "solve_one_var_one_occurence 7\n{:?}",
            result
        );

        // (x+4*2)^2 = 4
        result = solve(Expression::equality(
            Expression::exponentiation(
                Expression::addition(
                    Expression::Variable('x'),
                    Expression::multiplication(Expression::Number(4.0), Expression::Number(2.0)),
                ),
                Expression::Number(2.0),
            ),
            Expression::Number(4.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(-6.0)
            )),
            "solve_one_var_one_occurence 8\n{:?}",
            result
        );

        // log2(x) = 4
        result = solve(Expression::equality(
            Expression::log2(Expression::Variable('x')),
            Expression::Number(4.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(16.0)
            )),
            "solve_one_var_one_occurence 9\n{:?}",
            result
        );

        // log2(x+9/8) = 4
        result = solve(Expression::equality(
            Expression::log2(Expression::addition(
                Expression::Variable('x'),
                Expression::division(Expression::Number(9.0), Expression::Number(8.0)),
            )),
            Expression::Number(4.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(14.875)
            )),
            "solve_one_var_one_occurence 10\n{:?}",
            result
        );

        // sqrt(x) = 5
        result = solve(Expression::equality(
            Expression::sqrt(Expression::Variable('x')),
            Expression::Number(5.0),
        ));
        assert!(
            result.equal(&Expression::equality(
                Expression::Variable('x'),
                Expression::Number(25.0)
            )),
            "solve_one_var_one_occurence 11\n{:?}",
            result
        );
    }
}
