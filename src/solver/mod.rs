mod singleveriable;

use crate::{
    ast::{equality::Equality, Expression},
    solver::singleveriable::solve_one_var_multiple_occurence,
};

use self::singleveriable::solve_one_var_one_occurence;

pub fn solve(expression: Expression, variable: Option<char>) -> Vec<Expression> {
    solver(
        if let Expression::Equality(equality) = expression {
            all_to_left_side(*equality)
        } else {
            Equality::new(expression, Expression::Number(0))
        },
        None,
    )
}

fn all_to_left_side(mut equality: Equality) -> Equality {
    equality.simplified = false;
    equality.replace_left_side(
        Expression::addition(
            equality.get_left_side(),
            Expression::negation(equality.get_right_side()),
        )
        .simplify(),
    );
    equality.replace_right_side(Expression::Number(0));
    equality
}

fn solver(equality: Equality, variable: Option<char>) -> Vec<Expression> {
    if let Some(variables) = equality.get_left_side().contain_vars() {
        // print!("{:?}", variables);
        match variables.len() {
            1 => {
                if variables.values().all(|occurence| occurence == &1) {
                    vec![solve_one_var_one_occurence(equality)]
                } else {
                    if let Some(var) = variable {
                        println!("Solve for {var}");
                        solve_one_var_multiple_occurence(equality, var)
                    } else {
                        solve_one_var_multiple_occurence(
                            equality,
                            *variables.iter().max_by_key(|var| 
                                var.1
                            ).unwrap().0,
                        )
                    }
                }

                // solve for one variable
                // if variable occur 1 => simple isolation
                // else regroup the variable
            }
            _ => {
                // solve for multiple var
                vec![Expression::Equality(Box::new(equality))]
            }
        }
    } else {
        vec![equality.get_left_side().simplify()]
    }
}

#[cfg(test)]
mod test_solver {
    use crate::ast::function::{FunctionType, PredefinedFunction};

    use super::*;

    #[test]
    fn test_solve_single_var() {
        // 2x + 3 = 0
        let expression = Expression::Equality(Box::new(Equality::new(
            Expression::addition(
                Expression::multiplication(Expression::Number(2), Expression::Variable('x')),
                Expression::Number(3),
            ),
            Expression::Number(0),
        )));

        let result = solve(expression, Some('x'));

        assert!(result[0].equal(&Expression::equality(
            Expression::Variable('x'),
            Expression::fraction(Expression::Number(-3), Expression::Number(2))
        )));

        // 2x + 3 = 4x + 5
        let expression = Expression::Equality(Box::new(Equality::new(
            Expression::addition(
                Expression::multiplication(Expression::Number(2), Expression::Variable('x')),
                Expression::Number(3),
            ),
            Expression::addition(
                Expression::multiplication(Expression::Number(4), Expression::Variable('x')),
                Expression::Number(5),
            ),
        )));

        let result = solve(expression, Some('x'));

        assert!(result[0].equal(&Expression::equality(
            Expression::Variable('x'),
            Expression::fraction(Expression::Number(-2), Expression::Number(2))
        )));

        // sqrt(x - 8) = 9
        let expression = Expression::Equality(Box::new(Equality::new(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sqrt,
                vec![Expression::addition(
                    Expression::Variable('x'),
                    Expression::Number(-8),
                )],
            )),
            Expression::Number(9),
        )));

        let result = solve(expression, Some('x'));

        assert!(result[0].equal(&Expression::equality(
            Expression::Variable('x'),
            Expression::Number(89)
        )));

        // x^2 + 2x + 1 = 0
        let expression = Expression::equality(
            Expression::addition(
                Expression::addition(
                    Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
                    Expression::multiplication(Expression::Number(2), Expression::Variable('x')),
                ),
                Expression::Number(1),
            ),
            Expression::Number(0),
        );

        let result = solve(expression, Some('x'));

        assert!(result[0].equal(&Expression::equality(
            Expression::Variable('x'),
            Expression::Number(-1)
        )));

        // ax^2 + bx + c = 0
    }
}
