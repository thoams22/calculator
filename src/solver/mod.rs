mod singleveriable;

use crate::{
    ast::{equality::Equality, varibale::Variable, Expr, Expression, Statement},
    solver::singleveriable::solve_one_var_multiple_occurence,
};

use self::singleveriable::solve_one_var_one_occurence;

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum SolverError {
    #[error("Invalid varibale: Ask to solve for {0} but {0} not in the expression")]
    InvalidVariable(Variable),
}

// pub struct Evaluator {
//     statement: Statement,
// }

// impl Evaluator {
//     pub fn new(statement: Statement) -> Self {
//         Self { statement }
//     }

//     // pub fn evaluate(&mut self, ) -> {}
// }

pub fn solve(
    mut expression: Expression,
    variable: Option<Variable>,
) -> Result<Vec<Expression>, SolverError> {
    expression = expression.simplify();
    solver(
        if let Expression::Equality(equality) = expression {
            all_to_left_side(*equality)
        } else {
            Equality::new(expression, Expression::number(0))
        },
        variable,
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
    equality.replace_right_side(Expression::number(0));
    equality
}

fn solver(
    mut equality: Equality,
    variable: Option<Variable>,
) -> Result<Vec<Expression>, SolverError> {
    if let Some(variables) = equality.get_left_side().contain_vars() {
        if let Some(var) = variable {
            if let Some(occurence) = variables.get(&var) {
                match occurence {
                    1 => Ok(vec![solve_one_var_one_occurence(equality, var)]),
                    _ => Ok(solve_one_var_multiple_occurence(equality, var)),
                }
            } else {
                Err(SolverError::InvalidVariable(var))
            }
        } else if variables.len() == 1 {
            let var = variables.keys().next().unwrap();
            if variables.get(var).unwrap() == &1 {
                Ok(vec![solve_one_var_one_occurence(equality, var.clone())])
            } else {
                Ok(solve_one_var_multiple_occurence(
                    equality,
                    variables.iter().max_by_key(|var| var.1).unwrap().0.clone(),
                ))
            }
        } else if let Some(occurence) = variables.get(&Variable::new("x".to_string())) {
            match occurence {
                1 => Ok(vec![solve_one_var_one_occurence(
                    equality,
                    Variable::new("x".to_string()),
                )]),
                _ => Ok(solve_one_var_multiple_occurence(
                    equality,
                    Variable::new("x".to_string()),
                )),
            }
        } else {
            let var = variables.keys().next().unwrap();
            if variables.get(var).unwrap() == &1 {
                Ok(vec![solve_one_var_one_occurence(equality, var.clone())])
            } else {
                Ok(solve_one_var_multiple_occurence(
                    equality,
                    variables.iter().max_by_key(|var| var.1).unwrap().0.clone(),
                ))
            }
        }
    } else {
        equality.simplified = false;
        Ok(vec![equality.simplify()])
    }
}

#[cfg(test)]
mod test_solver {
    use crate::ast::function::{FunctionType, PredefinedFunction};

    use super::*;

    #[test]
    fn test_solve_single_var() {
        fn verify_solver(
            found: &Result<Vec<Expression>, SolverError>,
            expected: &Result<Vec<Expression>, SolverError>,
        ) {
            assert!(match (found, expected) {
                (Ok(fnd), Ok(xpct)) => fnd.iter().zip(xpct.iter()).all(|results| {
                    results.0.equal(results.1)
                }),
                (Err(fnd), Err(xpct)) => fnd == xpct,
                _ => false
            },
            "\nFound: {:?}\nExpected: {:?}", found, expected)
        }

        // 2x + 3 = 0
        verify_solver(
            &solve(
                Expression::Equality(Box::new(Equality::new(
                    Expression::addition(
                        Expression::multiplication(
                            Expression::number(2),
                            Expression::variable("x".to_string()),
                        ),
                        Expression::number(3),
                    ),
                    Expression::number(0),
                ))),
                Some(Variable::new("x".to_string())),
            ),
            &Ok(vec![Expression::equality(
                Expression::variable("x".to_string()),
                Expression::fraction(Expression::number(-3), Expression::number(2)),
            )]),
        );

        // x^2 + 1 = 0
        // verify_solver(
        //     &solve(
        //         Expression::Equality(Box::new(Equality::new(
        //             Expression::addition(
        //                 Expression::exponentiation(
        //                     Expression::variable(String::from("x")),
        //                     Expression::number(2),
        //                 ),
        //                 Expression::number(1),
        //             ),
        //             Expression::number(0),
        //         ))),
        //         Some(Variable::new("x".to_string())),
        //     ),
        //     &Ok(vec![Expression::equality(
        //         Expression::variable("x".to_string()),
        //         Expression::complex(Expression::number(0), Expression::number(1)),
        //     )]),
        // );

        // 2x + 3 = 4x + 5
        verify_solver(
            &solve(
                Expression::Equality(Box::new(Equality::new(
                    Expression::addition(
                        Expression::multiplication(
                            Expression::number(2),
                            Expression::variable("x".to_string()),
                        ),
                        Expression::number(3),
                    ),
                    Expression::addition(
                        Expression::multiplication(
                            Expression::number(4),
                            Expression::variable("x".to_string()),
                        ),
                        Expression::number(5),
                    ),
                ))),
                Some(Variable::new("x".to_string())),
            ),
            &Ok(vec![Expression::equality(
                Expression::variable("x".to_string()),
                Expression::number(-1),
            )]),
        );

        // sqrt(x - 8) = 9
        verify_solver(
            &solve(
                Expression::Equality(Box::new(Equality::new(
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Sqrt,
                        vec![Expression::addition(
                            Expression::variable("x".to_string()),
                            Expression::number(-8),
                        )],
                    )),
                    Expression::number(9),
                ))),
                Some(Variable::new("x".to_string())),
            ),
            &Ok(vec![Expression::equality(
                Expression::variable("x".to_string()),
                Expression::number(89),
            )]),
        );

        // x^2 + 2x + 1 = 0
        verify_solver(
            &solve(
                Expression::equality(
                    Expression::addition(
                        Expression::addition(
                            Expression::exponentiation(
                                Expression::variable("x".to_string()),
                                Expression::number(2),
                            ),
                            Expression::multiplication(
                                Expression::number(2),
                                Expression::variable("x".to_string()),
                            ),
                        ),
                        Expression::number(1),
                    ),
                    Expression::number(0),
                ),
                Some(Variable::new("x".to_string())),
            ),
            &Ok(vec![Expression::equality(
                Expression::variable("x".to_string()),
                Expression::number(-1),
            )]),
        );

        // ax^2 + bx + c = 0
    }
}
