mod singleveriable;

use crate::ast::{
    addition::Addition, equality::Equality, multiplication::Multiplication, Expression,
};

use self::singleveriable::solve_one_var_one_occurence;

pub fn solve(expression: Expression) -> Expression {
    solver(if let Expression::Equality(equality) = expression {
        all_to_left_side(*equality)
    } else {
        Equality::new(expression, Expression::Number(0))
    })
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
    // Expression::Equality(Box::new(equality.clone())).print_tree(None);
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

        let result = solve(expression);

        assert!(result.equal(&Expression::equality(
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

        let result = solve(expression);
        
        assert!(result.equal(&Expression::equality(
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

        let result = solve(expression);
        result.print_tree(None);

        assert!(result.equal(&Expression::equality(
            Expression::Variable('x'),
            Expression::Number(89)
        )));
    }
}
