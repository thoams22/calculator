pub mod addition;
pub mod equality;
pub mod exponentiation;
pub mod fraction;
pub mod function;
pub(crate) mod math;
pub mod multiplication;
pub mod negation;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use self::addition::Addition;
use self::equality::Equality;
use self::exponentiation::Exponentiation;
use self::fraction::Fraction;
use self::function::FunctionType;
use self::multiplication::Multiplication;
use self::negation::Negation;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Number(i64),
    Variable(char),
    Constant(ConstantKind),
    Addition(Box<Addition>),
    Multiplication(Box<Multiplication>),
    Exponentiation(Box<Exponentiation>),
    Fraction(Box<Fraction>),
    Equality(Box<Equality>),
    Negation(Box<Negation>),
    Function(Box<FunctionType>),
    Error,
}

// Helper constructor
impl Expression {
    pub fn addition(left: Expression, right: Expression) -> Expression {
        Expression::Addition(Box::new(Addition::new(left, right)))
    }

    pub fn addition_from_vec(sub_expr: Vec<Expression>) -> Expression {
        Expression::Addition(Box::new(Addition::from_vec(sub_expr)))
    }

    pub fn multiplication(left: Expression, right: Expression) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::new(left, right)))
    }

    pub fn multiplication_from_vec(sub_expr: Vec<Expression>) -> Expression {
        Expression::Multiplication(Box::new(Multiplication::from_vec(sub_expr)))
    }

    pub fn fraction(numerator: Expression, denominator: Expression) -> Expression {
        Expression::Fraction(Box::new(Fraction::new(numerator, denominator)))
    }

    pub fn negation(operand: Expression) -> Expression {
        Expression::Negation(Box::new(Negation::new(operand)))
    }

    pub fn function(function: FunctionType) -> Expression {
        Expression::Function(Box::new(function))
    }

    pub fn exponentiation(base: Expression, exponent: Expression) -> Expression {
        Expression::Exponentiation(Box::new(Exponentiation::new(base, exponent)))
    }

    pub fn equality(left: Expression, right: Expression) -> Expression {
        Expression::Equality(Box::new(Equality::new(left, right)))
    }
}

impl Expression {
    pub fn equal(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Number(left), Expression::Number(right)) => left == right,
            (Expression::Variable(left), Expression::Variable(right)) => left == right,
            (Expression::Constant(left), Expression::Constant(right)) => left == right,
            (Expression::Addition(left), Expression::Addition(right)) => left.equal(right),
            (Expression::Multiplication(left), Expression::Multiplication(right)) => {
                left.equal(right)
            }
            (Expression::Exponentiation(left), Expression::Exponentiation(right)) => {
                left.equal(right)
            }
            (Expression::Fraction(left), Expression::Fraction(right)) => left.equal(right),
            (Expression::Equality(left), Expression::Equality(right)) => left.equal(right),
            (Expression::Negation(left), Expression::Negation(right)) => left.equal(right),
            (Expression::Function(left), Expression::Function(right)) => left.equal(right),
            _ => false,
        }
    }

    pub fn simplify(self) -> Expression {
        match self {
            Expression::Addition(add) => add.simplify(),
            Expression::Multiplication(mult) => mult.simplify(),
            Expression::Exponentiation(expo) => expo.simplify(),
            Expression::Fraction(frac) => frac.simplify(),
            Expression::Equality(eq) => eq.simplify(),
            Expression::Negation(neg) => neg.simplify(),
            Expression::Function(func) => func.simplify(),
            Expression::Error => panic!("There should be no error in the expression tree"),
            _ => self,
        }
    }

    pub fn get_order(&self) -> i64 {
        match self {
            Expression::Number(_) => 0,
            Expression::Variable(_) => 1,
            Expression::Constant(_) => 1,
            Expression::Addition(add) => add
                .sub_expr
                .iter()
                .map(|expr| expr.get_order())
                .max()
                .unwrap_or(0),
            Expression::Multiplication(mult) => mult
                .sub_expr
                .iter()
                .map(|expr| expr.get_order())
                .max()
                .unwrap_or(0),
            Expression::Exponentiation(expo) => {
                if let Expression::Number(num) = expo.get_exponent() {
                    num
                } else {
                    2
                }
            }
            Expression::Fraction(frac) => frac
                .get_numerator()
                .get_order()
                .max(frac.get_denominator().get_order()),
            Expression::Equality(eq) => eq
                .sub_expr
                .iter()
                .map(|expr| expr.get_order())
                .max()
                .unwrap_or(0),
            Expression::Negation(neg) => neg.sub_expr.get_order(),
            Expression::Function(_) => 3,
            Expression::Error => panic!("There should be no error in the expression tree"),
        }
    }

    pub fn contain_var(&self) -> Option<HashMap<char, usize>> {
        match self {
            Expression::Number(_) => None,
            Expression::Variable(var) => {
                let mut map = HashMap::new();
                map.insert(*var, 1);
                Some(map)
            }
            Expression::Constant(_) => None,
            Expression::Addition(add) => {
                let mut map: HashMap<char, usize> = HashMap::new();
                for expr in add.sub_expr.iter() {
                    if let Some(mut sub_map) = expr.contain_var() {
                        for (key, value) in sub_map.iter_mut() {
                            if let Some(occurence) = map.get_mut(key) {
                                *occurence += *value;
                            } else {
                                map.insert(*key, *value);
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
            Expression::Multiplication(mult) => {
                let mut map: HashMap<char, usize> = HashMap::new();
                for expr in mult.sub_expr.iter() {
                    if let Some(mut sub_map) = expr.contain_var() {
                        for (key, value) in sub_map.iter_mut() {
                            if let Some(occurence) = map.get_mut(key) {
                                *occurence += *value;
                            } else {
                                map.insert(*key, *value);
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
            Expression::Exponentiation(expo) => {
                let mut map: HashMap<char, usize> = HashMap::new();
                for expr in expo.sub_expr.iter() {
                    if let Some(mut sub_map) = expr.contain_var() {
                        for (key, value) in sub_map.iter_mut() {
                            if let Some(occurence) = map.get_mut(key) {
                                *occurence += *value;
                            } else {
                                map.insert(*key, *value);
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
            Expression::Fraction(frac) => {
                let mut map: HashMap<char, usize> = HashMap::new();
                for expr in frac.sub_expr.iter() {
                    if let Some(mut sub_map) = expr.contain_var() {
                        for (key, value) in sub_map.iter_mut() {
                            if let Some(occurence) = map.get_mut(key) {
                                *occurence += *value;
                            } else {
                                map.insert(*key, *value);
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
            Expression::Equality(eq) => {
                let mut map: HashMap<char, usize> = HashMap::new();
                for expr in eq.sub_expr.iter() {
                    if let Some(mut sub_map) = expr.contain_var() {
                        for (key, value) in sub_map.iter_mut() {
                            if let Some(occurence) = map.get_mut(key) {
                                *occurence += *value;
                            } else {
                                map.insert(*key, *value);
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
            Expression::Negation(neg) => {
                let mut map: HashMap<char, usize> = HashMap::new();
                if let Some(mut sub_map) = neg.sub_expr.contain_var() {
                    for (key, value) in sub_map.iter_mut() {
                        if let Some(occurence) = map.get_mut(key) {
                            *occurence += *value;
                        } else {
                            map.insert(*key, *value);
                        }
                    }
                }

                if map.is_empty() {
                    None
                } else {
                    Some(map)
                }
            }
            Expression::Function(func) => match *func.clone() {
                FunctionType::Predefined(_, args) | FunctionType::UserDefined(_, args) => {
                    let mut map: HashMap<char, usize> = HashMap::new();
                    for expr in args.iter() {
                        if let Some(mut sub_map) = expr.contain_var() {
                            for (key, value) in sub_map.iter_mut() {
                                if let Some(occurence) = map.get_mut(key) {
                                    *occurence += *value;
                                } else {
                                    map.insert(*key, *value);
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
            },
            Expression::Error => panic!("There should be no error in the expression tree"),
        }
    }

    pub fn print_tree(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        match self {
            Expression::Number(num) => println!("{}{num}", current_span),
            Expression::Addition(add) => {
                println!("{}Addition :", current_span);
                for expr in add.sub_expr.iter() {
                    print!("{}", current_span);
                    expr.print_tree(Some(&new_span));
                }
            }
            Expression::Multiplication(mult) => {
                println!("{}Multiplication :", current_span);
                for expr in mult.sub_expr.iter() {
                    print!("{}", current_span);
                    expr.print_tree(Some(&new_span));
                }
            }
            Expression::Exponentiation(expo) => {
                println!("{}Exponentiation :", current_span);
                print!("{}", current_span);
                expo.sub_expr[0].print_tree(Some(&new_span));
                print!("{}", current_span);
                expo.sub_expr[1].print_tree(Some(&new_span));
            }
            Expression::Fraction(frac) => {
                println!("{}Fraction :", current_span);
                print!("{}", current_span);
                frac.sub_expr[0].print_tree(Some(&new_span));
                print!("{}", current_span);
                frac.sub_expr[1].print_tree(Some(&new_span));
            }
            Expression::Equality(eq) => {
                println!("{}Equality :", current_span);
                print!("{}", current_span);
                eq.sub_expr[0].print_tree(Some(&new_span));
                print!("{}", current_span);
                eq.sub_expr[1].print_tree(Some(&new_span));
            }
            Expression::Negation(neg) => {
                println!("{}Negation :", current_span);
                print!("{}", current_span);
                neg.sub_expr.print_tree(Some(&new_span));
            }
            Expression::Error => println!("{}Error", current_span),
            Expression::Variable(var) => println!("{}{var}", current_span),
            Expression::Constant(cons) => println!("{}{cons}", current_span),
            Expression::Function(func) => {
                println!("{}Function : {}", current_span, func.name());
                for arg in func.args() {
                    print!("{}", current_span);
                    arg.print_tree(Some(&new_span));
                }
            }
        }
    }

    pub fn print_console(&self, depth: i8) {
        match self {
            Expression::Number(num) => println!("{num} {depth}"),
            Expression::Constant(cons) => println!("{cons} {depth}"),
            Expression::Variable(var) => println!("{var} {depth}"),
            Expression::Addition(add) => {
                print!("+ ");
                for expr in add.sub_expr.iter() {
                    expr.print_console(depth);
                }
            }
            Expression::Multiplication(mult) => {
                print!("* ");
                for expr in mult.sub_expr.iter() {
                    expr.print_console(depth);
                }
            }
            Expression::Exponentiation(expo) => {
                println!("base {depth}");
                expo.get_base().print_console(depth);
                println!("exponent {depth}");
                expo.get_exponent().print_console(depth + 1);
            }
            Expression::Fraction(frac) => {
                frac.get_numerator().print_console(depth + 1);
                println!("bar {depth}");
                frac.get_denominator().print_console(depth - 1);
            }
            Expression::Equality(eq) => {
                eq.get_left_side().print_console(depth);
                eq.get_right_side().print_console(depth);
            }
            Expression::Negation(neg) => {
                println!("- {depth}");
                neg.sub_expr.print_console(depth);
            }
            Expression::Function(func) => {
                println!("{func} {depth}");
            }
            Expression::Error => println!("there should be no error in the expression tree"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(num) => write!(f, "{num}"),
            Expression::Variable(var) => write!(f, "{var}"),
            Expression::Constant(cons) => write!(f, "{cons}"),
            Expression::Addition(add) => {
                let mut first = true;
                for expr in add.sub_expr.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
            Expression::Multiplication(mult) => {
                let mut first = true;
                for expr in mult.sub_expr.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, " * ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
            Expression::Exponentiation(expo) => write!(f, "({})^({})", expo.get_base(), expo.get_exponent()),
            Expression::Fraction(frac) => write!(f, "({}) / ({})", frac.get_numerator(), frac.get_denominator()),
            Expression::Equality(eq) => write!(f, "{} = {}", eq.get_left_side(), eq.get_right_side()),
            Expression::Negation(neg) => write!(f, "-({})", neg.sub_expr),
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Error => write!(f, "Error"),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ConstantKind {
    E,
    Pi,
}

impl ConstantKind {
    pub fn as_text(&self) -> &str {
        match self {
            ConstantKind::E => "e",
            ConstantKind::Pi => "pi",
        }
    }
}

impl Display for ConstantKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantKind::E => write!(f, "e"),
            ConstantKind::Pi => write!(f, "pi"),
        }
    }
}

#[cfg(test)]
mod test_simplify {

    use crate::ast::{
        function::{FunctionType, PredefinedFunction},
        ConstantKind, Expression,
    };

    #[test]
    fn test_simplify_addition() {
        // 1+2+3
        assert_eq!(
            Expression::addition(
                Expression::addition(Expression::Number(1), Expression::Number(2)),
                Expression::Number(3),
            )
            .simplify(),
            Expression::Number(6)
        );

        // 1 + x +3 +Y
        assert!(Expression::addition_from_vec(vec![
            Expression::Number(1),
            Expression::Variable('x'),
            Expression::Number(3),
            Expression::Variable('Y'),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::Number(4),
            Expression::Variable('x'),
            Expression::Variable('Y'),
        ])));

        // 2 +x +4 +X +x
        assert!(Expression::addition_from_vec(vec![
            Expression::Number(2),
            Expression::Variable('x'),
            Expression::Number(4),
            Expression::Variable('X'),
            Expression::Variable('x'),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::Number(6),
            Expression::Variable('X'),
            Expression::multiplication(Expression::Number(2), Expression::Variable('x'),),
        ])));

        // 2x + 2y + x
        assert!(Expression::addition_from_vec(vec![
            Expression::multiplication(Expression::Number(2), Expression::Variable('x'),),
            Expression::multiplication(Expression::Number(2), Expression::Variable('y'),),
            Expression::Variable('x'),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::multiplication(Expression::Number(3), Expression::Variable('x'),),
            Expression::multiplication(Expression::Number(2), Expression::Variable('y'),),
        ])));

        // x^2 + y^2 + 2xy + x^2
        assert!(Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
            Expression::exponentiation(Expression::Variable('y'), Expression::Number(2)),
            Expression::multiplication(
                Expression::multiplication(Expression::Number(2), Expression::Variable('x')),
                Expression::Variable('y'),
            ),
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
        ])
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::multiplication(
                Expression::Number(2),
                Expression::exponentiation(Expression::Variable('x'), Expression::Number(2))
            ),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::Variable('x'),
                Expression::Variable('y'),
            ]),
            Expression::exponentiation(Expression::Variable('y'), Expression::Number(2)),
        ])));

        // a/b + b/a
        assert!(Expression::addition(
            Expression::fraction(Expression::Variable('a'), Expression::Variable('b')),
            Expression::fraction(Expression::Variable('b'), Expression::Variable('a')),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition(
                Expression::exponentiation(Expression::Variable('a'), Expression::Number(2)),
                Expression::exponentiation(Expression::Variable('b'), Expression::Number(2)),
            ),
            Expression::multiplication(Expression::Variable('a'), Expression::Variable('b')),
        )));

        // e + e
        assert!(Expression::addition(
            Expression::Constant(ConstantKind::E),
            Expression::Constant(ConstantKind::E)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::Constant(ConstantKind::E)
        )));

        // sin(x) + sin(y)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('y')]
            )),
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('y')]
            )),
        )));

        // sin(x) + sin(x)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
        )));

        // 2sin(x) + sin(x)
        assert!(Expression::addition(
            Expression::multiplication(
                Expression::Number(2),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Variable('x')]
                ))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(3),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
        )));

        // ln(x) + ln(y)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('y')]
            )),
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('y')]
            )),
        )));

        // ln(x) + ln(x)
        assert!(Expression::addition(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x'),]
            ))
        )));

        // a(f) + a(f)
        assert!(Expression::addition(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
        )));

        // a(f) + a(g)
        assert!(Expression::addition(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('g')]
            )),
        )
        .simplify()
        .equal(&Expression::addition(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('g')]
            )),
        )));
    }

    #[test]
    fn test_simplify_multiplication() {
        // 1*2*3
        assert_eq!(
            Expression::multiplication(
                Expression::multiplication(Expression::Number(1), Expression::Number(2)),
                Expression::Number(3),
            )
            .simplify(),
            Expression::Number(6)
        );

        // 1 * x *3 *Y
        assert!(Expression::multiplication_from_vec(vec![
            Expression::Number(1),
            Expression::Variable('x'),
            Expression::Number(3),
            Expression::Variable('Y'),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::Number(3),
            Expression::Variable('x'),
            Expression::Variable('Y'),
        ])));

        // 2 *x *4 *X *x
        assert!(Expression::multiplication_from_vec(vec![
            Expression::Number(2),
            Expression::Variable('x'),
            Expression::Number(4),
            Expression::Variable('X'),
            Expression::Variable('x'),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::Number(8),
            Expression::Variable('X'),
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
        ])));

        // 2x * 2y * x
        assert!(Expression::multiplication_from_vec(vec![
            Expression::multiplication(Expression::Number(2), Expression::Variable('x'),),
            Expression::multiplication(Expression::Number(2), Expression::Variable('y'),),
            Expression::Variable('x'),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::Number(4),
            Expression::Variable('y'),
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
        ])));

        // x^2 * y^2 * 2xy * x^2
        assert!(Expression::multiplication_from_vec(vec![
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
            Expression::exponentiation(Expression::Variable('y'), Expression::Number(2)),
            Expression::multiplication(
                Expression::multiplication(Expression::Number(2), Expression::Variable('x')),
                Expression::Variable('y'),
            ),
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(2)),
        ])
        .simplify()
        .equal(&Expression::multiplication_from_vec(vec![
            Expression::exponentiation(Expression::Variable('x'), Expression::Number(5)),
            Expression::Number(2),
            Expression::exponentiation(Expression::Variable('y'), Expression::Number(3)),
        ])));

        // a/b * b/a
        assert!(Expression::multiplication(
            Expression::fraction(Expression::Variable('a'), Expression::Variable('b')),
            Expression::fraction(Expression::Variable('b'), Expression::Variable('a')),
        )
        .simplify()
        .equal(&Expression::Number(1)));

        // a/b * c/d
        assert!(Expression::multiplication(
            Expression::fraction(Expression::Variable('a'), Expression::Variable('b')),
            Expression::fraction(Expression::Variable('c'), Expression::Variable('d')),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::Variable('a'), Expression::Variable('c')),
            Expression::multiplication(Expression::Variable('b'), Expression::Variable('d')),
        )));

        // e * e
        assert!(Expression::multiplication(
            Expression::Constant(ConstantKind::E),
            Expression::Constant(ConstantKind::E)
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::Constant(ConstantKind::E),
            Expression::Number(2)
        )));

        // sin(x) * sin(y)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('y')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('y')]
            )),
        )));

        // sin(x) * sin(x)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
            Expression::Number(2)
        )));

        // 2sin(x) * sin(x)
        assert!(Expression::multiplication(
            Expression::multiplication(
                Expression::Number(2),
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Variable('x')]
                ))
            ),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::Variable('x')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::exponentiation(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::Variable('x')]
                )),
                Expression::Number(2)
            )
        )));

        // ln(x) * ln(y)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('y')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('y')]
            )),
        )));

        // ln(x) * ln(x)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            )),
            Expression::Number(2)
        )));

        // a(f) * a(f)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
            Expression::Number(2)
        )));

        // a(f) * a(g)
        assert!(Expression::multiplication(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('g')]
            )),
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('f')]
            )),
            Expression::function(FunctionType::UserDefined(
                'a'.to_string(),
                vec![Expression::Variable('g')]
            )),
        )));
    }

    #[test]
    fn test_simplify_exponentiation() {
        // 1^2
        assert_eq!(
            Expression::exponentiation(Expression::Number(1), Expression::Number(2)).simplify(),
            Expression::Number(1)
        );

        // 2^2
        assert_eq!(
            Expression::exponentiation(Expression::Number(2), Expression::Number(2)).simplify(),
            Expression::Number(4)
        );

        // 2^2^3
        assert_eq!(
            Expression::exponentiation(
                Expression::Number(2),
                Expression::exponentiation(Expression::Number(2), Expression::Number(3))
            )
            .simplify(),
            Expression::Number(256)
        );

        // (2^2)^2
        assert_eq!(
            Expression::exponentiation(
                Expression::exponentiation(Expression::Number(2), Expression::Number(2)),
                Expression::Number(2)
            )
            .simplify(),
            Expression::Number(16)
        );

        // x^x
        assert!(
            Expression::exponentiation(Expression::Variable('x'), Expression::Variable('x'))
                .simplify()
                .equal(&Expression::exponentiation(
                    Expression::Variable('x'),
                    Expression::Variable('x')
                ))
        );

        // x^log(x, y)
        assert!(Expression::exponentiation(
            Expression::Variable('x'),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::Variable('x'), Expression::Variable('y')]
            ))
        )
        .simplify()
        .equal(&Expression::Variable('y'),));

        // x^log(y, x)
        assert!(Expression::exponentiation(
            Expression::Variable('x'),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::Variable('y'), Expression::Variable('x')]
            ))
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::Variable('x'),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Log,
                vec![Expression::Variable('y'), Expression::Variable('x')]
            ))
        )));

        // e^ln(x)
        assert!(Expression::exponentiation(
            Expression::Constant(ConstantKind::E),
            Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Variable('x')]
            ))
        )
        .simplify()
        .equal(&Expression::Variable('x')));

        // (1/2)^2
        assert!(Expression::exponentiation(
            Expression::fraction(Expression::Number(1), Expression::Number(2)),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::Number(4)
        )));

        // (ab)^2
        assert!(Expression::exponentiation(
            Expression::multiplication(Expression::Variable('a'), Expression::Variable('b'),),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2),),
            Expression::exponentiation(Expression::Variable('b'), Expression::Number(2),),
        )));

        // (a+b)^2
        assert!(Expression::exponentiation(
            Expression::addition(Expression::Variable('a'), Expression::Variable('b'),),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::Variable('a'),
                Expression::Variable('b'),
            ]),
            Expression::exponentiation(Expression::Variable('b'), Expression::Number(2),),
        ])));

        // (a-b)^2
        assert!(Expression::exponentiation(
            Expression::addition(
                Expression::Variable('a'),
                Expression::negation(Expression::Variable('b'),),
            ),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::Variable('a'),
                Expression::negation(Expression::Variable('b')),
            ]),
            Expression::exponentiation(
                Expression::negation(Expression::Variable('b')),
                Expression::Number(2),
            ),
        ])));

        // (a+b)^3
        assert!(Expression::exponentiation(
            Expression::addition(Expression::Variable('a'), Expression::Variable('b'),),
            Expression::Number(3)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(3),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(3),
                Expression::exponentiation(Expression::Variable('a'), Expression::Number(2),),
                Expression::Variable('b'),
            ]),
            Expression::multiplication_from_vec(vec![
                Expression::Number(3),
                Expression::exponentiation(Expression::Variable('b'), Expression::Number(2),),
                Expression::Variable('a'),
            ]),
            Expression::exponentiation(Expression::Variable('b'), Expression::Number(3),),
        ])));

        //(a+b+c)^2
        assert!(Expression::exponentiation(
            Expression::addition_from_vec(vec![
                Expression::Variable('a'),
                Expression::Variable('b'),
                Expression::Variable('c'),
            ]),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::addition_from_vec(vec![
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::Variable('a'),
                Expression::Variable('b'),
            ]),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::Variable('a'),
                Expression::Variable('c'),
            ]),
            Expression::exponentiation(Expression::Variable('b'), Expression::Number(2),),
            Expression::multiplication_from_vec(vec![
                Expression::Number(2),
                Expression::Variable('b'),
                Expression::Variable('c'),
            ]),
            Expression::exponentiation(Expression::Variable('c'), Expression::Number(2),),
        ])));
    }

    #[test]
    fn test_simplify_fraction() {
        // 1/2
        assert!(
            Expression::fraction(Expression::Number(1), Expression::Number(2))
                .simplify()
                .equal(&Expression::fraction(
                    Expression::Number(1),
                    Expression::Number(2)
                ))
        );

        // 2/2
        assert!(
            Expression::fraction(Expression::Number(2), Expression::Number(2))
                .simplify()
                .equal(&Expression::Number(1))
        );

        // 2/4
        assert!(
            Expression::fraction(Expression::Number(2), Expression::Number(4))
                .simplify()
                .equal(&Expression::fraction(
                    Expression::Number(1),
                    Expression::Number(2)
                ))
        );

        // 2/4/2
        assert!(Expression::fraction(
            Expression::fraction(Expression::Number(2), Expression::Number(4)),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::Number(4)
        )));

        // -a/b
        assert!(Expression::fraction(
            Expression::negation(Expression::Variable('a')),
            Expression::Variable('b')
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::negation(Expression::Variable('a')),
            Expression::Variable('b')
        )));

        // a/-b
        assert!(Expression::fraction(
            Expression::Variable('a'),
            Expression::negation(Expression::Variable('b'))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::negation(Expression::Variable('a')),
            Expression::Variable('b')
        )));

        // -a/-b
        assert!(Expression::fraction(
            Expression::negation(Expression::Variable('a')),
            Expression::negation(Expression::Variable('b'))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Variable('a'),
            Expression::Variable('b')
        )));

        //a/a^n
        assert!(Expression::fraction(
            Expression::Variable('a'),
            Expression::exponentiation(Expression::Variable('a'), Expression::Variable('n'))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::exponentiation(
                Expression::Variable('a'),
                Expression::addition(Expression::Variable('n'), Expression::Number(-1))
            )
        )));

        // a^2/a
        assert!(Expression::fraction(
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2)),
            Expression::Variable('a')
        )
        .simplify()
        .equal(&Expression::Variable('a')));

        // a^2/a^3
        assert!(Expression::fraction(
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(2)),
            Expression::exponentiation(Expression::Variable('a'), Expression::Number(3))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::Variable('a')
        )));

        // 2^n/2
        assert!(Expression::fraction(
            Expression::exponentiation(Expression::Number(2), Expression::Variable('n')),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::exponentiation(
            Expression::Number(2),
            Expression::addition(Expression::Variable('n'), Expression::Number(-1))
        )));

        // 2 / 2^n
        assert!(Expression::fraction(
            Expression::Number(2),
            Expression::exponentiation(Expression::Number(2), Expression::Variable('n'))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Number(1),
            Expression::exponentiation(
                Expression::Number(2),
                Expression::addition(Expression::Variable('n'), Expression::Number(-1))
            )
        )));

        // 4a / 2
        assert!(Expression::fraction(
            Expression::multiplication(Expression::Number(4), Expression::Variable('a')),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::multiplication(
            Expression::Number(2),
            Expression::Variable('a')
        )));

        // 4a / 2b
        assert!(Expression::fraction(
            Expression::multiplication(Expression::Number(4), Expression::Variable('a')),
            Expression::multiplication(Expression::Number(2), Expression::Variable('b'))
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::Number(2), Expression::Variable('a')),
            Expression::Variable('b')
        )));

        // (12a + 6)/(24a^2)
        assert!(Expression::fraction(
            Expression::addition(
                Expression::multiplication(Expression::Number(12), Expression::Variable('a')),
                Expression::Number(6)
            ),
            Expression::multiplication(
                Expression::Number(24),
                Expression::exponentiation(Expression::Variable('a'), Expression::Number(2))
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition(
                Expression::Number(1),
                Expression::multiplication(Expression::Number(2), Expression::Variable('a'))
            ),
            Expression::multiplication(
                Expression::Number(4),
                Expression::exponentiation(Expression::Variable('a'), Expression::Number(2))
            )
        )));

        // (18x^2y + 3xy + 9xy^2) / (6x^2y)
        assert!(Expression::fraction(
            Expression::addition_from_vec(vec![
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(18), Expression::Variable('y')),
                    Expression::exponentiation(Expression::Variable('x'), Expression::Number(2))
                ),
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(3), Expression::Variable('x')),
                    Expression::Variable('y')
                ),
                Expression::multiplication(
                    Expression::multiplication(Expression::Number(9), Expression::Variable('x')),
                    Expression::exponentiation(Expression::Variable('y'), Expression::Number(2))
                ),
            ]),
            Expression::multiplication(
                Expression::multiplication(Expression::Number(6), Expression::Variable('y')),
                Expression::exponentiation(Expression::Variable('x'), Expression::Number(2))
            )
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::addition_from_vec(vec![
                Expression::multiplication(Expression::Number(3), Expression::Variable('y')),
                Expression::Number(1),
                Expression::multiplication(Expression::Number(6), Expression::Variable('x')),
            ]),
            Expression::multiplication(Expression::Number(2), Expression::Variable('x'))
        )));

        // (a/b)/(c/d)
        assert!(Expression::fraction(
            Expression::fraction(Expression::Variable('a'), Expression::Variable('b'),),
            Expression::fraction(Expression::Variable('c'), Expression::Variable('d'),),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::Variable('a'), Expression::Variable('d'),),
            Expression::multiplication(Expression::Variable('b'), Expression::Variable('c'),),
        )));

        Expression::fraction(
            Expression::fraction(Expression::Variable('a'), Expression::Variable('b')),
            Expression::Variable('c'),
        )
        .simplify()
        .print_tree(None);

        // (a/b)/c
        assert!(Expression::fraction(
            Expression::fraction(Expression::Variable('a'), Expression::Variable('b'),),
            Expression::Variable('c'),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::Variable('a'),
            Expression::multiplication(Expression::Variable('b'), Expression::Variable('c'),),
        )));

        // a/(b/c)
        assert!(Expression::fraction(
            Expression::Variable('a'),
            Expression::fraction(Expression::Variable('b'), Expression::Variable('c'),),
        )
        .simplify()
        .equal(&Expression::fraction(
            Expression::multiplication(Expression::Variable('a'), Expression::Variable('c'),),
            Expression::Variable('b'),
        )));
    }

    #[test]
    fn test_negation() {
        // --2
        assert!(
            Expression::negation(Expression::negation(Expression::Number(2)))
                .simplify()
                .equal(&Expression::Number(2))
        );

        // -2
        assert!(Expression::negation(Expression::Number(2))
            .simplify()
            .equal(&Expression::Number(-2)));

        // -2 + 3
        assert!(Expression::addition(
            Expression::negation(Expression::Number(2)),
            Expression::Number(3)
        )
        .simplify()
        .equal(&Expression::Number(1)));

        // -2 - 3
        assert!(Expression::addition(
            Expression::negation(Expression::Number(2)),
            Expression::negation(Expression::Number(3))
        )
        .simplify()
        .equal(&Expression::Number(-5)));

        // -2 * 2
        assert!(Expression::multiplication(
            Expression::negation(Expression::Number(2)),
            Expression::Number(2)
        )
        .simplify()
        .equal(&Expression::Number(-4)));

        // -2 * -2
        assert!(Expression::multiplication(
            Expression::negation(Expression::Number(2)),
            Expression::negation(Expression::Number(2))
        )
        .simplify()
        .equal(&Expression::Number(4)));
    }
}
