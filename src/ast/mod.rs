pub mod binaryoperator;
pub mod unaryoperator;
pub mod function;

use std::fmt::{Display, Formatter};

use self::unaryoperator::{UnaryOperator, UnaryOperatorKind};
use self::binaryoperator::{BinaryOperator, BinaryOperatorKind};
use self::function::Function;


#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Number(i64),
    Variable(char),
    Constant(ConstantKind),
    BinaryOperator(Box<BinaryOperator>),
    UnaryOperator(Box<UnaryOperator>),
    Parenthesis(Box<Parenthesis>),
    Function(Box<Function>),
    Error,
}

// Helper constructor
impl Expression {
    pub fn binary_operator(
        kind: BinaryOperatorKind,
        left: Expression,
        right: Expression,
    ) -> Expression {
        Expression::BinaryOperator(Box::new(BinaryOperator::new(kind, left, right)))
    }

    pub fn unary_operator(kind: UnaryOperatorKind, operand: Expression) -> Expression {
        Expression::UnaryOperator(Box::new(UnaryOperator::new(kind, operand)))
    }

    pub fn parenthesis(expr: Expression) -> Expression {
        Expression::Parenthesis(Box::new(Parenthesis::new(expr)))
    }

    pub fn function(function: Function) -> Expression {
        Expression::Function(Box::new(function))
    }
}

impl Expression {
    pub fn print(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        match self {
            Expression::Number(_) => println!("{}{}", current_span, self),
            Expression::BinaryOperator(bin) => {
                println!("{}BinaryOperator : {}", current_span, bin.kind);

                print!("{}", current_span);
                bin.left.print(Some(&new_span));

                print!("{}", current_span);
                bin.right.print(Some(&new_span));
            }
            Expression::UnaryOperator(un) => {
                println!("{}UnaryOperator : {}", current_span, un.kind);

                print!("{}", current_span);
                un.operand.print(Some(&new_span));
            }
            Expression::Parenthesis(parent) => {
                println!("{}Parenthesis :", current_span);
                print!("{}", current_span);
                parent.expr.print(Some(&current_span));
            }
            Expression::Error => println!("{}{}", current_span, self),
            Expression::Variable(_) => println!("{}{}", current_span, self),
            Expression::Constant(_) => println!("{}{}", current_span, self),
            Expression::Function(func) => {
                println!("{}Function : {}", current_span, func.function_type.name());
                for arg in func.function_type.args() {
                    print!("{}", current_span);
                    arg.print(Some(&new_span));
                }
            }
        }
    }

    pub fn simplify(self) -> Expression {
        match self {
            Expression::Number(_) => self,
            Expression::Variable(_) => self,
            Expression::Constant(_) => self,
            Expression::BinaryOperator(binaryoperator) => binaryoperator.simplify(),
            Expression::UnaryOperator(unaryoperator) => unaryoperator.simplify(),
            Expression::Parenthesis(parenthesis) => todo!(),
            Expression::Function(function) => function.simplify(),
            Expression::Error => panic!("There should not be an error here!"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(num) => write!(f, "Number : {}", num),
            Expression::BinaryOperator(bin) => {
                writeln!(f, "{}", bin.left)?;
                writeln!(f, "BinaryOperator : {}", bin.kind)?;
                writeln!(f, "{}\n", bin.right)
            }
            Expression::UnaryOperator(un) => {
                writeln!(f, "UnaryOprator : {}", un.kind)?;
                writeln!(f, "{}", un.operand)
            }
            Expression::Error => writeln!(f, "Error"),
            Expression::Parenthesis(parent) => {
                writeln!(f, "Parenthesis : {}", parent.expr)
            }
            Expression::Variable(var) => write!(f, "Variable : {}", var),
            Expression::Constant(con) => write!(f, "Constant : {}", con),
            Expression::Function(func) => write!(f, "Function : {}", func.function_type),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Parenthesis {
    expr: Expression,
}

impl Parenthesis {
    pub fn new(expr: Expression) -> Self {
        Self { expr }
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
