pub mod binaryoperator;
pub(crate) mod unaryoperator;
pub(crate) mod function;

use std::fmt::{Display, Formatter};

use self::unaryoperator::{UnaryOperator, UnaryOperatorKind};
use self::binaryoperator::{BinaryOperator, BinaryOperatorKind};
use self::function::Function;


#[derive(PartialEq, Debug, Clone)]
pub enum Node {
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
impl Node {
    pub fn binary_operator(
        kind: BinaryOperatorKind,
        left: Node,
        right: Node,
    ) -> Node {
        Node::BinaryOperator(Box::new(BinaryOperator::new(kind, left, right)))
    }

    pub fn unary_operator(kind: UnaryOperatorKind, operand: Node) -> Node {
        Node::UnaryOperator(Box::new(UnaryOperator::new(kind, operand)))
    }

    pub fn parenthesis(expr: Node) -> Node {
        Node::Parenthesis(Box::new(Parenthesis::new(expr)))
    }

    pub fn function(function: Function) -> Node {
        Node::Function(Box::new(function))
    }
}

impl Node {
    pub fn print(&self, span: Option<&str>) {
        let current_span = span.unwrap_or("");
        let new_span = current_span.to_string() + "   ";
        match self {
            Node::Number(_) => println!("{}{}", current_span, self),
            Node::BinaryOperator(bin) => {
                println!("{}BinaryOperator : {}", current_span, bin.kind);

                    print!("{}", current_span);
                    bin.left.print(Some(&new_span));
                    
                    print!("{}", current_span);
                    bin.right.print(Some(&new_span));
                
            }
            Node::UnaryOperator(un) => {
                println!("{}UnaryOperator : {}", current_span, un.kind);

                print!("{}", current_span);
                un.operand.print(Some(&new_span));
            }
            Node::Parenthesis(parent) => {
                println!("{}Parenthesis :", current_span);
                print!("{}", current_span);
                parent.expr.print(Some(current_span));
            }
            Node::Error => println!("{}{}", current_span, self),
            Node::Variable(_) => println!("{}{}", current_span, self),
            Node::Constant(_) => println!("{}{}", current_span, self),
            Node::Function(func) => {
                println!("{}Function : {}", current_span, func.function_type.name());
                for arg in func.function_type.args() {
                    print!("{}", current_span);
                    arg.print(Some(&new_span));
                }
            }
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Number(num) => write!(f, "Number : {}", num),
            Node::BinaryOperator(bin) => {
                writeln!(f, "BinaryOperator : {}", bin.kind)?;
                writeln!(f, "{}", bin.left)?;                
                writeln!(f, "{}", bin.right)
            }
            Node::UnaryOperator(un) => {
                writeln!(f, "UnaryOprator : {}", un.kind)?;
                writeln!(f, "{}", un.operand)
            }
            Node::Error => writeln!(f, "Error"),
            Node::Parenthesis(parent) => {
                writeln!(f, "Parenthesis : {}", parent.expr)
            }
            Node::Variable(var) => write!(f, "Variable : {}", var),
            Node::Constant(con) => write!(f, "Constant : {}", con),
            Node::Function(func) => write!(f, "Function : {}", func.function_type),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Parenthesis {
    expr: Node,
}

impl Parenthesis {
    pub fn new(expr: Node) -> Self {
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
