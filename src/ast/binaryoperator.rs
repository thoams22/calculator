use std::fmt::{Display, Formatter};

use crate::ast::Node;

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Exponentiation,
    Equality,
}

impl BinaryOperatorKind {
    pub fn precedence(&mut self) -> usize {
        match self {
            BinaryOperatorKind::Equality => 5,
            BinaryOperatorKind::Exponentiation => 4,
            BinaryOperatorKind::Multiplication | BinaryOperatorKind::Division => 3,
            BinaryOperatorKind::Addition | BinaryOperatorKind::Subtraction => 2,
        }
}}

impl Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperatorKind::Addition => write!(f, "+"),
            BinaryOperatorKind::Subtraction => write!(f, "-"),
            BinaryOperatorKind::Multiplication => write!(f, "*"),
            BinaryOperatorKind::Division => write!(f, "/"),
            BinaryOperatorKind::Exponentiation => write!(f, "^"),
            BinaryOperatorKind::Equality => write!(f, "="),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub left: Node,
    pub right: Node,
}

impl BinaryOperator {
    pub fn new(kind: BinaryOperatorKind, left: Node, right: Node) -> Self {
        Self { kind,left, right }
    }
}