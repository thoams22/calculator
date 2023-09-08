use std::fmt::{Display, Formatter};

use crate::ast::Expression;

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
    pub left: Expression,
    pub right: Expression,
}

impl BinaryOperator {
    pub fn new(kind: BinaryOperatorKind, left: Expression, right: Expression) -> Self {
        Self { kind, left, right }
    }

    pub fn simplify(mut self) -> Expression {
        match self.kind {
            BinaryOperatorKind::Addition => todo!(),//self.simplify_addition(),
            BinaryOperatorKind::Subtraction => todo!(),
            BinaryOperatorKind::Multiplication => todo!(),
            BinaryOperatorKind::Division => todo!(),
            BinaryOperatorKind::Exponentiation => todo!(),
            BinaryOperatorKind::Equality => todo!(),
        }
    }

    // fn simplify_addition(&mut self) -> Expression {
        
    // }
}