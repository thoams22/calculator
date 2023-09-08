use std::fmt::{Display, Formatter};

use super::Expression;

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperatorKind {
    Negation,
}

impl UnaryOperatorKind {
    pub fn precedence(&mut self) -> usize {
        match self {
            UnaryOperatorKind::Negation => 6,
        }
    }
}

impl Display for UnaryOperatorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperatorKind::Negation => write!(f, "-"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnaryOperator {
    pub kind: UnaryOperatorKind,
    pub operand: Expression,
}

impl UnaryOperator {
    pub fn new(kind: UnaryOperatorKind, operand: Expression) -> Self {
        Self { kind, operand }
    }

    pub fn simplify(self) -> Expression {
        todo!()
    }
}