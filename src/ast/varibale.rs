use std::{fmt::Display, fmt::Formatter};

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Variable {
    pub sub_expr: String
}

impl Variable {
    pub fn new(sub_expr: String) -> Self {
        Self {
            sub_expr,
        }
    }
}

impl Variable {
    pub fn equal(&self, other: &Variable) -> bool {
        self.sub_expr == other.sub_expr
    }
}

impl Display for Variable {
    fn fmt(&self, f:&mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.sub_expr)
    }
}