use crate::ast::Expression;

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub struct Variable {
    pub sub_expr: String,
    pub simplified: bool,
}

impl Variable {
    pub fn new(sub_expr: String) -> Self {
        Self {
            sub_expr,
            simplified: false,
        }
    }
}

impl Variable {
    pub fn equal(&self, other: &Variable) -> bool {
        self.sub_expr == other.sub_expr
    }
}

impl Variable {

}