use crate::{
    ast::{function::PredefinedFunction, Expression},
    lexer::{Token, TokenKind},
};

#[derive(PartialEq, Debug, Clone)]
pub struct Diagnostics {
    messages: Vec<String>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            messages: Vec::new(),
        }
    }

    pub fn get_messages(&mut self) -> Vec<String> {
        self.messages.clone()
    }

    pub fn error(&mut self, message: String) {
        self.messages.push(message)
    }

    pub fn report_unexpected_var_for_solve(&mut self, var: Expression) {
        self.error(format!("Ask to solve for '{var}' but '{var}' not in the expression"))
    }

    pub fn report_unexpected_degree_for_derivate(&mut self, degree: Expression) {
        self.error(format!("Unexpected degree <{}>, expected no degree", degree))
    }
}
