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

    fn error(&mut self, message: String) {
        self.messages.push(message)
    }

    pub fn report_unknown_char(&mut self, unknown: char) {
        self.error(format!("Unknown char found '{}'", unknown))
    }

    pub fn report_unexpected_token(&mut self, token: &TokenKind, expected: &TokenKind) {
        self.error(format!("Expected <{}>, found <{}>", expected, token))
    }

    pub fn report_expected_variable_expression(&mut self, expression: &Expression) {
        self.error(format!("Expected <Variable>, found <{}>", expression))
    }

    pub fn report_unexpected_primary(&mut self, token: &TokenKind) {
        self.error(format!("Expected <Primary>, found <{}>", token))
    }

    pub fn report_unexpected_number(&mut self, token: &Token) {
        self.error(format!("Expected <Number: i64>, found <{}>", token.text))
    }

    pub fn report_unexpected_var_for_solve(&mut self, var: Expression) {
        self.error(format!("Ask to solve for '{var}' but '{var}' not in the expression"))
    }

    pub fn report_invalid_argument_count(&mut self, function: &PredefinedFunction, actual: usize) {
        self.error(format!(
            "Function '{}' expects {} argument(s), but was given {}",
            function,
            function.args_count(),
            actual
        ));
    }
}
