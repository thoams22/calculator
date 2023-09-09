use crate::{lexer::{TokenKind, Token}, ast::function::PredefinedFunction};

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
        self.messages.push(
            message
        )
    }

    pub fn report_unknown_char(&mut self, unknown: char) {
        self.error(
            format!("Unknown char found '{}'", unknown)
        )
    }

    pub fn report_unexpected_token(&mut self, token: &TokenKind, expected: &TokenKind) {
        self.error(
            format!("Expected <{}>, found <{}>", expected, token)
        )
    }

    pub fn report_unexpected_primary(&mut self, token: &TokenKind) {
        self.error(
            format!("Expected <Primary>, found <{}>", token)
        )
    } 

    pub fn report_unexpected_number(&mut self, token: &Token) {
        self.error(
            format!("Expected <Number: i64>, found <{}>", token.text)
        )
    }


    pub fn report_invalid_argument_count(&mut self, function: &PredefinedFunction, actual: usize) {
        self.error(format!("Function '{}' expects {} argument(s), but was given {}", function, function.args_count(), actual));
    }
}