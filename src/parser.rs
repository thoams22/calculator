use crate::{
    expression::Expression,
    lexer::{Lexer, Token, TokenKind},
};

pub struct Parser {
    position: usize,
    tokens: Vec<Token>,
    diagnostics: Vec<String>,
}

impl Parser {
    pub fn new(text: &str) -> Self {
        let mut lexer = Lexer::new(text.chars().collect());

        let mut token: Token = lexer.next_token();
        let mut tokens: Vec<Token> = Vec::new();

        while token.kind != TokenKind::End && token.kind != TokenKind::Error {
            tokens.push(token);
            token = lexer.next_token();
        }
        if token.kind == TokenKind::End {
            tokens.push(token);
        }

        Self {
            position: 0,
            tokens,
            diagnostics: lexer.get_diagnostics(),
        }
    }

    pub fn get_diagnostics(&mut self) -> Vec<String> {
        self.diagnostics.clone()
    }

    fn peek_token(&mut self, offset: usize) -> Token {
        let index = self.position + offset;
        self.tokens
            .get(index)
            .unwrap_or_else(|| self.tokens.get(self.tokens.len() - 1).unwrap())
            .clone()
    }

    fn next_token(&mut self) -> Token {
        let current = self.current_token().clone();
        self.position += 1;
        current
    }

    fn current_token(&mut self) -> &Token {
        self.tokens.get(self.position).unwrap()
    }

    fn eat_while(&mut self, predicate: impl Fn(TokenKind) -> bool) {
        while predicate(self.current_token().kind) {
            self.next_token();
        }
    }

    fn equal_or_create(&mut self, kind: TokenKind) -> Token {
        if self.current_token().kind == kind {
            self.next_token()
        } else {
            let current = self.current_token().kind;
            self.diagnostics.push(format!(
                "ERROR PARSER: Unexpected token '{:?}' expected '{:?}'",
                current,
                kind
            ));
            Token {
                kind: kind,
                text: String::new(),
            }
        }
    }

    fn parse(&mut self) -> Expression {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Expression {
        let mut first: Expression = self.parse_multiplicative();

        while TokenKind::Minus == self.current_token().kind {
            let start = self.position;
            self.eat_while(|tk| tk == TokenKind::Minus);

            let operator = if (self.position - start) % 2 == 0 {
                Token {
                    kind: TokenKind::Plus,
                    text: "".to_string(),
                }
            } else {
                Token {
                    kind: TokenKind::Minus,
                    text: "".to_string(),
                }
            };

            let second = self.parse_multiplicative();

            if operator.kind == TokenKind::Plus {
                first = Expression::addition(first, second);
            } else {
                first = Expression::subtraction(first, second);
            }
        }

        while TokenKind::Plus == self.current_token().kind {
            self.next_token();

            let second = self.parse_multiplicative();

            first = Expression::addition(first, second);
        }

        first
    }

    fn parse_multiplicative(&mut self) -> Expression {
        let mut first = self.parse_primary();

        while TokenKind::Star == self.current_token().kind {
            self.next_token();

            let second = self.parse_primary();

            first = Expression::multiplication(first, second);
        }

        while TokenKind::Slash == self.current_token().kind {
            self.next_token();

            let second = self.parse_primary();

            first = Expression::fraction(first, second);
        }

        first
    }

    fn parse_primary(&mut self) -> Expression {
        
        if self.current_token().kind == TokenKind::LeftParenthesis {

            self.next_token();
            self.next_token();
            let expr = self.parse_additive();
            self.equal_or_create(TokenKind::RightParenthesis);
            
            return expr;
        } 

        if self.current_token().kind == TokenKind::Minus {
            let start = self.position;
            self.eat_while(|tk| tk == TokenKind::Minus);

            return if (self.position - start) % 2 == 0 {
                self.parse_primary()
            } else {
                Expression::multiplication(Expression::number(-1.0), self.parse_primary())
            };
        }

        let number: f64 = match self.equal_or_create(TokenKind::Number).text.parse::<f64>() {
            Ok(number) => number,
            Err(_) => {
                0.0f64
            }
        };
        Expression::Number(number)
    }
}

#[cfg(test)]
mod tests_parser {
    use crate::expression::Expression;

    use super::Parser;

    fn verification_parser(expression: &str) -> Expression {
        let mut parser = Parser::new(expression);
        let result = parser.parse();

        if !parser.get_diagnostics().is_empty() {
            println!("\n***ERROR***\n");

            for diag in parser.get_diagnostics() {
                println!("{}", diag);
            }
            println!("\n***END OF ERROR***");
            println!();
        }

        result
    }

    // #[ignore]
    #[test]
    fn basic_op() {
        assert_eq!(
            verification_parser("- -1 + 3"),
            Expression::addition(Expression::Number(1.0), Expression::Number(3.0)),
            "\nbasic_op 1"
        );
    }
}
