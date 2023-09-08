use crate::{
    ast::{
        binaryoperator::BinaryOperatorKind, function::Function, function::PredefinedFunction,
        unaryoperator::UnaryOperatorKind, ConstantKind, Expression,
    },
    diagnostic::Diagnostics,
    lexer::{Lexer, Span, Token, TokenKind},
};

#[derive(PartialEq, Debug, Clone)]
pub struct Parser {
    position: usize,
    tokens: Vec<Token>,
    diagnostics: Diagnostics,
}

impl Parser {
    pub fn new(text: &str) -> Self {
        let mut lexer = Lexer::new(text.chars().collect());

        let mut token: Token = lexer.next_token();
        let mut tokens: Vec<Token> = Vec::new();

        while token.kind != TokenKind::End && token.kind != TokenKind::Error {
            if token.kind != TokenKind::WhiteSpace {
                tokens.push(token);
            }
            token = lexer.next_token();
        }
        if token.kind == TokenKind::End {
            tokens.push(token);
        }

        Self {
            position: 0,
            tokens,
            diagnostics: lexer.get_diagnostic(),
        }
    }

    fn insert_tokens(&mut self, elem: Token) {
        self.tokens.insert(self.position, elem);
    }

    pub fn get_tokens(&mut self) -> Vec<Token> {
        self.tokens.clone()
    }

    pub fn get_diagnostic(&mut self) -> Diagnostics {
        self.diagnostics.clone()
    }

    pub fn get_diagnostic_message(&mut self) -> Vec<String> {
        self.diagnostics.get_messages()
    }

    fn peek_token(&mut self, offset: isize) -> Token {
        let index: usize = (self.position as isize + offset).try_into().unwrap();
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

    fn current_token(&mut self) -> Token {
        self.peek_token(0)
    }

    fn eat_while(&mut self, predicate: impl Fn(TokenKind) -> bool) {
        while predicate(self.current_token().kind) {
            self.next_token();
        }
    }

    fn eat_if(&mut self, predicate: impl Fn(TokenKind) -> bool) -> bool {
        if predicate(self.current_token().kind) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn equal_or_create_next(&mut self, kind: TokenKind) -> Token {
        if self.current_token().kind == kind {
            self.next_token()
        } else {
            let current = self.current_token().kind;
            self.diagnostics.report_unexpected_token(&current, &kind);
            Token {
                kind: kind,
                text: String::new(),
                span: self.current_token().span,
            }
        }
    }

    fn equal_or_create(&mut self, kind: TokenKind) -> Token {
        if self.current_token().kind == kind {
            self.current_token()
        } else {
            let current = self.current_token().kind;
            self.diagnostics.report_unexpected_token(&current, &kind);
            Token {
                kind: kind,
                text: String::new(),
                span: self.current_token().span,
            }
        }
    }

    pub fn parse(&mut self) -> Vec<Expression> {
        let mut ast = Vec::new();
        while let Some(expr) = self.parse_type() {
            ast.push(expr);
            // println!("{}", self.current_token());
        }
        ast
    }

    fn parse_type(&mut self) -> Option<Expression> {
        if self.current_token().kind == TokenKind::End {
            return None;
        }
        Some(self.parse_expression())
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, precedence: usize) -> Expression {
        let mut left = self.parse_unary_expression();

        while let Some(mut operator) = self.parse_binary_operator() {
            let op_precedence = operator.precedence();
            if op_precedence <= precedence && operator != BinaryOperatorKind::Exponentiation {
                break;
            }
            self.next_token();

            let right = self.parse_binary_expression(op_precedence);
            left = Expression::binary_operator(operator, left, right);
        }
        left
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperatorKind> {
        match self.current_token().kind {
            TokenKind::Minus => Some(BinaryOperatorKind::Subtraction),
            TokenKind::Plus => Some(BinaryOperatorKind::Addition),
            TokenKind::Slash => Some(BinaryOperatorKind::Division),
            TokenKind::Star => Some(BinaryOperatorKind::Multiplication),
            TokenKind::Equal => Some(BinaryOperatorKind::Equality),
            TokenKind::Hat => Some(BinaryOperatorKind::Exponentiation),
            _ => None,
        }
    }

    fn parse_unary_expression(&mut self) -> Expression {
        while let Some(operator) = self.parse_unary_operator() {
            self.next_token();
            let operand = self.parse_unary_expression();
            return Expression::unary_operator(operator, operand);
        }
        self.parse_primary()
    }

    fn parse_unary_operator(&mut self) -> Option<UnaryOperatorKind> {
        match self.current_token().kind {
            TokenKind::Minus => Some(UnaryOperatorKind::Negation),
            _ => None,
        }
    }

    fn parse_primary(&mut self) -> Expression {
        let current = self.current_token();
        let result = match current.kind {
            TokenKind::Number => {
                let expression = self.parse_number_expression();

                // handle implicit multiplication
                if self.current_token().kind == TokenKind::LeftParenthesis
                    || self.current_token().kind == TokenKind::Literal
                {
                    self.insert_tokens(Token::new(
                        TokenKind::Star,
                        Span::new(0, 0),
                        "*".to_string(),
                    ))
                }
                expression
            }
            TokenKind::Literal => {
                let expression = self.parse_variable(current.text);
                self.next_token();
                println!("{}", self.current_token());

                // handle implicit multiplication
                if self.current_token().kind == TokenKind::Number
                    || self.current_token().kind == TokenKind::Literal
                {
                    self.insert_tokens(Token::new(
                        TokenKind::Star,
                        Span::new(0, 0),
                        "*".to_string(),
                    ))
                }
                expression
            }
            TokenKind::LeftParenthesis => {
                self.next_token();
                let expression = self.parse_expression();
                self.equal_or_create_next(TokenKind::RightParenthesis);

                // handle implicit multiplication
                if self.current_token().kind == TokenKind::LeftParenthesis
                    || self.current_token().kind == TokenKind::Number
                    || self.current_token().kind == TokenKind::Literal
                {
                    self.insert_tokens(Token::new(
                        TokenKind::Star,
                        Span::new(0, 0),
                        "*".to_string(),
                    ))
                }
                Expression::parenthesis(expression)
            }
            _ => {
                self.diagnostics.report_unexpected_primary(&current.kind);
                self.next_token();
                Expression::Error
            }
        };
        result
    }

    fn parse_number_expression(&mut self) -> Expression {
        let current = self.next_token();
        match current.text.parse::<i64>() {
            Ok(number) => Expression::Number(number),
            Err(_) => match current.text.parse::<f64>() {
                Ok(_) => {
                    let (num, denom) = Self::string_to_fraction(current.text);
                    Expression::binary_operator(
                        BinaryOperatorKind::Division,
                        Expression::Number(num),
                        Expression::Number(denom),
                    )
                }
                Err(_) => {
                    self.diagnostics.report_unexpected_number(&current);
                    Expression::Error
                }
            },
        }
    }

    fn parse_variable(&mut self, mut text: String) -> Expression {
        let mut components = vec![];

        if self.peek_token(1).kind == TokenKind::LeftParenthesis {
            match self.parse_function(&text) {
                (None, txt) => {
                    text = txt;
                }
                (Some(fucntion), txt) => {
                    text = txt;
                    components.push(fucntion);
                }
            }
        }

        let (text, constants) = self.parse_constant(text);

        if !constants.is_empty() {
            components.extend(constants.into_iter().map(Expression::Constant));
        }

        components.extend(text.chars().map(Expression::Variable));

        if components.len() == 1 {
            components.pop().unwrap()
        } else {
            components
                .into_iter()
                .reduce(|left, right| {
                    Expression::binary_operator(BinaryOperatorKind::Multiplication, left, right)
                })
                .unwrap()
        }
    }

    fn parse_function(&mut self, text: &str) -> (Option<Expression>, String) {
        let mut result = (None, text.to_string());

        PredefinedFunction::all().iter().any(|func| {
            if let Some(rest) = text.strip_suffix(func) {
                let function_type = PredefinedFunction::get_function(func).unwrap();

                let mut arguments = Vec::new();

                self.next_token();
                self.equal_or_create_next(TokenKind::LeftParenthesis);

                for args in 0..function_type.args_count() {
                    arguments.push(self.parse_expression());
                    if args < function_type.args_count() - 1 {
                        self.equal_or_create_next(TokenKind::Comma);
                    }
                }

                arguments.retain(|arg| arg != &Expression::Error);
                let mut actual = arguments.len();

                if self.current_token().kind == TokenKind::Comma
                    || actual != function_type.args_count()
                {
                    while self.current_token().kind == TokenKind::Comma {
                        self.parse_expression();
                        self.next_token();
                        actual += 1;
                    }
                    self.diagnostics
                        .report_invalid_argument_count(&function_type, actual);
                }
                self.equal_or_create(TokenKind::RightParenthesis);

                result = (
                    Some(Expression::function(Function::new_predefined_function(
                        function_type,
                        arguments,
                    ))),
                    rest.to_string(),
                );
                true
            } else {
                false
            }
        });

        if result.0.is_none() {
            let function_name = &text[text.len() - 1..];

            if let Some(rest) = text.strip_suffix(function_name) {
                let mut arguments = Vec::new();

                self.next_token();
                self.equal_or_create_next(TokenKind::LeftParenthesis);

                loop {
                    arguments.push(self.parse_expression());
                    if self.current_token().kind == TokenKind::Comma {
                        self.next_token();
                    } else {
                        break;
                    }
                }

                self.equal_or_create(TokenKind::RightParenthesis);

                result = (
                    Some(Expression::function(Function::new_user_defined_function(
                        function_name.to_string(),
                        arguments,
                    ))),
                    rest.to_string(),
                );
            }
        }

        result
    }

    fn parse_constant(&mut self, mut text: String) -> (String, Vec<ConstantKind>) {
        let mut constants: Vec<ConstantKind> = Vec::new();

        text = Self::find_constant(text, ConstantKind::E, &mut constants);
        text = Self::find_constant(text, ConstantKind::Pi, &mut constants);
        (text, constants)
    }

    fn find_constant(
        mut text: String,
        constant: ConstantKind,
        constants: &mut Vec<ConstantKind>,
    ) -> String {
        let value = constant.as_text();
        let mut text_parsed: String = String::new();

        while let Some(num) = text.find(value) {
            text_parsed += &text[..num];
            text = text[num + value.len()..].to_string();
            constants.push(constant);
        }

        text_parsed + &text
    }

    fn string_to_fraction(text: String) -> (i64, i64) {
        let parts: Vec<&str> = text.split('.').collect();

        let interger_part = parts[0];
        let floating_part = parts[1];
        let n = floating_part.len() as u32;

        let denominator: i64 = 10_i64.pow(n);
        let float_parts_i64 = floating_part.parse::<i64>().unwrap();
        let numerator = interger_part.parse::<i64>().unwrap() * denominator + float_parts_i64;

        let gcd = Self::gcd(float_parts_i64, denominator);
        (numerator / gcd, denominator / gcd)
    }

    fn gcd(mut first: i64, mut second: i64) -> i64 {
        while second != 0 {
            let r = first % second;
            first = second;
            second = r;
        }
        first
    }
}

#[cfg(test)]
mod tests_parser {
    use crate::ast::{function, Expression};

    fn verify_parser(input: &str, expected: Expression) {
        let mut parser = super::Parser::new(input);
        let result = parser.parse();
        assert_eq!(result[0], expected);
    }

    #[test]
    fn negative_number() {
        verify_parser(
            "-2",
            Expression::unary_operator(super::UnaryOperatorKind::Negation, Expression::Number(2)),
        );
        verify_parser(
            "--2",
            Expression::unary_operator(
                super::UnaryOperatorKind::Negation,
                Expression::unary_operator(
                    super::UnaryOperatorKind::Negation,
                    Expression::Number(2),
                ),
            ),
        );
        verify_parser(
            "-2 + 3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::unary_operator(
                    super::UnaryOperatorKind::Negation,
                    Expression::Number(2),
                ),
                Expression::Number(3),
            ),
        );
        verify_parser(
            "3-2",
            Expression::binary_operator(
                super::BinaryOperatorKind::Subtraction,
                Expression::Number(3),
                Expression::Number(2),
            ),
        );
        verify_parser(
            "3*-2",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::Number(3),
                Expression::unary_operator(
                    super::UnaryOperatorKind::Negation,
                    Expression::Number(2),
                ),
            ),
        );
        verify_parser(
            "3/-3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Division,
                Expression::Number(3),
                Expression::unary_operator(
                    super::UnaryOperatorKind::Negation,
                    Expression::Number(3),
                ),
            ),
        );
        verify_parser(
            "6^-5",
            Expression::binary_operator(
                super::BinaryOperatorKind::Exponentiation,
                Expression::Number(6),
                Expression::unary_operator(
                    super::UnaryOperatorKind::Negation,
                    Expression::Number(5),
                ),
            ),
        );
        verify_parser(
            "3-2+1",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::binary_operator(
                    super::BinaryOperatorKind::Subtraction,
                    Expression::Number(3),
                    Expression::Number(2),
                ),
                Expression::Number(1),
            ),
        );
    }

    #[test]
    fn float() {
        verify_parser(
            "2.5",
            Expression::binary_operator(
                super::BinaryOperatorKind::Division,
                Expression::Number(5),
                Expression::Number(2),
            ),
        );
        verify_parser(
            "45.76",
            Expression::binary_operator(
                super::BinaryOperatorKind::Division,
                Expression::Number(1144),
                Expression::Number(25),
            ),
        );
        verify_parser("45..443", Expression::Error);
    }

    #[test]
    fn parenthesis() {
        verify_parser(
            "(2+4)",
            Expression::parenthesis(Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::Number(2),
                Expression::Number(4),
            )),
        );
        verify_parser(
            "2+(4+2)",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::Number(2),
                Expression::parenthesis(Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(4),
                    Expression::Number(2),
                )),
            ),
        );
        verify_parser(
            "2+(4+2)+3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(2),
                    Expression::parenthesis(Expression::binary_operator(
                        super::BinaryOperatorKind::Addition,
                        Expression::Number(4),
                        Expression::Number(2),
                    )),
                ),
                Expression::Number(3),
            ),
        );
        verify_parser(
            "((2/5)(4+1))3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::parenthesis(Expression::binary_operator(
                    super::BinaryOperatorKind::Multiplication,
                    Expression::parenthesis(Expression::binary_operator(
                        super::BinaryOperatorKind::Division,
                        Expression::Number(2),
                        Expression::Number(5),
                    )),
                    Expression::parenthesis(Expression::binary_operator(
                        super::BinaryOperatorKind::Addition,
                        Expression::Number(4),
                        Expression::Number(1),
                    )),
                )),
                Expression::Number(3),
            ),
        );
        verify_parser(
            "-(2+4)",
            Expression::unary_operator(
                super::UnaryOperatorKind::Negation,
                Expression::parenthesis(Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(2),
                    Expression::Number(4),
                )),
            ),
        );
    }

    #[test]
    fn exponentiation() {
        verify_parser(
            "3^2^3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Exponentiation,
                Expression::Number(3),
                Expression::binary_operator(
                    super::BinaryOperatorKind::Exponentiation,
                    Expression::Number(2),
                    Expression::Number(3),
                ),
            ),
        );
        verify_parser(
            "3^2^3^2",
            Expression::binary_operator(
                super::BinaryOperatorKind::Exponentiation,
                Expression::Number(3),
                Expression::binary_operator(
                    super::BinaryOperatorKind::Exponentiation,
                    Expression::Number(2),
                    Expression::binary_operator(
                        super::BinaryOperatorKind::Exponentiation,
                        Expression::Number(3),
                        Expression::Number(2),
                    ),
                ),
            ),
        );
        verify_parser(
            "(3^2)^-8",
            Expression::binary_operator(
                super::BinaryOperatorKind::Exponentiation,
                Expression::parenthesis(Expression::binary_operator(
                    super::BinaryOperatorKind::Exponentiation,
                    Expression::Number(3),
                    Expression::Number(2),
                )),
                Expression::unary_operator(
                    super::UnaryOperatorKind::Negation,
                    Expression::Number(8),
                ),
            ),
        );
        verify_parser(
            "3^-2^3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Exponentiation,
                Expression::Number(3),
                Expression::binary_operator(
                    super::BinaryOperatorKind::Exponentiation,
                    Expression::unary_operator(
                        super::UnaryOperatorKind::Negation,
                        Expression::Number(2),
                    ),
                    Expression::Number(3),
                ),
            ),
        );
    }

    #[test]
    fn implicit_multiplication() {
        verify_parser(
            "2(3)",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::Number(2),
                Expression::parenthesis(Expression::Number(3)),
            ),
        );
        verify_parser(
            "2(3+2)",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::Number(2),
                Expression::parenthesis(Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(3),
                    Expression::Number(2),
                )),
            ),
        );
        verify_parser(
            "2(3+2)3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::binary_operator(
                    super::BinaryOperatorKind::Multiplication,
                    Expression::Number(2),
                    Expression::parenthesis(Expression::binary_operator(
                        super::BinaryOperatorKind::Addition,
                        Expression::Number(3),
                        Expression::Number(2),
                    )),
                ),
                Expression::Number(3),
            ),
        );

        verify_parser(
            "2(3+2)3(2+1)",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::binary_operator(
                    super::BinaryOperatorKind::Multiplication,
                    Expression::binary_operator(
                        super::BinaryOperatorKind::Multiplication,
                        Expression::Number(2),
                        Expression::parenthesis(Expression::binary_operator(
                            super::BinaryOperatorKind::Addition,
                            Expression::Number(3),
                            Expression::Number(2),
                        )),
                    ),
                    Expression::Number(3),
                ),
                Expression::parenthesis(Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(2),
                    Expression::Number(1),
                )),
            ),
        );
    }

    #[test]
    fn variables() {
        verify_parser("x", Expression::Variable('x'));
        verify_parser(
            "x+2",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::Variable('x'),
                Expression::Number(2),
            ),
        );
        verify_parser(
            "x+2y",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::Variable('x'),
                Expression::binary_operator(
                    super::BinaryOperatorKind::Multiplication,
                    Expression::Number(2),
                    Expression::Variable('y'),
                ),
            ),
        );
        verify_parser(
            "x+2y+3",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Variable('x'),
                    Expression::binary_operator(
                        super::BinaryOperatorKind::Multiplication,
                        Expression::Number(2),
                        Expression::Variable('y'),
                    ),
                ),
                Expression::Number(3),
            ),
        );
        verify_parser(
            "x+2y+3z",
            Expression::binary_operator(
                super::BinaryOperatorKind::Addition,
                Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Variable('x'),
                    Expression::binary_operator(
                        super::BinaryOperatorKind::Multiplication,
                        Expression::Number(2),
                        Expression::Variable('y'),
                    ),
                ),
                Expression::binary_operator(
                    super::BinaryOperatorKind::Multiplication,
                    Expression::Number(3),
                    Expression::Variable('z'),
                ),
            ),
        );
    }

    #[test]
    fn function() {
        verify_parser(
            "ln(2)",
            Expression::function(function::Function::new_predefined_function(
                function::PredefinedFunction::Ln,
                vec![Expression::Number(2)],
            )),
        );

        verify_parser(
            "ln(2+3)",
            Expression::function(function::Function::new_predefined_function(
                function::PredefinedFunction::Ln,
                vec![Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(2),
                    Expression::Number(3),
                )],
            )),
        );

        // sin(2+3)
        verify_parser(
            "sin(2+3)",
            Expression::function(function::Function::new_predefined_function(
                function::PredefinedFunction::Sin,
                vec![Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(2),
                    Expression::Number(3),
                )],
            )),
        );

        // asin(2+3)
        verify_parser(
            "asin(2+3)",
            Expression::function(function::Function::new_predefined_function(
                function::PredefinedFunction::Asin,
                vec![Expression::binary_operator(
                    super::BinaryOperatorKind::Addition,
                    Expression::Number(2),
                    Expression::Number(3),
                )],
            )),
        );

        //a(x)
        verify_parser(
            "a(x)",
            Expression::function(function::Function::new_user_defined_function(
                "a".to_string(),
                vec![Expression::Variable('x')],
            )),
        );

        //ab(x, r)
        verify_parser(
            "ab(x, r)",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::function(function::Function::new_user_defined_function(
                    "b".to_string(),
                    vec![Expression::Variable('x'), Expression::Variable('r')],
                )),
                Expression::Variable('a'),
            ),
        );

        //ab(x, r)b(x, t)
        verify_parser(
            "ab(x, r)b(x, t)",
            Expression::binary_operator(
                super::BinaryOperatorKind::Multiplication,
                Expression::binary_operator(
                    super::BinaryOperatorKind::Multiplication,
                    Expression::function(function::Function::new_user_defined_function(
                        "b".to_string(),
                        vec![Expression::Variable('x'), Expression::Variable('r')],
                    )),
                    Expression::Variable('a'),
                ),
                Expression::function(function::Function::new_user_defined_function(
                    "b".to_string(),
                    vec![Expression::Variable('x'), Expression::Variable('t')],
                )),
            ),
        );
    }
}
