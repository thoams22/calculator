use crate::{
    ast::{
        function::{FunctionType, PredefinedFunction},
        ConstantKind, Expression, Statement,
    },
    diagnostic::Diagnostics,
    lexer::{Lexer, Span, Token, TokenKind},
    utils::gcd,
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

    pub fn get_diagnostic_message(&mut self) -> Vec<String> {
        self.diagnostics.get_messages()
    }

    fn peek_token(&mut self, offset: isize) -> Token {
        let index: usize = (self.position as isize + offset).try_into().unwrap();
        self.tokens
            .get(index)
            .unwrap_or_else(|| self.tokens.last().unwrap())
            .clone()
    }

    fn next_token(&mut self) -> Token {
        let current = self.current_token().clone();
        self.position += 1;
        current
    }

    fn previous_token(&mut self) -> Token {
        let current = self.current_token().clone();
        self.position -= 1;
        current
    }

    fn current_token(&mut self) -> Token {
        self.peek_token(0)
    }

    fn equal_or_create_next(&mut self, kind: TokenKind) -> Token {
        if self.current_token().kind == kind {
            self.next_token()
        } else {
            let current = self.current_token().kind;
            self.diagnostics.report_unexpected_token(&current, &kind);
            Token {
                kind,
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
                kind,
                text: String::new(),
                span: self.current_token().span,
            }
        }
    }

    pub fn parse(&mut self) -> Statement {
        let mut result = Statement::Error;
        while let Some(expr) = self.parse_type() {
            if self.current_token().kind == TokenKind::Comma {
                // TODO add a loop to get second equality if substitue or for later sys eq
                self.next_token();
                let after_comma = self.parse_expression();
                println!("{after_comma}");
                if let Expression::Variable(var) = after_comma {
                    result = Statement::SolveFor(expr, var);
                    break;
                } else if let Expression::Equality(eq) = after_comma {
                    result = Statement::Replace(expr, *eq);
                    break;
                }
            } else {
                if let Expression::Equality(_) = expr {
                    result = Statement::Solve(expr);
                } else {
                    result = Statement::Simplify(expr);
                }
                break;
            }
        }
        result
    }

    fn parse_type(&mut self) -> Option<Expression> {
        if self.current_token().kind == TokenKind::End {
            return None;
        }
        Some(self.parse_expression())
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_binary_expression(None, 0)
    }

    fn parse_binary_expression(
        &mut self,
        left: Option<Expression>,
        precedence: usize,
    ) -> Expression {
        let mut left = if let Some(expr) = left {
            expr
        } else {
            self.parse_unary_expression()
        };

        while let Some(mut operator) = self.parse_binary_operator() {
            let op_precedence = operator.precedence();
            if op_precedence < precedence
                || (precedence != BinaryOperatorKind::Exponentiation.precedence()
                    && op_precedence == precedence)
            {
                break;
            }
            self.next_token();

            let right = self.parse_binary_expression(None, op_precedence);
            left = match operator {
                BinaryOperatorKind::Addition => Expression::addition(left, right),
                BinaryOperatorKind::Subtraction => {
                    Expression::addition(left, Expression::negation(right))
                }
                BinaryOperatorKind::Multiplication => Expression::multiplication(left, right),
                BinaryOperatorKind::Division => Expression::fraction(left, right),
                BinaryOperatorKind::Exponentiation => Expression::exponentiation(left, right),
                BinaryOperatorKind::Equality => Expression::equality(left, right),
            }
        }
        left
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperatorKind> {
        match self.current_token().kind {
            TokenKind::Minus => Some(BinaryOperatorKind::Subtraction),
            TokenKind::Plus => Some(BinaryOperatorKind::Addition),
            TokenKind::Slash => Some(BinaryOperatorKind::Division),
            TokenKind::Star => Some(BinaryOperatorKind::Multiplication),
            TokenKind::Hat => Some(BinaryOperatorKind::Exponentiation),
            TokenKind::Equal => Some(BinaryOperatorKind::Equality),
            _ => None,
        }
    }

    fn parse_unary_expression(&mut self) -> Expression {
        while let Some(operator) = self.parse_unary_operator() {
            self.next_token();
            let operand = self.parse_unary_expression();

            return match operator {
                UnaryOperatorKind::Negation => Expression::negation(operand),
            };
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
        match current.kind {
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
                expression
            }
            _ => {
                self.diagnostics.report_unexpected_primary(&current.kind);
                self.next_token();
                Expression::Error
            }
        }
    }

    fn parse_number_expression(&mut self) -> Expression {
        let current = self.next_token();
        match current.text.parse::<i64>() {
            Ok(number) => Expression::Number(number),
            Err(_) => match current.text.parse::<f64>() {
                Ok(_) => {
                    let (num, denom) = Self::string_to_fraction(current.text);
                    Expression::fraction(Expression::Number(num), Expression::Number(denom))
                }
                Err(_) => {
                    self.diagnostics.report_unexpected_number(&current);
                    Expression::Error
                }
            },
        }
    }

    fn parse_variable(&mut self, mut text: String) -> Expression {
        let mut components: Vec<Expression> = vec![];

        if self.peek_token(1).kind == TokenKind::LeftParenthesis {
            match self.parse_function(&text) {
                (None, txt) => {
                    text = txt;
                }
                (Some(function), txt) => {
                    text = txt;
                    if self.peek_token(1).kind == TokenKind::Hat {
                        self.next_token();
                        components.push(self.parse_binary_expression(
                            Some(function),
                            BinaryOperatorKind::Multiplication.precedence(),
                        ));
                        self.previous_token();
                    } else {
                        components.push(function);
                    }
                }
            }
        }

        let (mut text, constants) = self.parse_constant(text);

        if !constants.is_empty() {
            components.extend(constants);
        }

        if self.peek_token(1).kind == TokenKind::Hat {
            self.next_token();
            let var = text.chars().last().unwrap();
            components.push(self.parse_binary_expression(
                Some(Expression::Variable(var)),
                BinaryOperatorKind::Multiplication.precedence(),
            ));
            text.pop();
            self.previous_token();
            components.extend(text.chars().map(Expression::Variable));
        } else {
            components.extend(text.chars().map(Expression::Variable));
        }

        if components.len() == 1 {
            components.pop().unwrap()
        } else {
            components
                .into_iter()
                .reduce(Expression::multiplication)
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
                    Some(Expression::function(FunctionType::Predefined(
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
                    Some(Expression::function(FunctionType::UserDefined(
                        function_name.to_string(),
                        arguments,
                    ))),
                    rest.to_string(),
                );
            }
        }

        result
    }

    fn parse_constant(&mut self, mut text: String) -> (String, Vec<Expression>) {
        let mut components: Vec<Expression> = Vec::new();

        text = self.find_constant(text, ConstantKind::E, &mut components);
        text = self.find_constant(text, ConstantKind::Pi, &mut components);
        (text, components)
    }

    fn find_constant(
        &mut self,
        mut text: String,
        constant: ConstantKind,
        components: &mut Vec<Expression>,
    ) -> String {
        let value = constant.as_text();
        let mut text_parsed: String = String::new();

        while let Some(num) = text.find(value) {
            if num == text.len() - value.len() {
                if self.peek_token(1).kind == TokenKind::Hat {
                    self.next_token();
                    components.push(self.parse_binary_expression(
                        Some(Expression::Constant(constant)),
                        BinaryOperatorKind::Multiplication.precedence(),
                    ));
                    self.previous_token();
                } else {
                    components.push(Expression::Constant(constant));
                }
            } else {
                components.push(Expression::Constant(constant));
            }
            text_parsed += &text[..num];
            text = text[num + value.len()..].to_string();
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

        let gcd = gcd(float_parts_i64, denominator);
        (numerator / gcd, denominator / gcd)
    }
}

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
            BinaryOperatorKind::Exponentiation => 4,
            BinaryOperatorKind::Multiplication | BinaryOperatorKind::Division => 3,
            BinaryOperatorKind::Addition | BinaryOperatorKind::Subtraction => 2,
            BinaryOperatorKind::Equality => 1,
        }
    }
}

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

#[cfg(test)]
mod tests_parser {
    use crate::{
        ast::{
            function::{FunctionType, PredefinedFunction},
            ConstantKind, Expression, Statement,
        },
        parser::Parser,
    };

    fn verify_parser(input: &str, expected: Statement) {
        let mut parser = Parser::new(input);
        let result = parser.parse();
        assert_eq!(result, expected);
    }

    #[test]
    fn negative_number() {
        verify_parser(
            "-2",
            Statement::Simplify(Expression::negation(Expression::Number(2))),
        );
        verify_parser(
            "--2",
            Statement::Simplify(Expression::negation(Expression::negation(
                Expression::Number(2),
            ))),
        );
        verify_parser(
            "-2 + 3",
            Statement::Simplify(Expression::addition(
                Expression::negation(Expression::Number(2)),
                Expression::Number(3),
            )),
        );
        verify_parser(
            "3-2",
            Statement::Simplify(Expression::addition(
                Expression::Number(3),
                Expression::negation(Expression::Number(2)),
            )),
        );
        verify_parser(
            "3*-2",
            Statement::Simplify(Expression::multiplication(
                Expression::Number(3),
                Expression::negation(Expression::Number(2)),
            )),
        );
        verify_parser(
            "3/-3",
            Statement::Simplify(Expression::fraction(
                Expression::Number(3),
                Expression::negation(Expression::Number(3)),
            )),
        );
        verify_parser(
            "6^-5",
            Statement::Simplify(Expression::exponentiation(
                Expression::Number(6),
                Expression::negation(Expression::Number(5)),
            )),
        );
        verify_parser(
            "3-2+1",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::Number(3),
                    Expression::negation(Expression::Number(2)),
                ),
                Expression::Number(1),
            )),
        );
    }

    #[test]
    fn float() {
        verify_parser(
            "2.5",
            Statement::Simplify(Expression::fraction(
                Expression::Number(5),
                Expression::Number(2),
            )),
        );
        verify_parser(
            "45.76",
            Statement::Simplify(Expression::fraction(
                Expression::Number(1144),
                Expression::Number(25),
            )),
        );
        verify_parser("45..443", Statement::Simplify(Expression::Error));
    }

    #[test]
    fn parenthesis() {
        verify_parser(
            "(2+4)",
            Statement::Simplify(Expression::addition(
                Expression::Number(2),
                Expression::Number(4),
            )),
        );
        verify_parser(
            "2+(4+2)",
            Statement::Simplify(Expression::addition(
                Expression::Number(2),
                Expression::addition(Expression::Number(4), Expression::Number(2)),
            )),
        );
        verify_parser(
            "2+(4+2)+3",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::Number(2),
                    Expression::addition(Expression::Number(4), Expression::Number(2)),
                ),
                Expression::Number(3),
            )),
        );
        verify_parser(
            "((2/5)(4+1))3",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::fraction(Expression::Number(2), Expression::Number(5)),
                    Expression::addition(Expression::Number(4), Expression::Number(1)),
                ),
                Expression::Number(3),
            )),
        );
        verify_parser(
            "-(2+4)",
            Statement::Simplify(Expression::negation(Expression::addition(
                Expression::Number(2),
                Expression::Number(4),
            ))),
        );
    }

    #[test]
    fn exponentiation() {
        verify_parser(
            "3^2^3",
            Statement::Simplify(Expression::exponentiation(
                Expression::Number(3),
                Expression::exponentiation(Expression::Number(2), Expression::Number(3)),
            )),
        );
        verify_parser(
            "3^2^3^2",
            Statement::Simplify(Expression::exponentiation(
                Expression::Number(3),
                Expression::exponentiation(
                    Expression::Number(2),
                    Expression::exponentiation(Expression::Number(3), Expression::Number(2)),
                ),
            )),
        );
        verify_parser(
            "(3^2)^-8",
            Statement::Simplify(Expression::exponentiation(
                Expression::exponentiation(Expression::Number(3), Expression::Number(2)),
                Expression::negation(Expression::Number(8)),
            )),
        );
        verify_parser(
            "3^-2^3",
            Statement::Simplify(Expression::exponentiation(
                Expression::Number(3),
                Expression::exponentiation(
                    Expression::negation(Expression::Number(2)),
                    Expression::Number(3),
                ),
            )),
        );
    }

    #[test]
    fn implicit_multiplication() {
        verify_parser(
            "2(3)",
            Statement::Simplify(Expression::multiplication(
                Expression::Number(2),
                Expression::Number(3),
            )),
        );
        verify_parser(
            "2(3+2)",
            Statement::Simplify(Expression::multiplication(
                Expression::Number(2),
                Expression::addition(Expression::Number(3), Expression::Number(2)),
            )),
        );
        verify_parser(
            "2(3+2)3",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::Number(2),
                    Expression::addition(Expression::Number(3), Expression::Number(2)),
                ),
                Expression::Number(3),
            )),
        );

        verify_parser(
            "2(3+2)3(2+1)",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::Number(2),
                        Expression::addition(Expression::Number(3), Expression::Number(2)),
                    ),
                    Expression::Number(3),
                ),
                Expression::addition(Expression::Number(2), Expression::Number(1)),
            )),
        );
    }

    #[test]
    fn variables() {
        verify_parser("x", Statement::Simplify(Expression::Variable('x')));
        verify_parser(
            "x+2",
            Statement::Simplify(Expression::addition(
                Expression::Variable('x'),
                Expression::Number(2),
            )),
        );
        verify_parser(
            "x+2y",
            Statement::Simplify(Expression::addition(
                Expression::Variable('x'),
                Expression::multiplication(Expression::Number(2), Expression::Variable('y')),
            )),
        );
        verify_parser(
            "x+2y+3",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::Variable('x'),
                    Expression::multiplication(Expression::Number(2), Expression::Variable('y')),
                ),
                Expression::Number(3),
            )),
        );
        verify_parser(
            "x+2y+3z",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::Variable('x'),
                    Expression::multiplication(Expression::Number(2), Expression::Variable('y')),
                ),
                Expression::multiplication(Expression::Number(3), Expression::Variable('z')),
            )),
        );
    }

    #[test]
    fn function() {
        verify_parser(
            "ln(2)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::Number(2)],
            ))),
        );

        verify_parser(
            "ln(2+3)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::addition(
                    Expression::Number(2),
                    Expression::Number(3),
                )],
            ))),
        );

        // sin(2+3)
        verify_parser(
            "sin(2+3)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::addition(
                    Expression::Number(2),
                    Expression::Number(3),
                )],
            ))),
        );

        // asin(2+3)
        verify_parser(
            "asin(2+3)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Asin,
                vec![Expression::addition(
                    Expression::Number(2),
                    Expression::Number(3),
                )],
            ))),
        );

        //bsin(pi/2)
        verify_parser(
            "bsin(pi/2)",
            Statement::Simplify(Expression::multiplication(
                Expression::function(FunctionType::Predefined(
                    PredefinedFunction::Sin,
                    vec![Expression::fraction(
                        Expression::Constant(ConstantKind::Pi),
                        Expression::Number(2),
                    )],
                )),
                Expression::Variable('b'),
            )),
        );

        //a(x)
        verify_parser(
            "a(x)",
            Statement::Simplify(Expression::function(FunctionType::UserDefined(
                "a".to_string(),
                vec![Expression::Variable('x')],
            ))),
        );

        //ab(x, r)
        verify_parser(
            "ab(x, r)",
            Statement::Simplify(Expression::multiplication(
                Expression::function(FunctionType::UserDefined(
                    "b".to_string(),
                    vec![Expression::Variable('x'), Expression::Variable('r')],
                )),
                Expression::Variable('a'),
            )),
        );

        //ab(x, r)b(x, t)
        verify_parser(
            "ab(x, r)b(x, t)",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::function(FunctionType::UserDefined(
                        "b".to_string(),
                        vec![Expression::Variable('x'), Expression::Variable('r')],
                    )),
                    Expression::Variable('a'),
                ),
                Expression::function(FunctionType::UserDefined(
                    "b".to_string(),
                    vec![Expression::Variable('x'), Expression::Variable('t')],
                )),
            )),
        );
    }

    #[test]
    fn equality() {
        // 2= 2
        verify_parser(
            "2=2",
            Statement::Solve(Expression::equality(
                Expression::Number(2),
                Expression::Number(2),
            )),
        );

        // 2+3= 2
        verify_parser(
            "2+3=2",
            Statement::Solve(Expression::equality(
                Expression::addition(Expression::Number(2), Expression::Number(3)),
                Expression::Number(2),
            )),
        );

        // 2ln(4) = e^(-pi)
        verify_parser(
            "2ln(4) = e^(-pi)",
            Statement::Solve(Expression::equality(
                Expression::multiplication(
                    Expression::Number(2),
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Ln,
                        vec![Expression::Number(4)],
                    )),
                ),
                Expression::exponentiation(
                    Expression::Constant(ConstantKind::E),
                    Expression::negation(Expression::Constant(ConstantKind::Pi)),
                ),
            )),
        );
    }
}
