use crate::{
    ast::{
        constant::ConstantKind,
        function::{FunctionType, PredefinedFunction},
        varibale::Variable,
        Expression, Statement,
    },
    lexer::{Lexer, Span, Token, TokenKind},
    utils::gcd,
};

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("No Token to parse")]
    NoToken,
    #[error("Unexpected Token: <{0}>")]
    UnexpectedToken(TokenKind),
    #[error("Invalid TokenKind: Expected <{expected}>, found <{found}>")]
    UnexpectedTokenKind {
        expected: TokenKind,
        found: TokenKind,
    },
    #[error("Invalid Expression: Expected <Variable> OR <Equality>, found <{0}>")]
    ExpectedVarOrEquality(Expression),
    #[error("Invalid Number: Expected <i64> OR <f64>, found <{0}>")]
    ExpectedNumber(String),
    #[error(
        "Invalid number of argument(s): Function '{0}' expects {1} argument(s), but was given {2}"
    )]
    InvalidArgumentCount(PredefinedFunction, usize, usize),
    #[error("Invalid degree of derivation: Expected same values, found <{0}> and <{1}>")]
    InvalidDegreeOfDerivation(Expression, Expression),
    #[error("Invalid variable of derivation: Expected <Literal>, found <{0}>")]
    InvalidVariableOfDerivation(TokenKind),
}

#[derive(Debug, Default)]
pub struct Parser {
    pub position: usize,
    pub tokens: Vec<Token>,
    pub diagnostics: Vec<ParserError>,
}

impl Parser {
    /// Lex the input string
    pub fn lex(mut self, text: &str) -> Result<Self, anyhow::Error> {
        let mut lexer = Lexer::new(text.chars().collect());

        let mut token: Token = lexer.next_token()?;
        let mut tokens: Vec<Token> = Vec::new();

        while token.kind != TokenKind::End {
            if token.kind != TokenKind::WhiteSpace {
                tokens.push(token);
            }
            token = lexer.next_token()?;
        }

        if token.kind == TokenKind::End {
            tokens.push(token);
        }
        self.tokens = tokens;
        Ok(self)
    }

    fn insert_tokens(&mut self, elem: Token) {
        self.tokens.insert(self.position, elem);
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
            self.diagnostics.push(ParserError::UnexpectedTokenKind {
                expected: kind,
                found: current,
            });
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
            self.diagnostics.push(ParserError::UnexpectedTokenKind {
                expected: kind,
                found: current,
            });
            Token {
                kind,
                text: String::new(),
                span: self.current_token().span,
            }
        }
    }

    fn is_equal(&mut self, kind: TokenKind) -> bool {
        self.current_token().kind == kind
    }

    /// Parse the tokens previously lexed and return a Statement or an error
    pub fn parse(&mut self) -> Result<Statement, Vec<ParserError>> {
        if self.tokens.is_empty() {
            self.diagnostics.push(ParserError::NoToken);
            return Err(self.diagnostics.clone());
        } else {
            while let Some(expr) = self.parse_type() {
                if self.current_token().kind == TokenKind::Comma {
                    // TODO add a loop to get second equality if substitue or for later sys eq
                    self.next_token();
                    let after_comma = self.parse_expression();
                    if let Expression::Variable(var) = after_comma {
                        if self.diagnostics.is_empty() {
                            return Ok(Statement::SolveFor(expr, var));
                        } else {
                            return Err(self.diagnostics.clone());
                        }
                    } else if let Expression::Equality(eq) = after_comma {
                        if self.diagnostics.is_empty() {
                            return Ok(Statement::Replace(expr, *eq));
                        } else {
                            return Err(self.diagnostics.clone());
                        }
                    } else {
                        self.diagnostics
                            .push(ParserError::ExpectedVarOrEquality(after_comma));
                        return Err(self.diagnostics.clone());
                    }
                } else {
                    // if current is not end throw error and try parse the rest to maybe show other error(s) 
                    if self.current_token().kind != TokenKind::End {
                        let current  = self.current_token().kind;
                        self.diagnostics.push(ParserError::UnexpectedToken(current));
                        self.next_token();
                    }
                    else if self.diagnostics.is_empty() {
                        if let Expression::Equality(_) = expr {
                            return Ok(Statement::Solve(expr));
                        } else {
                            return Ok(Statement::Simplify(expr));
                        }
                    } else {
                        return Err(self.diagnostics.clone());
                    }
                }
            }
        }
        Err(self.diagnostics.clone())
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
                BinaryOperatorKind::Subtraction => Expression::substraction(left, right),
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
                let expression = self.parse_literal_expression();
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
                        String::from("*"),
                    ))
                }
                expression
            }
            _ => {
                self.diagnostics
                    .push(ParserError::UnexpectedToken(
                        current.kind,
                    ));
                self.next_token();
                Expression::number(0)
            }
        }
    }

    fn parse_number_expression(&mut self) -> Expression {
        let current = self.next_token();
        match current.text.parse::<i64>() {
            Ok(number) => Expression::number(number),
            Err(_) => match current.text.parse::<f64>() {
                Ok(_) => {
                    let (num, denom) = Self::string_to_fraction(current.text);
                    Expression::fraction(Expression::number(num), Expression::number(denom))
                }
                Err(_) => {
                    self.diagnostics
                        .push(ParserError::ExpectedNumber(current.text));
                    Expression::number(0)
                }
            },
        }
    }

    fn parse_literal_expression(&mut self) -> Expression {
        if let Some(expr) = self.parse_derivate() {
            return expr;
        }

        let mut text = self.current_token().text;

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

        text = self.parse_imaginary_unit(text, &mut components);

        if !text.is_empty() {
            components.push(self.parse_variable(text, true));
        }

        if components.len() == 1 {
            components[0].clone()
        } else {
            Expression::multiplication_from_vec(components)
        }
    }

    fn parse_variable(&mut self, mut text: String, peek_hat: bool) -> Expression {
        let mut components: Vec<Expression> = vec![];

        // check if the last var as a power
        if peek_hat && self.peek_token(1).kind == TokenKind::Hat {
            self.next_token();
            let var = text.chars().last().unwrap();
            components.push(self.parse_binary_expression(
                Some(Expression::variable(var.to_string())),
                BinaryOperatorKind::Multiplication.precedence(),
            ));
            text.pop();
            self.previous_token();
            components.extend(
                text.chars()
                    .map(|var| Expression::variable(var.to_string())),
            );
        } else {
            components.extend(
                text.chars()
                    .map(|var| Expression::variable(var.to_string())),
            );
        }

        if components.len() == 1 {
            components[0].clone()
        } else {
            Expression::multiplication_from_vec(components)
        }
    }

    fn parse_imaginary_unit(
        &mut self,
        mut text: String,
        components: &mut Vec<Expression>,
    ) -> String {
        let mut text_parsed: String = String::new();

        while let Some(num) = text.find('i') {
            if num == text.len() - 1 {
                if self.peek_token(1).kind == TokenKind::Hat {
                    self.next_token();
                    components.push(self.parse_binary_expression(
                        Some(Expression::ImaginaryUnit),
                        BinaryOperatorKind::Multiplication.precedence(),
                    ));
                    self.previous_token();
                } else {
                    components.push(Expression::ImaginaryUnit);
                }
            } else {
                components.push(Expression::ImaginaryUnit);
            }
            text_parsed += &text[..num];
            text = text[num + 1..].to_string();
        }

        text_parsed + &text
    }

    fn parse_function(&mut self, text: &str) -> (Option<Expression>, String) {
        let mut result = (None, text.to_string());

        // TODO add parsing of user defined function.

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

                let mut actual = arguments.len();

                if self.current_token().kind == TokenKind::Comma
                    || actual != function_type.args_count()
                {
                    while self.current_token().kind == TokenKind::Comma {
                        self.parse_expression();
                        self.next_token();
                        actual += 1;
                    }
                    self.diagnostics.push(ParserError::InvalidArgumentCount(
                        function_type.clone(),
                        function_type.args_count(),
                        actual,
                    ));
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

        // loop to get all constants
        for constant in ConstantKind::get_all() {
            text = self.find_constant(text, constant, &mut components);
        }

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

    /// d / dy (xxx)
    fn parse_derivate(&mut self) -> Option<Expression> {
        // TODO add degree of derivation

        let begin_position = self.position;
        let text = self.current_token().text;

        if "d" == text {
            self.next_token();
            let derivation_degree: Option<Expression> = if self.is_equal(TokenKind::Hat) {
                // d^
                self.next_token();

                let degree = if self.is_equal(TokenKind::Number) {
                    // d^x
                    self.parse_number_expression()
                } else {
                    self.position = begin_position;
                    return None;
                };
                Some(degree)
            } else {
                None
            };
            // d{^x}
            if let (TokenKind::Slash, TokenKind::Literal) =
                (self.current_token().kind, self.peek_token(1).kind)
            {
                // d{^x}/lit
                let text_peeked = self.peek_token(1).text;
                if text_peeked.starts_with('d') && text_peeked.len() >= 2 {
                    // d{^x}/dlit
                    self.next_token();

                    let derivation_variable = if let Expression::Variable(var) =
                        self.parse_variable(String::from(&text_peeked[1..]), false)
                    {
                        var
                    } else {
                        self.diagnostics
                            .push(ParserError::InvalidVariableOfDerivation(
                                self.tokens
                                    .get(self.position)
                                    .unwrap_or_else(|| self.tokens.last().unwrap())
                                    .kind,
                            ));
                        Variable::new(String::from("var€"))
                    };

                    self.next_token();

                    if self.is_equal(TokenKind::Hat) {
                        if let Some(degree) = derivation_degree.clone() {
                            // d^x/dy^
                            self.next_token();
                            let num = self.parse_number_expression();
                            if !self.is_equal(TokenKind::Number) && !num.equal(&degree) {
                                self.diagnostics
                                    .push(ParserError::InvalidDegreeOfDerivation(degree, num));
                                self.position = begin_position;
                                return None;
                            }
                            // d^x/dy^x
                        } else {
                            // d/dy^
                            self.diagnostics.push(ParserError::UnexpectedTokenKind {
                                expected: TokenKind::LeftParenthesis,
                                found: TokenKind::Hat,
                            });
                            self.next_token();
                            self.parse_number_expression();
                            self.position = begin_position;
                            return None;
                        }
                    }
                    // d{^x}/dy{^x}(expr)
                    if !self.is_equal(TokenKind::LeftParenthesis) {
                        self.position = begin_position;
                        return None;
                    }
                    self.next_token();
                    let expression = self.parse_expression();
                    self.equal_or_create(TokenKind::RightParenthesis);

                    return Some(Expression::derivation(
                        expression,
                        derivation_variable,
                        derivation_degree,
                    ));
                }
                return None;
            }
            return None;
        }
        None
    }

    fn string_to_fraction(text: String) -> (i64, i64) {
        let parts: Vec<&str> = text.split('.').collect();

        let interger_part = parts[0];
        let floating_part = parts[1];
        let n: u32 = floating_part.len() as u32;

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

#[cfg(test)]
mod tests_parser {
    use crate::{
        ast::{
            constant::ConstantKind,
            function::{FunctionType, PredefinedFunction},
            varibale::Variable,
            Expression, Statement,
        },
        parser::Parser,
    };

    fn verify_parser(input: &str, expected: Statement) {
        match Parser::default().lex(input) {
            Ok(mut parser) => match parser.parse() {
                Ok(mut statement) => assert!(
                    statement.equal(&expected),
                    "Inuput : {:?} \nResult : {:?}\nExpected : {:?}",
                    input,
                    statement,
                    expected
                ),
                Err(err) => {
                    err.iter().for_each(|e| eprintln!("{}", e));
                }
            },
            Err(err) => eprintln!("{}", err),
        };
    }

    // fn verify_parser_error(input: &str, expected: Result<Statement, Vec<ParserError>>) {
    //     match Parser::default().lex(input) {
    //         Ok(mut parser) => match parser.parse() {
    //             Ok(mut statement) => assert!(
    //                 expected.is_ok_and(|mut expect_statmnt| expect_statmnt.equal(&statement)),
    //                 "Inuput : {:?} \nResult : {:?}\nExpected : {:?}",
    //                 input,
    //                 statement,
    //                 expected
    //             ),
    //             Err(err) => {
    //                 assert!(expected.is_err_and(|expect_err| expect_err == err),
    //                 "Inuput : {:?} \nResult : {:?}\nExpected : {:?}",
    //                 input,
    //                 err,
    //                 expected
    //             );
    //             }
    //         },
    //         Err(err) => eprintln!("{}", err),
    //     };
    // }

    #[test]
    fn negative_number() {
        verify_parser(
            "-2",
            Statement::Simplify(Expression::negation(Expression::number(2))),
        );
        verify_parser(
            "--2",
            Statement::Simplify(Expression::negation(Expression::negation(
                Expression::number(2),
            ))),
        );
        verify_parser(
            "-2 + 3",
            Statement::Simplify(Expression::addition(
                Expression::negation(Expression::number(2)),
                Expression::number(3),
            )),
        );
        verify_parser(
            "3-2",
            Statement::Simplify(Expression::addition(
                Expression::number(3),
                Expression::negation(Expression::number(2)),
            )),
        );
        verify_parser(
            "3*-2",
            Statement::Simplify(Expression::multiplication(
                Expression::number(3),
                Expression::negation(Expression::number(2)),
            )),
        );
        verify_parser(
            "3/-3",
            Statement::Simplify(Expression::fraction(
                Expression::number(3),
                Expression::negation(Expression::number(3)),
            )),
        );
        verify_parser(
            "6^-5",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(6),
                Expression::negation(Expression::number(5)),
            )),
        );
        verify_parser(
            "3-2+1",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::number(3),
                    Expression::negation(Expression::number(2)),
                ),
                Expression::number(1),
            )),
        );
    }

    #[test]
    fn float() {
        verify_parser(
            "2.5",
            Statement::Simplify(Expression::fraction(
                Expression::number(5),
                Expression::number(2),
            )),
        );
        verify_parser(
            "45.76",
            Statement::Simplify(Expression::fraction(
                Expression::number(1144),
                Expression::number(25),
            )),
        );
    }

    #[test]
    fn parenthesis() {
        verify_parser(
            "(2+4)",
            Statement::Simplify(Expression::addition(
                Expression::number(2),
                Expression::number(4),
            )),
        );
        verify_parser(
            "2+(4+2)",
            Statement::Simplify(Expression::addition(
                Expression::number(2),
                Expression::addition(Expression::number(4), Expression::number(2)),
            )),
        );
        verify_parser(
            "2+(4+2)+3",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::number(2),
                    Expression::addition(Expression::number(4), Expression::number(2)),
                ),
                Expression::number(3),
            )),
        );
        verify_parser(
            "((2/5)(4+1))3",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::fraction(Expression::number(2), Expression::number(5)),
                    Expression::addition(Expression::number(4), Expression::number(1)),
                ),
                Expression::number(3),
            )),
        );
        verify_parser(
            "-(2+4)",
            Statement::Simplify(Expression::negation(Expression::addition(
                Expression::number(2),
                Expression::number(4),
            ))),
        );
    }

    #[test]
    fn exponentiation() {
        verify_parser(
            "3^2^3",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(3),
                Expression::exponentiation(Expression::number(2), Expression::number(3)),
            )),
        );
        verify_parser(
            "3^2^3^2",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(3),
                Expression::exponentiation(
                    Expression::number(2),
                    Expression::exponentiation(Expression::number(3), Expression::number(2)),
                ),
            )),
        );
        verify_parser(
            "(3^2)^-8",
            Statement::Simplify(Expression::exponentiation(
                Expression::exponentiation(Expression::number(3), Expression::number(2)),
                Expression::negation(Expression::number(8)),
            )),
        );
        verify_parser(
            "3^-2^3",
            Statement::Simplify(Expression::exponentiation(
                Expression::number(3),
                Expression::exponentiation(
                    Expression::negation(Expression::number(2)),
                    Expression::number(3),
                ),
            )),
        );
    }

    #[test]
    fn variable() {
        // a

        // b

        // a_1

        // A_1

        // a_a

        // a_A

        // a_(i)

        // a_(5)

        // a_(ab)

        // a_(5b)

        // a_(4b + 8)
    }

    #[test]
    fn implicit_multiplication() {
        verify_parser(
            "2(3)",
            Statement::Simplify(Expression::multiplication(
                Expression::number(2),
                Expression::number(3),
            )),
        );
        verify_parser(
            "2(3+2)",
            Statement::Simplify(Expression::multiplication(
                Expression::number(2),
                Expression::addition(Expression::number(3), Expression::number(2)),
            )),
        );
        verify_parser(
            "2(3+2)3",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::number(2),
                    Expression::addition(Expression::number(3), Expression::number(2)),
                ),
                Expression::number(3),
            )),
        );

        verify_parser(
            "2(3+2)3(2+1)",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::addition(Expression::number(3), Expression::number(2)),
                    ),
                    Expression::number(3),
                ),
                Expression::addition(Expression::number(2), Expression::number(1)),
            )),
        );
    }

    #[test]
    fn fraction() {
        // 2/2c
        verify_parser(
            "2/2c",
            Statement::Simplify(Expression::multiplication(
                Expression::fraction(Expression::number(2), Expression::number(2)),
                Expression::variable("c".to_string()),
            )),
        );

        // a/b
        verify_parser(
            "a/b",
            Statement::Simplify(Expression::fraction(
                Expression::variable("a".to_string()),
                Expression::variable("b".to_string()),
            )),
        );

        // b * 1/a
        verify_parser(
            "b * 1/a",
            Statement::Simplify(Expression::fraction(
                Expression::multiplication(
                    Expression::number(1),
                    Expression::variable("b".to_string()),
                ),
                Expression::variable("a".to_string()),
            )),
        );

        // 2x * 1/(x+x^2)
        verify_parser(
            "2x * 1/(x+x^2)",
            Statement::Simplify(Expression::fraction(
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::variable("x".to_string()),
                    ),
                    Expression::number(1),
                ),
                Expression::addition(
                    Expression::variable("x".to_string()),
                    Expression::exponentiation(
                        Expression::variable("x".to_string()),
                        Expression::number(2),
                    ),
                ),
            )),
        );
    }

    #[test]
    fn variables() {
        verify_parser(
            "x",
            Statement::Simplify(Expression::variable("x".to_string())),
        );
        verify_parser(
            "x+2",
            Statement::Simplify(Expression::addition(
                Expression::variable("x".to_string()),
                Expression::number(2),
            )),
        );
        verify_parser(
            "x+2y",
            Statement::Simplify(Expression::addition(
                Expression::variable("x".to_string()),
                Expression::multiplication(
                    Expression::number(2),
                    Expression::variable("y".to_string()),
                ),
            )),
        );
        verify_parser(
            "x+2y+3",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::variable("x".to_string()),
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::variable("y".to_string()),
                    ),
                ),
                Expression::number(3),
            )),
        );
        verify_parser(
            "x+2y+3z",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::variable("x".to_string()),
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::variable("y".to_string()),
                    ),
                ),
                Expression::multiplication(
                    Expression::number(3),
                    Expression::variable("z".to_string()),
                ),
            )),
        );
    }

    #[test]
    fn function() {
        verify_parser(
            "ln(2)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::number(2)],
            ))),
        );

        verify_parser(
            "ln(2+3)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Ln,
                vec![Expression::addition(
                    Expression::number(2),
                    Expression::number(3),
                )],
            ))),
        );

        // sin(2+3)
        verify_parser(
            "sin(2+3)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Sin,
                vec![Expression::addition(
                    Expression::number(2),
                    Expression::number(3),
                )],
            ))),
        );

        // asin(2+3)
        verify_parser(
            "asin(2+3)",
            Statement::Simplify(Expression::function(FunctionType::Predefined(
                PredefinedFunction::Asin,
                vec![Expression::addition(
                    Expression::number(2),
                    Expression::number(3),
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
                        Expression::number(2),
                    )],
                )),
                Expression::variable("b".to_string()),
            )),
        );

        //a(x)
        verify_parser(
            "a(x)",
            Statement::Simplify(Expression::function(FunctionType::UserDefined(
                "a".to_string(),
                vec![Expression::variable("x".to_string())],
            ))),
        );

        //ab(x, r)
        verify_parser(
            "ab(x, r)",
            Statement::Simplify(Expression::multiplication(
                Expression::function(FunctionType::UserDefined(
                    "b".to_string(),
                    vec![
                        Expression::variable("x".to_string()),
                        Expression::variable("r".to_string()),
                    ],
                )),
                Expression::variable("a".to_string()),
            )),
        );

        //ab(x, r)b(x, t)
        verify_parser(
            "ab(x, r)b(x, t)",
            Statement::Simplify(Expression::multiplication(
                Expression::multiplication(
                    Expression::function(FunctionType::UserDefined(
                        "b".to_string(),
                        vec![
                            Expression::variable("x".to_string()),
                            Expression::variable("r".to_string()),
                        ],
                    )),
                    Expression::variable("a".to_string()),
                ),
                Expression::function(FunctionType::UserDefined(
                    "b".to_string(),
                    vec![
                        Expression::variable("x".to_string()),
                        Expression::variable("t".to_string()),
                    ],
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
                Expression::number(2),
                Expression::number(2),
            )),
        );

        // 2+3= 2
        verify_parser(
            "2+3=2",
            Statement::Solve(Expression::equality(
                Expression::addition(Expression::number(2), Expression::number(3)),
                Expression::number(2),
            )),
        );

        // 2ln(4) = e^(-pi)
        verify_parser(
            "2ln(4) = e^(-pi)",
            Statement::Solve(Expression::equality(
                Expression::multiplication(
                    Expression::number(2),
                    Expression::function(FunctionType::Predefined(
                        PredefinedFunction::Ln,
                        vec![Expression::number(4)],
                    )),
                ),
                Expression::exponentiation(
                    Expression::Constant(ConstantKind::E),
                    Expression::negation(Expression::Constant(ConstantKind::Pi)),
                ),
            )),
        );
    }

    #[test]
    fn complex() {
        // a + bi
        verify_parser(
            "a + bi",
            Statement::Simplify(Expression::addition(
                Expression::variable("a".to_string()),
                Expression::multiplication(
                    Expression::ImaginaryUnit,
                    Expression::variable("b".to_string()),
                ),
            )),
        );

        // a + bi + c
        verify_parser(
            "a + bi + c",
            Statement::Simplify(Expression::addition(
                Expression::addition(
                    Expression::variable("a".to_string()),
                    Expression::multiplication(
                        Expression::ImaginaryUnit,
                        Expression::variable("b".to_string()),
                    ),
                ),
                Expression::variable("c".to_string()),
            )),
        );

        // 2 + 3i
        verify_parser(
            "2 + 3i",
            Statement::Simplify(Expression::addition(
                Expression::number(2),
                Expression::multiplication(Expression::number(3), Expression::ImaginaryUnit),
            )),
        );

        // 2 + 0i
        verify_parser(
            "2 + 0i",
            Statement::Simplify(Expression::addition(
                Expression::number(2),
                Expression::multiplication(Expression::number(0), Expression::ImaginaryUnit),
            )),
        );

        // 4 + i^2
        verify_parser(
            "4 + i^2",
            Statement::Simplify(Expression::addition(
                Expression::number(4),
                Expression::exponentiation(Expression::ImaginaryUnit, Expression::number(2)),
            )),
        );
    }

    #[test]
    fn derivation() {
        // d/dx(x + 1)
        verify_parser(
            "d/dx(x + 1)",
            Statement::Simplify(Expression::derivation(
                Expression::addition(Expression::variable("x".to_string()), Expression::number(1)),
                Variable::new("x".to_string()),
                None,
            )),
        );

        // d/dx(x^2)
        verify_parser(
            "d/dx(x^2)",
            Statement::Simplify(Expression::derivation(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2),
                ),
                Variable::new("x".to_string()),
                None,
            )),
        );

        // d/dx(2x*(y+2))
        verify_parser(
            "d/dx(2x*(y+2))",
            Statement::Simplify(Expression::derivation(
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::variable("x".to_string()),
                    ),
                    Expression::addition(
                        Expression::variable("y".to_string()),
                        Expression::number(2),
                    ),
                ),
                Variable::new("x".to_string()),
                None,
            )),
        );

        // d/dx(2x*(y+2)^2)
        verify_parser(
            "d/dx(2x*(y+2)^2)",
            Statement::Simplify(Expression::derivation(
                Expression::multiplication(
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::variable("x".to_string()),
                    ),
                    Expression::exponentiation(
                        Expression::addition(
                            Expression::variable("y".to_string()),
                            Expression::number(2),
                        ),
                        Expression::number(2),
                    ),
                ),
                Variable::new("x".to_string()),
                None,
            )),
        );

        // d^2/dx^2(x^2)
        verify_parser(
            "d^2/dx^2(x^2)",
            Statement::Simplify(Expression::derivation(
                Expression::exponentiation(
                    Expression::variable("x".to_string()),
                    Expression::number(2),
                ),
                Variable::new("x".to_string()),
                Some(Expression::number(2)),
            )),
        );

        // d^2/dx^2(x^2 + 2x)
        verify_parser(
            "d^2/dx^2(x^2 + 2x)",
            Statement::Simplify(Expression::derivation(
                Expression::addition(
                    Expression::exponentiation(
                        Expression::variable("x".to_string()),
                        Expression::number(2),
                    ),
                    Expression::multiplication(
                        Expression::number(2),
                        Expression::variable("x".to_string()),
                    ),
                ),
                Variable::new("x".to_string()),
                Some(Expression::number(2)),
            )),
        );

        // d/dx^2(x^2)
        verify_parser(
            "d/dx^2(x^2)",
            Statement::Simplify(Expression::multiplication(
                Expression::fraction(
                    Expression::variable(String::from("d")),
                    Expression::multiplication(
                        Expression::variable(String::from("d")),
                        Expression::exponentiation(
                            Expression::variable(String::from("x")),
                            Expression::number(2),
                        ),
                    ),
                ),
                Expression::exponentiation(
                    Expression::variable(String::from("x")),
                    Expression::number(2),
                ),
            )),
        );
    }
}
