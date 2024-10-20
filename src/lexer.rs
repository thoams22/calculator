use std::fmt::{Display, Formatter};

use anyhow::anyhow;

#[derive(Debug, thiserror::Error, Clone)]
pub enum LexerError {
    #[error("Unknown char: '{0}'")]
    UnknwonChar(char),
}

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub enum TokenKind {
    Literal,
    Number,
    WhiteSpace,

    LeftParenthesis,
    RightParenthesis,
    LeftCurly,
    RightCurly,
    LeftBracket,
    RightBracket,

    Comma,
    Semicolon,
    Colon,
    Underscore,
    BackSlash,
    
    Plus,
    Minus,
    Star,
    Slash,
    Hat,
    Equal,
    Percent,
    Prime,
    Dot,

    NewLine,
    End,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Literal => write!(f, "Literal"),
            TokenKind::Number => write!(f, "Number"),
            TokenKind::WhiteSpace => write!(f, "WhiteSpace"),
            TokenKind::LeftParenthesis => write!(f, "("),
            TokenKind::RightParenthesis => write!(f, ")"),
            TokenKind::LeftCurly => write!(f, "{{"),
            TokenKind::RightCurly => write!(f, "}}"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::Underscore => write!(f, "_"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Hat => write!(f, "^"),
            TokenKind::Prime => write!(f, "'"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::End => write!(f, "End of file"),
            TokenKind::NewLine => write!(f, "NewLine"),
            TokenKind::BackSlash => write!(f, "\\"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn lenght(&mut self) -> usize {
        self.end - self.start
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub text: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[Kind: {}, Text: {:?}]", self.kind, self.text)
    }
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, text: String) -> Self {
        Self { kind, span, text }
    }
}

pub struct Lexer {
    chars: Vec<char>,
    peeked: Option<Token>,
    end: bool,
    position: usize,
}

impl Lexer {
    pub fn new(chars: Vec<char>) -> Self {
        Self {
            chars,
            peeked: None,
            end: false,
            position: 0,
        }
    }

    /// Return the current Token if it exist else Lex the next one
    pub fn next_token(&mut self) -> Result<Token, anyhow::Error> {
        if let Some(token) = self.peeked.take() {
            Ok(token)
        } else {
            self.convert()
        }
    }

    fn eat(&mut self) -> Option<char> {
        self.chars.get(self.position).cloned().map(|ch| {
            self.position += 1;
            ch
        })
    }

    /// Eat if predicated
    fn eat_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char> {
        self.chars.get(self.position).cloned().and_then(|ch| {
            if predicate(ch) {
                self.eat()
            } else {
                None
            }
        })
    }

    /// Convert the char(s) that beigin in self.position into `Token` 
    /// 
    /// Can return an `LexerError`
    fn convert(&mut self) -> Result<Token, anyhow::Error> {
        if self.end {
            panic!("Lexer ended");
        }
        let start: usize = self.position;
        match self.eat() {
            Some(x) => {
                let mut text = x.to_string();
                match x {
                    ' ' => {
                        while let Some(x) = self.eat_if(|x: char| x.is_whitespace()) {
                            text.push(x)
                        }
                        Ok(Token {
                            kind: TokenKind::WhiteSpace,
                            span: Span {
                                start,
                                end: self.position,
                            },
                            text,
                        })
                    }
                    '(' => Ok(Token {
                        kind: TokenKind::LeftParenthesis,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    ')' => Ok(Token {
                        kind: TokenKind::RightParenthesis,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),

                    '{' => Ok(Token {
                        kind: TokenKind::LeftCurly,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '}' => Ok(Token {
                        kind: TokenKind::RightCurly,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '[' => Ok(Token {
                        kind: TokenKind::LeftBracket,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    ']' => Ok(Token {
                        kind: TokenKind::RightBracket,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    ',' => Ok(Token {
                        kind: TokenKind::Comma,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    ';' => Ok(Token {
                        kind: TokenKind::Semicolon,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    ':' => Ok(Token {
                        kind: TokenKind::Colon,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '=' => Ok(Token {
                        kind: TokenKind::Equal,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '_' => Ok(Token {
                        kind: TokenKind::Underscore,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '\\' => Ok(Token {
                        kind: TokenKind::BackSlash,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '+' => Ok(Token {
                        kind: TokenKind::Plus,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '-' => Ok(Token {
                        kind: TokenKind::Minus,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '*' => Ok(Token {
                        kind: TokenKind::Star,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '/' => Ok(Token {
                        kind: TokenKind::Slash,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '%' => Ok(Token {
                        kind: TokenKind::Percent,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '\'' => Ok(Token {
                        kind: TokenKind::Prime,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '.' => Ok(Token {
                        kind: TokenKind::Dot,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '^' => Ok(Token {
                        kind: TokenKind::Hat,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '\n' => Ok(Token {
                        kind: TokenKind::NewLine,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                    '\r' => {
                        if let Some(x) = self.eat_if(|x| x == '\n') {
                            text.push(x)
                        }
                        Ok(Token {
                            kind: TokenKind::NewLine,
                            text,
                            span: Span {
                                start,
                                end: self.position,
                            },
                        })
                    }
                    _ => {
                        if x.is_ascii_alphabetic() {
                            while let Some(x) = self.eat_if(|x| x.is_ascii_alphabetic()) {
                                text.push(x)
                            }

                            Ok(Token {
                                kind: TokenKind::Literal,
                                text,
                                span: Span {
                                    start,
                                    end: self.position,
                                },
                            })
                        } else if x.is_ascii_digit() {
                            // 45
                            while let Some(x) = self.eat_if(|x| x.is_ascii_digit()) {
                                text.push(x)
                            }
                            // 45.
                            if x == '.' {
                                text.push(x)
                            }
                            // 45.67
                            while let Some(x) = self.eat_if(|x| x.is_ascii_digit()) {
                                text.push(x)
                            }
                            

                            Ok(Token {
                                kind: TokenKind::Number,
                                text,
                                span: Span {
                                    start,
                                    end: self.position,
                                },
                            })
                        } else {
                            Err(
                                anyhow!(LexerError::UnknwonChar(x))
                            )
                        }
                    }
                }
            }
            None => {
                self.end = true;
                Ok(Token {
                    kind: TokenKind::End,
                    text: "".to_string(),
                    span: Span {
                        start,
                        end: self.position,
                    },
                })
            }
        }
    }
}


#[cfg(test)]
mod test_lexer {
    use super::*;

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new("1 + 2".chars().collect());
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Number));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Plus));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Number));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::End));
    }
    
    #[test]
    fn test_lexer_2() {
        let mut lexer = Lexer::new("1 * 2".chars().collect());
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Number));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Star));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Number));    
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::End));
    }

    #[test]
    fn test_lexer_3() {
        let mut lexer = Lexer::new("1 / 2".chars().collect());
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Number));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Slash));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Number));    
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::End));
    }

    #[test]
    fn test_lexer_4() {
        let mut lexer = Lexer::new("ae % vrr ^fs ".chars().collect());
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Literal));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_lexer_5() {
        let mut lexer = Lexer::new("true & false".chars().collect());
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Literal));
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::WhiteSpace));
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_lexer_6() {
        let mut lexer = Lexer::new("''fç".chars().collect());
        assert!(lexer.next_token().is_err());
    }

    #[test]
    fn test_lexer_7() {
        let mut lexer = Lexer::new("34.45;".chars().collect());
        assert!(lexer.next_token().is_ok_and(|token| token.kind == TokenKind::Number));
        assert!(lexer.next_token().is_err());
    }
}