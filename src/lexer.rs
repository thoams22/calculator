use std::fmt::{Display, Formatter};

use crate::diagnostic::Diagnostics;

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

    NewLine,
    End,
    Error,
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
            TokenKind::End => write!(f, "End of file"),
            TokenKind::Error => write!(f, "Error"),
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
    diagnostics: Diagnostics,
}

impl Lexer {
    pub fn new(chars: Vec<char>) -> Self {
        Self {
            chars,
            peeked: None,
            end: false,
            position: 0,
            diagnostics: Diagnostics::new(),
        }
    }

    pub fn get_diagnostic(&mut self) -> Diagnostics {
        self.diagnostics.clone()
    }

    /// Return the current Token if it exist OR Lex it
    pub fn next_token(&mut self) -> Token {
        self.peeked.take().unwrap_or_else(|| self.convert())
    }

    /// Return the char and increase position by 1
    fn eat(&mut self) -> Option<char> {
        self.chars.get(self.position).cloned().map(|ch| {
            self.position += 1;
            ch
        })
    }

    /// if char equals the predicate eat it
    ///
    /// else Nothing
    fn eat_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char> {
        self.chars.get(self.position).cloned().and_then(|ch| {
            if predicate(ch) {
                self.eat()
            } else {
                None
            }
        })
    }

    fn convert(&mut self) -> Token {
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
                        Token {
                            kind: TokenKind::WhiteSpace,
                            span: Span {
                                start,
                                end: self.position,
                            },
                            text,
                        }
                    }
                    '(' => Token {
                        kind: TokenKind::LeftParenthesis,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    ')' => Token {
                        kind: TokenKind::RightParenthesis,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },

                    // '{' => Token {
                    //     kind: TokenKind::LeftCurly,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },
                    // '}' => Token {
                    //     kind: TokenKind::RightCurly,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },

                    // '[' => Token {
                    //     kind: TokenKind::LeftBracket,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },
                    // ']' => Token {
                    //     kind: TokenKind::RightBracket,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },

                    ',' => Token {
                        kind: TokenKind::Comma,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    // ';' => Token {
                    //     kind: TokenKind::Semicolon,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },
                    // ':' => Token {
                    //     kind: TokenKind::Colon,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },
                    '=' => Token {
                        kind: TokenKind::Equal,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    // '_' => Token {
                    //     kind: TokenKind::Underscore,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },

                    '+' => Token {
                        kind: TokenKind::Plus,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    '-' => Token {
                        kind: TokenKind::Minus,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    '*' => Token {
                        kind: TokenKind::Star,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    '/' => Token {
                        kind: TokenKind::Slash,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    // '%' => Token {
                    //     kind: TokenKind::Percent,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },
                    '^' => Token {
                        kind: TokenKind::Hat,
                        text,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    },
                    // '\'' => Token {
                    //     kind: TokenKind::Prime,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },
                    // '\n' => Token {
                    //     kind: TokenKind::NewLine,
                    //     text,
                    //     span: Span {
                    //         start,
                    //         end: self.position,
                    //     },
                    // },
                    // '\r' => {
                    //     if let Some(x) = self.eat_if(|x| x == '\n') {
                    //         text,.push(x)
                    //     }
                    //     Token {
                    //         kind: TokenKind::NewLine,
                    //         text,
                    //         span: Span {
                    //             start,
                    //             end: self.position,
                    //         },
                    //     }
                    // }
                    _ => {
                        if x.is_ascii_alphabetic() {
                            while let Some(x) = self.eat_if(|x| x.is_ascii_alphabetic()) {
                                text.push(x)
                            }

                            Token {
                                kind: TokenKind::Literal,
                                text,
                                span: Span {
                                    start,
                                    end: self.position,
                                },
                            }
                        } else if x.is_ascii_digit() {
                            while let Some(x) = self.eat_if(|x| x.is_ascii_digit() || x == '.') {
                                text.push(x)
                            }

                            Token {
                                kind: TokenKind::Number,
                                text,
                                span: Span {
                                    start,
                                    end: self.position,
                                },
                            }
                        } else {
                            self.diagnostics.report_unknown_char(x);
                            Token {
                                kind: TokenKind::Error,
                                text,
                                span: Span {
                                    start,
                                    end: self.position,
                                },
                            }
                        }
                    }
                }
            }
            None => {
                self.end = true;
                Token {
                    kind: TokenKind::End,
                    text: "".to_string(),
                    span: Span {
                        start,
                        end: self.position,
                    },
                }
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
        assert_eq!(lexer.next_token().kind, TokenKind::Number);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Plus);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Number);
        assert_eq!(lexer.next_token().kind, TokenKind::End);
    }

    #[test]
    fn test_lexer_2() {
        let mut lexer = Lexer::new("1 * 2".chars().collect());
        assert_eq!(lexer.next_token().kind, TokenKind::Number);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Star);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Number);
        assert_eq!(lexer.next_token().kind, TokenKind::End);
    }

    #[test]
    fn test_lexer_3() {
        let mut lexer = Lexer::new("1 / 2".chars().collect());
        assert_eq!(lexer.next_token().kind, TokenKind::Number);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Slash);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Number);
        assert_eq!(lexer.next_token().kind, TokenKind::End);
    }

    #[test]
    fn test_lexer_4() {
        let mut lexer = Lexer::new("ae % vrr ^fs ".chars().collect());
        assert_eq!(lexer.next_token().kind, TokenKind::Literal);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Error);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Literal);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Hat);
        assert_eq!(lexer.next_token().kind, TokenKind::Literal);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::End);
    }

    #[test]
    fn test_lexer_5() {
        let mut lexer = Lexer::new("true & false".chars().collect());
        assert_eq!(lexer.next_token().kind, TokenKind::Literal);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Error);
        assert_eq!(lexer.next_token().kind, TokenKind::WhiteSpace);
        assert_eq!(lexer.next_token().kind, TokenKind::Literal);
        assert_eq!(lexer.next_token().kind, TokenKind::End);
    }

    #[test]
    fn test_lexer_6() {
        let mut lexer = Lexer::new("''fç".chars().collect());
        assert_eq!(lexer.next_token().kind, TokenKind::Error);
        assert_eq!(lexer.next_token().kind, TokenKind::Error);
        assert_eq!(lexer.next_token().kind, TokenKind::Literal);
        assert_eq!(lexer.next_token().kind, TokenKind::Error);
        assert_eq!(lexer.next_token().kind, TokenKind::End);
    }

    #[test]
    fn test_lexer_7() {
        let mut lexer = Lexer::new("34.45;".chars().collect());
        assert_eq!(lexer.next_token().kind, TokenKind::Number);
        assert_eq!(lexer.next_token().kind, TokenKind::Error);
        assert_eq!(lexer.next_token().kind, TokenKind::End);
    }
}