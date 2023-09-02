#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub enum TokenKind {
    Var,
    Number,

    LeftParenthesis,
    RightParenthesis,
    LeftCurly,
    RightCurly,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
    Colon,
    Equal,
    Underscore,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Hat,
    Prime,

    End,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
}

pub struct Lexer {
    chars: Vec<char>,
    peeked: Option<Token>,
    end: bool,
    position: usize,
    diagnostics: Vec<String>,
}

impl Lexer {
    pub fn new(chars: Vec<char>) -> Self {
        Self {
            chars,
            peeked: None,
            end: false,
            position: 0,
            diagnostics: Vec::new(),
        }
    }

    pub fn get_diagnostics(&mut self) -> Vec<String> {
        self.diagnostics.clone()
    }

    /// Return the current Token
    pub fn current_token(&mut self) -> &Token {
        let token = self.next_token();
        self.peeked.insert(token)
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

        while let Some(x) = self.chars.get(self.position) {
            if x.is_whitespace() {
                self.eat();
            } else {
                break;
            }
        }

        match self.eat() {
            Some(x) => {
                let mut text = x.to_string();
                match x {
                    '(' => Token {
                        kind: TokenKind::LeftParenthesis,
                        text: text,
                    },
                    ')' => Token {
                        kind: TokenKind::RightParenthesis,
                        text: text,
                    },

                    '{' => Token {
                        kind: TokenKind::LeftCurly,
                        text: text,
                    },
                    '}' => Token {
                        kind: TokenKind::RightCurly,
                        text: text,
                    },

                    '[' => Token {
                        kind: TokenKind::LeftBracket,
                        text: text,
                    },
                    ']' => Token {
                        kind: TokenKind::RightBracket,
                        text: text,
                    },

                    ',' => Token {
                        kind: TokenKind::Comma,
                        text: text,
                    },
                    ';' => Token {
                        kind: TokenKind::Semicolon,
                        text: text,
                    },
                    ':' => Token {
                        kind: TokenKind::Colon,
                        text: text,
                    },
                    '=' => Token {
                        kind: TokenKind::Equal,
                        text: text,
                    },
                    '_' => Token {
                        kind: TokenKind::Underscore,
                        text: text,
                    },

                    '+' => Token {
                        kind: TokenKind::Plus,
                        text: text,
                    },
                    '-' => Token {
                        kind: TokenKind::Minus,
                        text: text,
                    },
                    '*' => Token {
                        kind: TokenKind::Star,
                        text: text,
                    },
                    '/' => Token {
                        kind: TokenKind::Slash,
                        text: text,
                    },
                    '%' => Token {
                        kind: TokenKind::Percent,
                        text: text,
                    },
                    '^' => Token {
                        kind: TokenKind::Hat,
                        text: text,
                    },
                    '\'' => Token {
                        kind: TokenKind::Prime,
                        text: text,
                    },

                    _ => {
                        if x.is_ascii_alphabetic() {
                            while let Some(x) = self.eat_if(|x| x.is_ascii_alphabetic()) {
                                text.push(x)
                            }

                            Token {
                                kind: TokenKind::Var,
                                text: text,
                            }
                        } else if x.is_ascii_digit() {
                            while let Some(x) = self.eat_if(|x| x.is_ascii_digit() || x == '.') {
                                text.push(x)
                            }

                            Token {
                                kind: TokenKind::Number,
                                text: text,
                            }
                        } else {
                            self.diagnostics.push(format!("ERROR LEXER: char not supported '{text}' position ({})", self.position));
                            Token {
                                kind: TokenKind::Error,
                                text: text,
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
                }
            }
        }
    }
}