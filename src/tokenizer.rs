use std::str::Chars;
use std::fmt;

pub struct Tokenizer <'a> {
    source: &'a str,
    chars: Chars<'a>,
    current: Option<char>,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    EOF,
    WhiteSpace,
}


#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<String>,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", 
            self.token_type, 
            self.lexeme, 
            self.literal.as_deref().unwrap_or("null")
        )
    }
}

impl fmt:: Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        match self {
            TokenType:: LeftParen => write!(f, "LEFT_PAREN"),
            TokenType:: RightParen => write!(f, "RIGHT_PAREN"),
            TokenType:: EOF => write!(f, "EOF_PAREN"),
            TokenType:: WhiteSpace => write!(f, "WHITESPACE"),
        }
        
    }
}
impl<'a> Tokenizer<'a> {

    pub fn new(source: &'a str) -> Self {
        
        let mut chars = source.chars();
        let current = chars.next();
        Tokenizer {
            source,
            chars,
            current,
        }

    }

    fn advance (&mut self) {

        self.current = self.chars.next();

    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {

        let mut tokens = Vec::new();
        
        while let Some(c) = self.current {

            match c {

                '(' => {
                    tokens.push(Token {
                        token_type: TokenType:: LeftParen,
                        lexeme: "(".to_string(),
                        literal: None,
                    });
                    self.advance();
                }
                ')' => {
                    tokens.push(Token {
                        token_type: TokenType:: RightParen,
                        lexeme: ")".to_string(),
                        literal: None,
                    });
                }
                ' ' | '\t' | '\r' | '\n' => {
                    let mut lexeme = String::new();
                    while let Some(ch) = self.current {
                        if ch.is_whitespace() {
                            lexeme.push(ch);
                            self.advance();
                        }
                        else {
                            break;
                        }
                    }
                    tokens.push(Token {
                        token_type: TokenType::WhiteSpace,
                        lexeme,
                        literal: None,

                    });

                }
                _ => {
                    self.advance();
                }
            }
        }
        tokens
    }
}