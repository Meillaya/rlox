use std::str::Chars;
use std::fmt;

pub struct Tokenizer <'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start:usize,
    current: usize,
    line: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    EOF,
    WhiteSpace,
}


#[derive(Debug, Clone)]
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
            TokenType:: EOF => write!(f, "EOF"),
            TokenType:: WhiteSpace => write!(f, "WHITESPACE"),
        }
        
    }
}
impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: String::from(""),
            literal: None,
        });
        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            // Add other cases here for different token types
            ' ' | '\r' | '\t' => (), // Ignore whitespace
            '\n' => self.line += 1,
            _ => {
                // Handle unknown characters or other token types
            }
        }
    }

    fn advance(&mut self) -> char {
        let current_char = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        current_char
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token {
            token_type,
            lexeme: text.to_string(),
            literal: None,
        })
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}