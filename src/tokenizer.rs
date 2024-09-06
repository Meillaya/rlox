use std::fmt;
use std::io::{self, Write};

pub struct Tokenizer <'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start:usize,
    current: usize,
    line: usize,
    errors: Vec<String>,
    pub has_error: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Comma,
    Dot,
    Plus,
    Minus,
    SemiColon,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
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
            TokenType:: LeftBrace => write!(f, "LEFT_BRACE"),
            TokenType:: RightBrace => write!(f, "RIGHT_BRACE"),
            TokenType:: Star => write!(f, "STAR"),
            TokenType:: Comma => write!(f, "COMMA"),
            TokenType:: Dot => write!(f, "DOT"),
            TokenType:: Plus => write!(f, "PLUS"),
            TokenType:: Minus => write!(f, "MINUS"),
            TokenType:: SemiColon => write!(f, "SEMICOLON"),
            TokenType:: Equal => write!(f, "EQUAL"),
            TokenType:: EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenType:: Bang => write!(f, "BANG"),
            TokenType:: BangEqual => write!(f, "BANG_EQUAL"),
            TokenType:: Greater => write!(f, "GREATER"),
            TokenType:: GreaterEqual => write!(f, "GREATER_EQUAL"),
            TokenType:: Less => write!(f, "LESS"),
            TokenType:: LessEqual => write!(f, "LESS_EQUAL"),
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
            errors: Vec::new(),
            has_error: false,

        }
    }


    pub fn report_error(&mut self, unexpected_char: char) {

        let error_msg = format! (

            "[line {}] Error: Unexpected character: {}\n",
            self.line,
            unexpected_char,
        );

        eprint!("{}", error_msg);
        // writeln!(io::stderr(), "{}", error_msg).unwrap();
        self.has_error = true;



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
        
        // if self.has_error {
        //     for error in &self.errors {
        //         eprint!("{}", error);
        //     }
        // }
        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '*' => self.add_token(TokenType::Star),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::SemiColon),
            '!' => {
                    if self.match_next('=') {
                        self.add_token(TokenType::BangEqual)
                    }
                    else {
                        self.add_token(TokenType::Bang)
                    }
                },
            '=' => {
                    if self.match_next('=') {
                        self.add_token(TokenType::EqualEqual)
                    }
                    else {
                        self.add_token(TokenType::Equal)
                    }
            },
            '<' => {
                if self.match_next('=') {
                    self.add_token(TokenType::LessEqual)
                }
                else {
                    self.add_token(TokenType::Less)
                }

            },
            '>' => {
                if self.match_next('=') {
                    self.add_token(TokenType::GreaterEqual)
                }
                else {
                    self.add_token(TokenType::Greater)
                }

            },
            ' ' | '\r' | '\t' => self.add_token(TokenType::WhiteSpace), // Ignore whitespace
            '\n' => {
                self.line += 1;
                self.add_token(TokenType::WhiteSpace);
            }
            _ => {
                self.report_error(c);
            }
        }
    }

    fn match_next(&mut self, expected: char) -> bool {

        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {

            return false;
        }

        self.current += 1;
        true
        
    

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