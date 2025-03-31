use std::fmt;
use lazy_static::lazy_static;
use std::collections::HashMap;


pub struct Tokenizer <'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start:usize,
    current: usize,
    line: usize,
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
    Slash,
    String,
    Number,
    Identifier,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF,
    WhiteSpace,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<String>,
    pub line: usize,
}

lazy_static! {

    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap:: new();
        m.insert("and", TokenType::And);
        m.insert("class", TokenType::Class);
        m.insert("else", TokenType::Else);
        m.insert("false", TokenType::False);
        m.insert("for", TokenType::For);
        m.insert("fun", TokenType::Fun);
        m.insert("if", TokenType::If);
        m.insert("nil", TokenType::Nil);
        m.insert("or", TokenType::Or);
        m.insert("print", TokenType::Print);
        m.insert("return", TokenType::Return);
        m.insert("super", TokenType::Super);
        m.insert("this", TokenType::This);
        m.insert("true", TokenType::True);
        m.insert("var", TokenType::Var);
        m.insert("while", TokenType::While);
        m.insert("_", TokenType::Identifier);
        m

    };
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
            TokenType:: Slash => write!(f, "SLASH"),
            TokenType:: String => write!(f, "STRING"),
            TokenType:: Number => write!(f, "NUMBER"),
            TokenType:: Identifier => write!(f, "IDENTIFIER"),
            TokenType:: EOF => write!(f, "EOF"),
            TokenType:: WhiteSpace => write!(f, "WHITESPACE"),
            TokenType::And => write!(f, "AND"),
            TokenType::Class => write!(f, "CLASS"),
            TokenType::Else => write!(f, "ELSE"),
            TokenType::False => write!(f, "FALSE"),
            TokenType::For => write!(f, "FOR"),
            TokenType::Fun => write!(f, "FUN"),
            TokenType::If => write!(f, "IF"),
            TokenType::Nil => write!(f, "NIL"),
            TokenType::Or => write!(f, "OR"),
            TokenType::Print => write!(f, "PRINT"),
            TokenType::Return => write!(f, "RETURN"),
            TokenType::Super => write!(f, "SUPER"),
            TokenType::This => write!(f, "THIS"),
            TokenType::True => write!(f, "TRUE"),
            TokenType::Var => write!(f, "VAR"),
            TokenType::While => write!(f, "WHILE"),
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

    pub fn report_error_string(&mut self, unexpected_string: String) {

        let error_msg = format! (

            "[line {}] Error: Unterminated string.{}\n",
            self.line,
            unexpected_string,
        );

        eprint!("{}", error_msg);
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
            line: self.line,
        });
        
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
            } else {
                self.add_token(TokenType::Bang)
            }
        },
        '=' => {
            if self.match_next('=') {
                self.add_token(TokenType::EqualEqual)
            } else {
                self.add_token(TokenType::Equal)
            }
        },
        '<' => {
            if self.match_next('=') {
                self.add_token(TokenType::LessEqual)
            } else {
                self.add_token(TokenType::Less)
            }
        },
        '>' => {
            if self.match_next('=') {
                self.add_token(TokenType::GreaterEqual)
            } else {
                self.add_token(TokenType::Greater)
            }
        },
        '/' => {
            if self.match_next('/') {
                while self.peek() != '\n' && !self.is_at_end() {
                    self.advance();
                }
            } else {
                self.add_token(TokenType::Slash);
            }
        },
        '"' => {
            self.string();
        }
        ' ' | '\r' | '\t' => {}, // Ignore whitespace
        '\n' => {
            self.line += 1;
        },
        _ => {

            if c.is_ascii_digit() {
                self.number();
            }
            else if self.is_alpha(c) {
                self.identifier();
            }
            else{
               self.report_error(c); 
            }
            
        }
    }
}

    fn identifier(&mut self) {

        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }
    
        let text = &self.source[self.start..self.current];
        let token_type = match text { 
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            "_" => TokenType::Identifier,
            _ => TokenType::Identifier,
        };
        self.add_token(token_type);
    }
    

    fn is_alpha(&self, c: char) -> bool {

        c.is_alphabetic() || c == '_'

    }
    
    
    fn is_alpha_numeric(&self, c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.report_error_string("".to_string());
            return;
        }

        // Consume the closing quote
        self.advance();

        // Extract the string literal using byte indices
        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token_with_literal(TokenType::String, Some(value.to_string()));
    }
    
    
    fn number(&mut self) {

        while self.peek().is_digit(10) {

            self.advance();
        }

        // Look for a fractional part.

        if self.peek() == '.' && self.peek_next().is_digit(10) {

            // Consume the "."

            self.advance();

            while self.peek().is_digit(10) {

                self.advance();

            }

        }

        let value: f64 = self.source[self.start..self.current].parse().unwrap_or(0.0);
        self.add_token_with_literal(TokenType::Number, Some(format!("{:?}", value)));

    }

    
    fn add_token_with_literal(&mut self, token_type: TokenType, literal: Option<String>) {

        let lexeme = &self.source[self.start..self.current];

        self.tokens.push(Token {

            token_type,

            lexeme: lexeme.to_string(),

            literal,

            line: self.line,

        })

    }
    

 
    fn peek(&self) -> char {
        self.source[self.current..]
            .chars()
            .next()
            .unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        let mut iter = self.source[self.current..].chars();
        iter.next();
        iter.next().unwrap_or('\0')
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
        if self.is_at_end() {
            return '\0';
        }

        let current_char = self.peek();
        self.current += current_char.len_utf8();
        current_char
    }


    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token {
            token_type,
            lexeme: text.to_string(),
            literal: None,
            line: self.line,
        })
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}