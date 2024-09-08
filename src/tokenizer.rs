use std::fmt;

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
            TokenType:: Slash => write!(f, "SLASH"),
            TokenType:: String => write!(f, "STRING"),
            TokenType:: Number => write!(f, "NUMBER"),
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
            '/' => {
                if self.match_next('/') {

                    while self.peek() != '\n' && !self.is_at_end() {

                        self.advance();
                    }
                }
                else {
                    self.add_token(TokenType::Slash);
                }
                
            },
            '"' => {
                self.string();
            }
            ' ' | '\r' | '\t' => self.add_token(TokenType::WhiteSpace), // Ignore whitespace
            '\n' => {
                self.line += 1;
                self.add_token(TokenType::WhiteSpace);
            }
            _ => {

                if self.is_digit(c) {

                    self.is_number();
                }
                else {
                    self.report_error(c);
                }
                
            }
        }
    }


    fn is_digit(&self, c: char) -> bool {

        // c.is_ascii_digit()

        return c >= '0' && c <= '9';
    }

    fn is_number(&mut self) {
        
        let start = self.current - 1;

        let mut has_decimal = false;

    
        while self.is_digit(self.peek()) {
            self.advance();
        }
        // Look for fractional part
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            has_decimal = true;
            // Consume the "."
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let number_str = &self.source[self.start..self.current];

        let number: f64 = number_str.parse().unwrap_or(0.0);

        let formatted_number = if has_decimal {
            number.to_string()
        }
        else {

            format!("{}.0", number)
        };


        self.add_token_with_literal(TokenType::Number, Some(formatted_number.clone()));


    }

    fn string(&mut self){

        let start_quote = self.current - 1;

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

        self.advance();
        
        let string_content = &self.source[start_quote + 1..self.current - 1];

        self.add_token_with_literal(TokenType:: String, Some(string_content.to_string()));
      


    }

    fn add_token_with_literal(&mut self, token_type: TokenType, literal: Option<String>){

        let text = &self.source[self.start..self.current];

        self.tokens.push(Token {
            token_type,
            lexeme: text.to_string(),
            literal,
        })
    }

 
    fn peek(&self) -> char{

       self.source.chars().nth(self.current).unwrap_or('\0')
    }

    pub fn peek_next(&self) -> char {

        if self.current + 1 >= self.source.len() {

            return '\0';
        }

        return self.source.chars().nth(self.current + 1).unwrap_or('\0');
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
        let current_char = self.source.chars().nth(self.current).unwrap_or('\0');
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