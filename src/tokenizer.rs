use std::{intrinsics::mir::PtrMetadata, str::Chars};


pub struct Tokenizer <'a> {
    source: &'a str,
    chars: Chars<'a>,
    current: Option<char>,
}

pub enum TokenType {
    LeftParen,
    RightParen,
    EOF,
    WhiteSpace,
}

pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<String>,
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
                        token_type: TokenType:: LeftParen,
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