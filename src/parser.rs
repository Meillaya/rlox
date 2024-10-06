use crate::tokenizer::{Token, TokenType};

#[derive(Debug)]
pub enum Expr {
    Literal(LiteralValue),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
}

#[derive(Debug)]
pub enum LiteralValue {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {

    pub fn new(tokens: Vec<Token>)-> Self {
        Parser {tokens, current: 0}
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;

        while self.match_token(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.addition()?;

        while self.match_token(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual]) {
            let operator = self.previous().clone();
            let right = self.addition()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, String> {
        let mut expr = self.multiplication()?;

        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Star, TokenType::Slash]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {

        if self.match_token(&[TokenType::Minus, TokenType::Bang]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression")?;
            Ok(Expr::Grouping(Box::new(expr)))
        } else {
            self.literal()
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(message.to_string())
        }
    }

    fn literal(&mut self) -> Result<Expr, String> {
        if self.match_token(&[TokenType::False]) {
            Ok(Expr::Literal(LiteralValue::Boolean(false)))
        } else if self.match_token(&[TokenType::True]) {
            Ok(Expr::Literal(LiteralValue::Boolean(true)))
        } else if self.match_token(&[TokenType::Nil]) {
            Ok(Expr::Literal(LiteralValue::Nil))
        } else if self.match_token(&[TokenType::Number]) {
            let value =  self.previous().literal.as_ref()
                .and_then(|s| s.parse::<f64>().ok())
                .ok_or_else(|| "Invalid number literal".to_string())?;
            Ok(Expr::Literal(LiteralValue::Number(value)))
        } else if self.match_token(&[TokenType::String]) {
            let value =  self.previous().literal.clone()
                .ok_or_else(|| "Invalid number literal".to_string())?;
            Ok(Expr::Literal(LiteralValue::String(value)))
        }
        
         else {
            Err("Expected literal".to_string())
        }
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(t.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, t: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == t
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}


pub fn print_ast(expr: &Expr) -> String {
    match expr {
        Expr::Literal(value) => match value {
            LiteralValue::Nil => "nil".to_string(),
            LiteralValue::Boolean(b) => b.to_string(),
            LiteralValue::Number(n) => format!("{:?}", n),
            LiteralValue::String(s) => format!("{}", s),
        },

        Expr::Grouping(expr) => format!("(group {})", print_ast(expr)),
        Expr::Unary(operator, expr) =>
            format!("({} {})", operator.lexeme, print_ast(expr)),
        Expr::Binary(left, operator, right) =>
            format!("({} {} {})", operator.lexeme, print_ast(left), print_ast(right)),
    }
}