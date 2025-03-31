use crate::tokenizer::{Token, TokenType};

#[derive(Debug, PartialEq, Clone )]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Function(Token, Vec<Token>, Vec<Stmt>),
    Return(Token, Option<Expr>),
    Class { name: Token, superclass: Option<Expr>, methods: Vec<Stmt> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(LiteralValue),
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Variable(Token),
    Assign(Token, Box<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Get(Box<Expr>, Token),
    Set(Box<Expr>, Token, Box<Expr>),
    This(Token),
    Super(Token, Token),
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.message)
    }
}

impl std::error::Error for ParseError {}

impl Parser {

    fn call(&mut self, expr: Expr) -> Result<Expr, String> {
        let mut expr = expr;
        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn return_statement(&mut self) -> Result<Stmt, String> {
        let keyword = self.previous().clone();
        let value = if !self.check(TokenType::SemiColon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::SemiColon, "Expect ';' after return value.")?;
        Ok(Stmt::Return(keyword, value))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_stmt()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, &format!("Expect {} name.", kind))?.clone();
        self.consume(TokenType::LeftParen, &format!("Expect '(' after {} name.", kind))?;
        
        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                parameters.push(self.consume(TokenType::Identifier, "Expect parameter name.")?.clone());
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        
        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::LeftBrace, &format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;
        
        Ok(Stmt::Function(name, parameters, body))
    }

    fn var_declaration(&mut self) -> Result<Stmt, String> {
        
        let name = self
            .consume(TokenType::Identifier, "Expect variable name.")?
            .clone();
    
        let initializer = if self.match_token(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };
    
        self.consume(TokenType::SemiColon, "Expect ';' after variable declaration.")?;
        Ok(Stmt::Var(name, initializer))
    }
    

    
    pub fn new(tokens: Vec<Token>)-> Self {
        Parser {tokens, current: 0}
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_stmt()?);
        }

        Ok(statements)
    }
    
    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        if self.match_token(&[TokenType::Return]) {
            return self.return_statement();
        }
        if self.match_token(&[TokenType::Class]) {
            return self.class_declaration();
        }
        if self.match_token(&[TokenType::Fun]) {
            return self.function("function");
        }
        if self.match_token(&[TokenType::For]) {
            return self.for_statement();
        }
        if self.match_token(&[TokenType::While]) {
            return self.while_statement();
        }
        if self.match_token(&[TokenType::If]) {
            return self.if_statement();
        }
        if self.match_token(&[TokenType::LeftBrace]) {
            return Ok(Stmt::Block(self.block()?));
        }
        if self.match_token(&[TokenType::Var]) {
            return self.var_declaration();
        }
        if self.match_token(&[TokenType::Print]) {
            return self.print_statement();
        }
        
        self.expression_stmt()
    }
    
    fn for_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
    
        // Handle initializer
        let initializer = if self.match_token(&[TokenType::SemiColon]) {
            None
        } else if self.match_token(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_stmt()?)
        };
    
        // Handle condition
        let condition = if !self.check(TokenType::SemiColon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::SemiColon, "Expect ';' after loop condition.")?;
    
        // Handle increment
        let increment = if !self.check(TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;
    
        // Validate that the body is a valid statement
        if self.check(TokenType::Var) {
            return Err("Variable declaration cannot be used as loop body".to_string());
        }
    
        let body = self.parse_stmt()?;
    
        // Desugar for loop into while loop
        let mut result = body;
        if let Some(inc) = increment {
            result = Stmt::Block(vec![result, Stmt::Expression(inc)]);
        }
    
        let cond = condition.unwrap_or(Expr::Literal(LiteralValue::Boolean(true)));
        result = Stmt::While(cond, Box::new(result));
    
        if let Some(init) = initializer {
            result = Stmt::Block(vec![init, result]);
        }
    
        Ok(result)
    }
    
    fn while_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let body = Box::new(self.parse_stmt()?);
        Ok(Stmt::While(condition, body))
    }
    
    fn print_statement(&mut self) -> Result<Stmt, String> {
        let value = self.expression()?;
        self.consume(TokenType::SemiColon, "Expected ';' after value")?;
        Ok(Stmt::Print(value))
    }
    
    
    fn if_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.parse_stmt()?);
        let else_branch = if self.match_token(&[TokenType::Else]) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn expression_stmt(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        if !self.is_at_end() {
            self.consume(TokenType::SemiColon, "Expected ';' after expression")?;
        }
        
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }
    
    
    fn assignment(&mut self) -> Result<Expr, String> {
        let expr = self.or()?;
    
        if self.match_token(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;
    
            match expr {
                Expr::Variable(name) => {
                    return Ok(Expr::Assign(name, Box::new(value)));
                },
                Expr::Get(object, name) => {
                    return Ok(Expr::Set(object, name, Box::new(value)));
                },
                _ => {
                    return Err(format!("Invalid assignment target at line {}", equals.line));
                }
            }
        }
    
        Ok(expr)
    }
    
    fn or(&mut self) -> Result<Expr, String> {
        let mut expr = self.and()?;

        while self.match_token(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }
    
    fn and(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;

        while self.match_token(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
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
            self.call_or_get()
        }
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_token(&[TokenType::False]) {
            Ok(Expr::Literal(LiteralValue::Boolean(false)))
        } else if self.match_token(&[TokenType::True]) {
            Ok(Expr::Literal(LiteralValue::Boolean(true)))
        } else if self.match_token(&[TokenType::Nil]) {
            Ok(Expr::Literal(LiteralValue::Nil))
        } else if self.match_token(&[TokenType::Number]) {
            let value = self.previous().literal.as_ref()
                .and_then(|s| s.parse::<f64>().ok())
                .ok_or_else(|| "Invalid number literal".to_string())?;
            Ok(Expr::Literal(LiteralValue::Number(value)))
        } else if self.match_token(&[TokenType::String]) {
            let value = self.previous().literal.clone()
                .ok_or_else(|| "Invalid string literal".to_string())?;
            Ok(Expr::Literal(LiteralValue::String(value)))
        } else if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression")?;
            Ok(Expr::Grouping(Box::new(expr)))
        } else if self.match_token(&[TokenType::Identifier]) {
            Ok(Expr::Variable(self.previous().clone()))
        } else if self.match_token(&[TokenType::This]) {
            Ok(Expr::This(self.previous().clone()))
        } else if self.match_token(&[TokenType::Super]) {
            let keyword = self.previous().clone();
            self.consume(TokenType::Dot, "Expect '.' after 'super'.")?;
            let method = self.consume(TokenType::Identifier, "Expect superclass method name.")?;
            Ok(Expr::Super(keyword, method.clone()))
        } else {
            let token = self.peek();
            Err(format!("Expected expression, found token type {:?} at line {}", token.token_type, token.line))
        }
    }

    fn call_or_get(&mut self) -> Result<Expr, String> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_token(&[TokenType::Dot]) {
                let name = self.consume(TokenType::Identifier, "Expect property name after '.'.")?.clone();
                expr = Expr::Get(Box::new(expr), name);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut arguments = Vec::new();
    
        if !self.check(TokenType::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
    
        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(Expr::Call(Box::new(callee), paren.clone(), arguments))
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(message.to_string())
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

    fn class_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect class name.")?.clone();

        // Check for superclass
        let superclass = if self.match_token(&[TokenType::Less]) {
            // Parse superclass name after '<'
            let superclass_name = self.consume(TokenType::Identifier, "Expect superclass name.").map(|token| token.clone())?;
            Some(Expr::Variable(superclass_name))
        } else {
            None
        };

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

        let mut methods = Vec::new();
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;

        Ok(Stmt::Class { name, superclass, methods })
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
        Expr::Variable(token) => token.lexeme.clone(),
        Expr::Assign(token, expr) => format!("({} = {})", token.lexeme, print_ast(expr)),
        Expr::Logical(expr, token, expr1) => 
            format!("({} {} {})", print_ast(expr), token.lexeme, print_ast(expr1)),
        Expr::Call(callee, _paren, arguments) => {
            let mut result = format!("(call {})", print_ast(callee));
            for arg in arguments {
                result.push_str(&format!(" {}", print_ast(arg)));
            }
            result
        },
        Expr::Get(object, name) => format!("(get {} .{})", print_ast(object), name.lexeme),
        Expr::Set(object, name, value) => format!("(set {} .{} {})", print_ast(object), name.lexeme, print_ast(value)),
        Expr::This(keyword) => keyword.lexeme.clone(),
        Expr::Super(keyword, method) => format!("(super {} .{})", keyword.lexeme, method.lexeme),
    }
}