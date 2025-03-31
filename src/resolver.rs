use std::collections::HashMap;
use crate::parser::{Expr, Stmt};
use crate::tokenizer::Token;

#[derive(PartialEq, Clone, Debug)]
enum FunctionType {
    None,
    Function,
}

pub struct Resolver {
    // Stack of scopes for variable resolution
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    // Maps expressions to their resolved environment depths
    locals: HashMap<usize, usize>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: Vec::new(),
            current_function: FunctionType::None,
            locals: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for statement in statements {
            self.resolve_stmt(statement)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Block(statements) => {
                self.begin_scope();
                // Resolve all statements sequentially within the new scope
                for statement in statements {
                    self.resolve_stmt(statement)?;
                }
                self.end_scope();
                Ok(())
            },
            Stmt::Class { name, methods } => {
                self.declare(name)?;
                self.define(name);
                // Note: Methods will be resolved later when instances are created
                Ok(())
            },
            Stmt::Expression(expr) => self.resolve_expr(expr),
            Stmt::Function(name, params, body) => {
                // Declare and define the function in the current scope immediately.
                // No need to check if already declared, as hoisting is removed.
                self.declare(name)?;
                self.define(name);
                
                // Resolve the function body in its own scope
                self.resolve_function(params, body, FunctionType::Function)?;
                Ok(())
            },
            Stmt::If(condition, then_branch, else_branch) => {
                self.resolve_expr(condition)?;
                // Use resolve_stmt directly on the Box<Stmt>
                self.resolve_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }
                Ok(())
            },
            Stmt::Print(expr) => self.resolve_expr(expr),
            Stmt::Return(keyword, value) => {
                if self.current_function == FunctionType::None {
                    return Err(format!("Cannot return from top-level code at line {}.", keyword.line));
                }

                if let Some(value) = value {
                    self.resolve_expr(value)?;
                }
                
                Ok(())
            },
            Stmt::Var(name, initializer) => {
                self.declare(name)?;
                if let Some(initializer) = initializer {
                    self.resolve_expr(initializer)?;
                }
                self.define(name);
                Ok(())
            },
            Stmt::While(condition, body) => {
                self.resolve_expr(condition)?;
                // Use resolve_stmt directly on the Box<Stmt>
                self.resolve_stmt(body)?;
                Ok(())
            },
        }
    }

    fn resolve_function(&mut self, params: &[Token], body: &[Stmt], function_type: FunctionType) -> Result<(), String> {
        let enclosing_function = self.current_function.clone();
        self.current_function = function_type;

        self.begin_scope();
        
        // Define parameters in the function's scope
        for param in params {
            self.declare(param)?;
            self.define(param);
        }
        
        // Resolve nested statements sequentially. 
        // If a nested function is found, resolve_stmt will handle it.
        for statement in body {
            self.resolve_stmt(statement)?;
        }
        
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Variable(name) => {
                if !self.scopes.is_empty() {
                    if let Some(scope) = self.scopes.last() {
                        if let Some(false) = scope.get(&name.lexeme) {
                            return Err(format!(
                                "Cannot read local variable '{}' in its own initializer at line {}.",
                                name.lexeme, name.line
                            ));
                        }
                    }
                }
                
                self.resolve_local(expr, name);
                Ok(())
            },
            Expr::Assign(name, value) => {
                self.resolve_expr(value)?;
                self.resolve_local(expr, name);
                Ok(())
            },
            Expr::Binary(left, _operator, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
                Ok(())
            },
            Expr::Call(callee, _paren, arguments) => {
                self.resolve_expr(callee)?;
                
                for argument in arguments {
                    self.resolve_expr(argument)?;
                }
                
                Ok(())
            },
            Expr::Grouping(expr) => self.resolve_expr(expr),
            Expr::Literal(_) => Ok(()),
            Expr::Logical(left, _operator, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
                Ok(())
            },
            Expr::Unary(_operator, right) => self.resolve_expr(right),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<(), String> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name.lexeme) {
            return Err(format!("Variable with name '{}' already declared in this scope at line {}.", name.lexeme, name.line));
        }

        scope.insert(name.lexeme.clone(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        let scope = self.scopes.last_mut().unwrap();
        scope.insert(name.lexeme.clone(), true);
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        // Search from innermost to outermost scope
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                // Found it! Store the distance in the locals map
                let expr_id = self.get_expr_id(expr);
                self.locals.insert(expr_id, i);
                // DEBUG PRINT: Log resolved local variables
                eprintln!("[Resolver] Resolved local: '{}' at distance {} (Expr ID: {})", name.lexeme, i, expr_id);
                return;
            }
        }
        // Not found. Assume it is global.
        // DEBUG PRINT: Log assumed global variables
        eprintln!("[Resolver] Assuming global: '{}' (Expr ID: {})", name.lexeme, self.get_expr_id(expr));
    }

    // Generate a unique ID for an expression based on its memory address
    fn get_expr_id(&self, expr: &Expr) -> usize {
        expr as *const _ as usize
    }

    pub fn get_locals(&self) -> &HashMap<usize, usize> {
        &self.locals
    }
}
