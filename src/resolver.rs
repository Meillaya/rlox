use std::collections::HashMap;
use crate::parser::{Expr, Stmt};
use crate::tokenizer::Token;

#[derive(PartialEq, Clone, Copy, Debug)]
enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum ClassType {
    None,
    Class,
    Subclass,
}

pub struct Resolver {
    // Stack of scopes for variable resolution
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
    // Maps expressions to their resolved environment depths
    locals: HashMap<usize, usize>,
    // Store super expressions separately by line number to avoid memory address issues
    super_expressions: HashMap<usize, usize>, // Line number -> distance
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
            locals: HashMap::new(),
            super_expressions: HashMap::new(),
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
            Stmt::Class { name, superclass, methods } => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;
                
                eprintln!("Resolver: Resolving class {} with superclass {:?}", name.lexeme, superclass);

                self.declare(name)?;
                self.define(name);

                // Handle superclass
                if let Some(superclass_expr) = superclass {
                    if let Expr::Variable(ref superclass_name) = superclass_expr {
                        if superclass_name.lexeme == name.lexeme {
                            return Err(format!("A class cannot inherit from itself at line {}.", superclass_name.line));
                        }
                    }

                    // Resolve the superclass expression
                    self.resolve_expr(superclass_expr)?;
                    self.current_class = ClassType::Subclass;
                    
                    // Create a new scope for 'super'
                    self.begin_scope();
                    eprintln!("Resolver: Adding 'super' to scope for class {}", name.lexeme);
                    self.scopes.last_mut().unwrap().insert("super".to_string(), true);
                }

                self.begin_scope(); // Scope for methods and 'this'
                // Define 'this' within the class scope
                eprintln!("Resolver: Adding 'this' to scope for class {}", name.lexeme);
                self.scopes.last_mut().unwrap().insert("this".to_string(), true);

                for method in methods {
                    // Resolve each method within the class scope
                    if let Stmt::Function(method_name, params, body) = method {
                         // Check if this is an initializer
                         let declaration = if method_name.lexeme == "init" {
                             FunctionType::Initializer
                         } else {
                             FunctionType::Method
                         };
                         eprintln!("Resolver: Resolving method {} in class {}", method_name.lexeme, name.lexeme);
                         self.resolve_function(params, body, declaration)?;
                    } else {
                         // Should not happen if parser is correct
                         return Err(format!("Invalid statement in class body at line {}", name.line));
                    }
                }

                self.end_scope(); // End class scope
                
                // End superclass scope if we have one
                if superclass.is_some() {
                    eprintln!("Resolver: Ending superclass scope for {}", name.lexeme);
                    self.end_scope();
                }
                
                self.current_class = enclosing_class;
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

                // Check for returning a value from an initializer
                if self.current_function == FunctionType::Initializer && value.is_some() {
                    return Err(format!("Can't return a value from an initializer at line {}.", keyword.line));
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
        let enclosing_function = self.current_function;
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
            Expr::Get(object, _name) => {
                self.resolve_expr(object)?;
                Ok(())
            },
            Expr::Set(object, _name, value) => {
                self.resolve_expr(value)?;
                self.resolve_expr(object)?;
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
            Expr::This(keyword) => {
                if self.current_class == ClassType::None {
                    return Err(format!("Cannot use 'this' outside of a class at line {}.", keyword.line));
                }
                self.resolve_local(expr, keyword);
                Ok(())
            },
            Expr::Super(keyword, _) => {
                match self.current_class {
                    ClassType::None => {
                        eprintln!("Resolver Error: Cannot use 'super' outside of a class at line {}", keyword.line);
                        return Err(format!("Cannot use 'super' outside of a class at line {}.", keyword.line));
                    },
                    ClassType::Class => {
                        eprintln!("Resolver Error: Cannot use 'super' in a class with no superclass at line {}", keyword.line);
                        return Err(format!("Cannot use 'super' in a class with no superclass at line {}.", keyword.line));
                    },
                    ClassType::Subclass => {
                        // This is valid - resolve 'super' by line number for more stable reference
                        eprintln!("Resolver: Resolving 'super' in subclass at line {}", keyword.line);
                        
                        // Look for 'super' in the scopes
                        for (i, scope) in self.scopes.iter().rev().enumerate() {
                            if scope.contains_key("super") {
                                // Store distance by line number for super expressions
                                eprintln!("  Found 'super' at distance {} (line: {})", i, keyword.line);
                                self.super_expressions.insert(keyword.line, i);
                                break;
                            }
                        }
                        
                        // Also do traditional resolution
                        self.resolve_local(expr, keyword);
                    },
                }
                Ok(())
            },
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
        eprintln!("Resolver: Resolving local variable '{}' at line {}", name.lexeme, name.line);
        
        // Search from innermost to outermost scope
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            eprintln!("  Checking scope {}: {:?}", i, scope.keys().collect::<Vec<_>>());
            if scope.contains_key(&name.lexeme) {
                // Found it! Store the distance in the locals map
                let expr_id = self.get_expr_id(expr);
                self.locals.insert(expr_id, i);
                eprintln!("  Found '{}' at distance {} (Expr ID: {})", name.lexeme, i, expr_id);
                return;
            }
        }
        // Not found. Assume it is global.
        eprintln!("  '{}' not found in any scope, assuming global", name.lexeme);
    }

    // Generate a unique ID for an expression based on its memory address
    fn get_expr_id(&self, expr: &Expr) -> usize {
        expr as *const _ as usize
    }

    pub fn get_locals(&self) -> &HashMap<usize, usize> {
        &self.locals
    }

    // Add method to get super expressions for the interpreter
    pub fn get_super_expressions(&self) -> &HashMap<usize, usize> {
        &self.super_expressions
    }
}
