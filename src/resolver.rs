use crate::parser::{Expr, Stmt};
use crate::tokenizer::Token;
use std::collections::HashMap;

/// Represents the type of function currently being resolved.
#[derive(Clone, Copy, PartialEq, Debug)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

/// Represents the type of class context currently active.
#[derive(Clone, Copy, PartialEq, Debug)]
enum ClassType {
    None,
    Class,
    Subclass,
}

/// The Resolver struct performs static scope resolution, analyzing variable bindings in the AST.
/// It maintains a stack of scopes (each a HashMap from variable names to a boolean indicating if defined),
/// collects errors encountered during resolution, records the resolution depth for each variable expression for optimized variable access,
/// and tracks the semantic context (e.g., whether we are currently in a function or class) to enforce semantic rules.
pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    /// A mapping from the unique identifier (pointer) of an expression to its resolution depth.
    pub locals: HashMap<usize, usize>,
    /// A collection of error messages encountered during resolution.
    pub errors: Vec<String>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    /// Creates a new Resolver with an empty scope stack, no errors, an empty locals mapping, and no current function or class context.
    pub fn new() -> Self {
        Resolver {
            scopes: Vec::new(),
            locals: HashMap::new(),
            errors: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    /// Resolves a list of statements, processing each one for variable declaration, definition, usage, and recording resolution depths.
    /// This typically is called on the entire AST of a program.
    pub fn resolve(&mut self, statements: &[Stmt]) {
        for stmt in statements {
            self.resolve_stmt(stmt);
        }
    }

    /// Resolves a single statement by matching on its type and delegating resolution to helper methods.
    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            // For variable declarations, declare the variable, resolve its initializer if any, then define it
            Stmt::Var(name, initializer) => {
                self.declare(name);
                if let Some(expr) = initializer {
                    self.resolve_expr(expr);
                }
                self.define(name);
            },
            // For a block, a new scope is introduced for its statements.
            Stmt::Block(statements) => {
                self.begin_scope();
                for statement in statements {
                    self.resolve_stmt(statement);
                }
                self.end_scope();
            },
            // For expression statements, resolve the contained expression.
            Stmt::Expression(expr) => {
                self.resolve_expr(expr);
            },
            // For print statements, resolve the expression to be printed.
            Stmt::Print(expr) => {
                self.resolve_expr(expr);
            },
            // For if statements, resolve the condition and both branches.
            Stmt::If(condition, then_branch, else_branch) => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_branch);
                if let Some(else_stmt) = else_branch {
                    self.resolve_stmt(else_stmt);
                }
            },
            // For while loops, resolve the condition and the loop body.
            Stmt::While(condition, body) => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            },
            // Function declarations: declare and define the function, then resolve its body in a new scope.
            Stmt::Function(name, params, body) => {
                self.declare(name);
                self.define(name);
                self.resolve_function(params, body);
            },
            // Class declarations: declare the class, resolve its superclass if any, and resolve its methods.
            Stmt::Class(ref name, ref superclass, ref methods) => {
                self.declare(name);
                self.define(name);
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;

                if let Some(ref super_expr) = superclass {
                    // Check that a class does not inherit from itself
                    if let Expr::Variable(ref super_name) = super_expr {
                        if super_name.lexeme == name.lexeme {
                            self.errors.push(format!("A class can't inherit from itself [line {}]", name.line));
                        }
                    }
                    self.resolve_expr(super_expr);

                    // Begin a new scope for 'super'
                    self.begin_scope();
                    self.scopes.last_mut().unwrap().insert("super".to_string(), true);
                    self.current_class = ClassType::Subclass;
                }

                // Begin a new scope for the class body and define 'this'
                self.begin_scope();
                self.scopes.last_mut().unwrap().insert("this".to_string(), true);

                for method in methods.iter() {
                    if let Stmt::Function(ref method_name, ref params, ref body) = method {
                        let declaration = if method_name.lexeme == "init" {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Function
                        };
                        let enclosing_function = self.current_function;
                        self.current_function = declaration;
                        self.begin_scope();
                        for param in params.iter() {
                            self.declare(param);
                            self.define(param);
                        }
                        for stmt in body.iter() {
                            self.resolve_stmt(stmt);
                        }
                        self.end_scope();
                        self.current_function = enclosing_function;
                    } else {
                        self.errors.push(format!("Invalid method declaration in class '{}'.", name.lexeme));
                    }
                }

                self.end_scope(); // end 'this' scope
                if superclass.is_some() {
                    self.end_scope(); // end 'super' scope
                }
                self.current_class = enclosing_class;
            },
            // Return statements: check that they occur inside a function and resolve the expression if present.
            Stmt::Return(keyword, value) => {
                if self.current_function == FunctionType::None {
                    self.errors.push(format!("Can't return from top-level code [line {}]", keyword.line));
                } else if self.current_function == FunctionType::Initializer {
                    if value.is_some() {
                        self.errors.push(format!("Can't return a value from an initializer [line {}]", keyword.line));
                    }
                } else if let Some(expr) = value {
                    self.resolve_expr(expr);
                }
            }
            // Additional statement types would have corresponding resolution logic here.
        }
    }

    /// Resolves an expression by recursively processing its sub-expressions.
    /// This method handles variable usage, assignments, binary operations, logical expressions, function calls, etc.
    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            // For variable expressions, check if the variable is declared but not yet defined in the current scope
            Expr::Variable(name) => {
                if !self.scopes.is_empty() {
                    if let Some(&is_defined) = self.scopes.last().unwrap().get(&name.lexeme) {
                        if !is_defined {
                            self.errors.push(format!("Can't read local variable in its own initializer [line {}]", name.line));
                        }
                    }
                    // Record the resolution depth for optimized variable lookup
                    let expr_ptr = expr as *const Expr as usize;
                    let depth = self.scopes.len() - 1;
                    self.locals.insert(expr_ptr, depth);
                }
            },
            // For assignments, first resolve the value, then record the resolution depth for the variable being assigned
            Expr::Assign(name, value) => {
                self.resolve_expr(value);
                if !self.scopes.is_empty() {
                    if let Some(&is_defined) = self.scopes.last().unwrap().get(&name.lexeme) {
                        if !is_defined {
                            self.errors.push(format!("Can't assign to variable in its own initializer [line {}]", name.line));
                        }
                    }
                    let expr_ptr = expr as *const Expr as usize;
                    let depth = self.scopes.len() - 1;
                    self.locals.insert(expr_ptr, depth);
                }
            },
            // Binary expressions require resolving both left and right operands.
            Expr::Binary(left, _operator, right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            },
            // Unary expressions resolve their single sub-expression.
            Expr::Unary(_operator, right) => {
                self.resolve_expr(right);
            },
            // Grouping expressions simply delegate to resolving the inner expression.
            Expr::Grouping(inner) => {
                self.resolve_expr(inner);
            },
            // Logical expressions resolve both left and right sub-expressions.
            Expr::Logical(left, _operator, right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            },
            // Function call expressions require resolving the callee and each argument.
            Expr::Call(callee, _paren, arguments) => {
                self.resolve_expr(callee);
                for arg in arguments {
                    self.resolve_expr(arg);
                }
            },
            // Handle 'this' expressions: they must occur within a class.
            Expr::This(name) => {
                if self.current_class == ClassType::None {
                    self.errors.push(format!("Can't use 'this' outside of a class [line {}]", name.line));
                    return;
                }
                let expr_ptr = expr as *const Expr as usize;
                let depth = self.scopes.len() - 1;
                self.locals.insert(expr_ptr, depth);
            },
            // Handle 'super' expressions: they must occur within a subclass.
            Expr::Super(keyword, _method) => {
                if self.current_class == ClassType::None {
                    self.errors.push(format!("Can't use 'super' outside of a class [line {}]", keyword.line));
                } else if self.current_class != ClassType::Subclass {
                    self.errors.push(format!("Can't use 'super' in a class with no superclass [line {}]", keyword.line));
                }
                let expr_ptr = expr as *const Expr as usize;
                let depth = self.scopes.len() - 1;
                self.locals.insert(expr_ptr, depth);
            },
            // Literals are self-contained and do not need resolution.
            Expr::Literal(_literal) => {},
            // Additional expression types can be added here with corresponding resolution logic.
        }
    }

    /// Resolves a function by creating a new scope for its parameters and body.
    /// Parameters are declared and defined in the new scope, then the function body is resolved.
    fn resolve_function(&mut self, params: &Vec<Token>, body: &Vec<Stmt>) {
        let enclosing_function = self.current_function;
        self.current_function = FunctionType::Function;
        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        for stmt in body {
            self.resolve_stmt(stmt);
        }
        self.end_scope();
        self.current_function = enclosing_function;
    }

    /// Begins a new scope by pushing a new HashMap onto the scope stack.
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Ends the current scope by popping the last HashMap off the scope stack.
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    /// Declares a variable in the current scope.
    /// This marks the variable as not yet defined (false), preventing its premature use in its own initializer.
    fn declare(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name.lexeme) {
            self.errors.push(format!("Variable '{}' already declared in this scope. [line {}]", name.lexeme, name.line));
        }
        scope.insert(name.lexeme.clone(), false);
    }

    /// Marks a variable as defined in the current scope.
    /// After this point, the variable can be safely read in its scope.
    fn define(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }
        self.scopes.last_mut().unwrap().insert(name.lexeme.clone(), true);
    }
    
    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                let expr_ptr = expr as *const Expr as usize;
                self.locals.insert(expr_ptr, i);
                return;
            }
        }
        // Not found in any scope - assume it's global
    }
}

// Helper method to lookup a variable in the current scope

