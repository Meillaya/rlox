use crate::parser::{Expr, LiteralValue, Stmt};
use crate::tokenizer::{Token, TokenType};
use crate::environment::Environment;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;


#[derive(Debug)]
pub enum RuntimeError {
    Error { message: String, line: usize },
    Return(Value),
}

impl RuntimeError {
    pub fn new(message: String, line: usize) -> Self {
        RuntimeError::Error { message, line }
    }
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    NativeFunction(fn() -> Value),
    FunctionRef(String, usize, Rc<RefCell<Environment>>),
    Class(String),
}

// Manual Debug implementation for Value to prevent recursion
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Value::String(s) => f.debug_tuple("String").field(s).finish(),
            Value::Boolean(b) => f.debug_tuple("Boolean").field(b).finish(),
            Value::Nil => write!(f, "Nil"),
            Value::NativeFunction(_) => write!(f, "NativeFunction(<fn ptr>)"),
            Value::FunctionRef(name, stmt_id, closure_env) => {
                f.debug_struct("FunctionRef")
                 .field("name", name)
                 .field("stmt_id", stmt_id)
                 .field("closure", &format_args!("<Env@{:p}>", Rc::as_ptr(closure_env)))
                 .finish()
            }
            Value::Class(name) => f.debug_tuple("Class").field(name).finish(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::NativeFunction(_) => write!(f, "<native fn>"),
            Value::FunctionRef(name, _stmt_id, _) => write!(f, "<fn {}>", name),
            Value::Class(name) => write!(f, "{}", name),
        }
    }
}

fn is_number(value: &Value) -> bool {
    matches!(value, Value::Number(_))
}

fn get_number(value: &Value) -> Result<f64, RuntimeError> {
    match value {
        Value::Number(n) => Ok(*n),
        _ => Err(RuntimeError::new("Operand must be a number.".to_string(), 0)),
    }
}

fn is_string(value: &Value) -> bool {
    matches!(value, Value::String(_))
}

pub struct Interpreter {
    source_statements: Rc<Vec<Stmt>>,
    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    locals: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new(source: Rc<Vec<Stmt>>) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        globals.borrow_mut().define_natives();
        
        Interpreter {
            source_statements: source,
            environment: Rc::clone(&globals),
            globals,
            locals: HashMap::new(),
        }
    }
    
    pub fn interpret(&mut self, statements: &[Stmt], print_expr_result: bool) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute_stmt(statement, print_expr_result)?;
        }
        Ok(())
    }
    
    fn get_stmt_id(&self, stmt: &Stmt) -> usize { stmt as *const _ as usize }
    fn get_expr_id(&self, expr: &Expr) -> usize { expr as *const _ as usize }
    
    pub fn set_locals(&mut self, locals: HashMap<usize, usize>) { self.locals = locals; }
    
    fn look_up_variable(&self, name: &Token, expr: &Expr) -> Result<Value, RuntimeError> {
        let expr_id = self.get_expr_id(expr);
        eprintln!("[Evaluator] Looking up variable: '{}' (Expr ID: {}) at line {}", name.lexeme, expr_id, name.line);

        if let Some(distance) = self.locals.get(&expr_id) {
            eprintln!("[Evaluator] Found distance {} in locals map for '{}'. Trying environment.get_at({}, \"{}\").", distance, name.lexeme, distance, name.lexeme);
            let value = self.environment.borrow().get_at(*distance, &name.lexeme);
            eprintln!("[Evaluator] environment.get_at({}, '{}') returned: {:?}", distance, name.lexeme, value);
            
            value.ok_or_else(|| RuntimeError::new(format!("Undefined variable '{}'.", name.lexeme), name.line))
        } else {
            eprintln!("[Evaluator] Variable '{}' not found in locals map. Assuming global. Trying globals.get(\"{}\").", name.lexeme, name.lexeme);
            let result = self.globals.borrow().get(name);
            eprintln!("[Evaluator] globals.get('{}') returned: {:?}", name.lexeme, result);
            result
        }
    }
    
    fn execute_stmt(&mut self, stmt: &Stmt, print_expr_result: bool) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Print(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value);
                Ok(())
            }
            Stmt::Expression(expr) => {
                let value = self.evaluate(expr)?;
                if print_expr_result {
                    println!("{}", value);
                }
                Ok(())
            }
            Stmt::Var(name, initializer) => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };
                self.environment.borrow_mut().define(name.lexeme.clone(), value);
                Ok(())
            }
            Stmt::Block(statements) => {
                let block_env = Rc::new(RefCell::new(Environment::new_with_enclosing(Rc::clone(&self.environment))));
                self.execute_block(statements, block_env, print_expr_result)
            }
            Stmt::Function(name, params, body) => {
                let stmt_id = self.get_stmt_id(stmt);
                let function_ref = Value::FunctionRef(
                    name.lexeme.clone(), 
                    stmt_id, 
                    Rc::clone(&self.environment)
                );
                self.environment.borrow_mut().define(name.lexeme.clone(), function_ref);
                Ok(())
            },
            Stmt::Class { name, methods } => {
                self.environment.borrow_mut().define(
                    name.lexeme.clone(), 
                    Value::Class(name.lexeme.clone())
                );
                Ok(())
            },
            Stmt::If(condition, then_branch, else_branch) => {
                let condition_value = self.evaluate(condition)?;
                if is_truthy(&condition_value) {
                    self.execute_stmt(then_branch, print_expr_result)?;
                } else if let Some(else_stmt) = else_branch {
                    self.execute_stmt(else_stmt, print_expr_result)?;
                }
                Ok(())
            },
            Stmt::While(condition, body) => {
                while is_truthy(&self.evaluate(condition)?) {
                    self.execute_stmt(body, print_expr_result)?;
                }
                Ok(())
            },
            Stmt::Return(_, value) => {
                let return_value = match value {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };
                Err(RuntimeError::Return(return_value))
            }
        }
    }
    
    fn execute_block(&mut self, statements: &[Stmt], environment: Rc<RefCell<Environment>>, print_expr_result: bool) -> Result<(), RuntimeError> {
        let previous = Rc::clone(&self.environment);
        self.environment = environment;
        
        let result = (|| {
            for statement in statements {
                self.execute_stmt(statement, print_expr_result)?;
            }
            Ok(())
        })();
        
        self.environment = previous;
        result
    }
    
    fn evaluate(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(literal) => Ok(match literal {
                LiteralValue::Boolean(value) => Value::Boolean(*value),
                LiteralValue::Number(value) => Value::Number(*value),
                LiteralValue::String(value) => Value::String(value.clone()),
                LiteralValue::Nil => Value::Nil,
            }),
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Unary(operator, expr) => {
                let right = self.evaluate(expr)?;
                match operator.token_type {
                    TokenType::Minus => {
                        if let Value::Number(n) = right {
                            Ok(Value::Number(-n))
                        } else {
                            Err(RuntimeError::new("Operand must be a number.".to_string(), operator.line))
                        }
                    },
                    TokenType::Bang => Ok(Value::Boolean(!is_truthy(&right))),
                    _ => Ok(Value::String("Unimplemented".to_string())),
                }
            },
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                match operator.token_type {
                    TokenType::Plus => {
                        if is_number(&left) && is_number(&right) {
                            Ok(Value::Number(get_number(&left)? + get_number(&right)?))
                        } else if is_string(&left) && is_string(&right) {
                            match (&left, &right) {
                                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                                _ => unreachable!(),
                            }
                        } else {
                            Err(RuntimeError::new("Operands must be two numbers or two strings.".to_string(), operator.line))
                        }
                    },
                    TokenType::Minus => {
                        if is_number(&left) && is_number(&right) {
                            Ok(Value::Number(get_number(&left)? - get_number(&right)?))
                        } else {
                            Err(RuntimeError::new("Operands must be numbers.".to_string(), operator.line))
                        }
                    },
                    TokenType::Star => {
                        if is_number(&left) && is_number(&right) {
                            Ok(Value::Number(get_number(&left)? * get_number(&right)?))
                        } else {
                            Err(RuntimeError::new("Operands must be numbers.".to_string(), operator.line))
                        }
                    },
                    TokenType::Slash => {
                        if is_number(&left) && is_number(&right) {
                            let right_num = get_number(&right)?;
                            if right_num == 0.0 {
                                Err(RuntimeError::new("Division by zero.".to_string(), operator.line))
                            } else {
                                Ok(Value::Number(get_number(&left)? / right_num))
                            }
                        } else {
                            Err(RuntimeError::new("Operands must be numbers.".to_string(), operator.line))
                        }
                    },
                    TokenType::Greater => compare_values(&left, &right, |a, b| a > b, operator.line),
                    TokenType::GreaterEqual => compare_values(&left, &right, |a, b| a >= b, operator.line),
                    TokenType::Less => compare_values(&left, &right, |a, b| a < b, operator.line),
                    TokenType::LessEqual => compare_values(&left, &right, |a, b| a <= b, operator.line),
                    TokenType::EqualEqual => Ok(Value::Boolean(compare_equality(&left, &right))),
                    TokenType::BangEqual => Ok(Value::Boolean(!compare_equality(&left, &right))),
                    _ => Ok(Value::String("Unimplemented".to_string())),
                }
            },
            Expr::Variable(name) => {
                self.look_up_variable(name, expr)
            },
            Expr::Assign(name, value_expr) => {
                let value = self.evaluate(value_expr)?;
                
                let expr_id = self.get_expr_id(expr);
                if let Some(distance) = self.locals.get(&expr_id) {
                    self.environment.borrow_mut().assign_at(*distance, name, value.clone())?;
                } else {
                    self.globals.borrow_mut().assign(name, value.clone())?;
                }
                
                Ok(value)
            },
            Expr::Logical(left, operator, right) => {
                let left_val = self.evaluate(left)?;
                
                if operator.token_type == TokenType::Or {
                    if is_truthy(&left_val) {
                        return Ok(left_val);
                    }
                } else if operator.token_type == TokenType::And {
                    if !is_truthy(&left_val) {
                        return Ok(left_val);
                    }
                }
                
                self.evaluate(right)
            },
            Expr::Call(callee, paren, arguments) => {
                let callee_val = self.evaluate(callee)?;

                let mut arg_values = Vec::new();
                for arg in arguments {
                    arg_values.push(self.evaluate(arg)?);
                }

                match callee_val {
                    Value::NativeFunction(func) => {
                        if !arg_values.is_empty() {
                            return Err(RuntimeError::new(
                                format!("Expected 0 arguments but got {}.", arg_values.len()),
                                paren.line,
                            ));
                        }
                        Ok(func())
                    }
                    Value::FunctionRef(name, stmt_id, closure) => {
                        let func_stmt = self.find_stmt_by_id(stmt_id)
                            .ok_or_else(|| RuntimeError::new(
                                format!("Internal error: Failed to find function definition for ID {}.", stmt_id),
                                paren.line
                            ))?;

                        if let Stmt::Function(_, params, body) = func_stmt {
                            if arg_values.len() != params.len() {
                                return Err(RuntimeError::Error {
                                    message: format!("Expected {} arguments but got {}.", params.len(), arg_values.len()),
                                    line: paren.line,
                                });
                            }

                            let function_env = Rc::new(RefCell::new(Environment::new_with_enclosing(closure)));

                            for (param, arg) in params.iter().zip(arg_values) {
                                function_env.borrow_mut().define(param.lexeme.clone(), arg);
                            }

                            let mut temp_interpreter = Interpreter {
                                source_statements: Rc::clone(&self.source_statements),
                                environment: function_env,
                                globals: Rc::clone(&self.globals),
                                locals: self.locals.clone(),
                            };
                            
                            let execution_result = (|| {
                                for statement in body.iter() {
                                    temp_interpreter.execute_stmt(statement, false)?;
                                }
                                Ok(Value::Nil)
                            })();

                            match execution_result {
                                Err(RuntimeError::Return(value)) => Ok(value),
                                Err(e) => Err(e),
                                Ok(nil_value) => Ok(nil_value),
                            }
                        } else {
                            Err(RuntimeError::new(
                                format!("Internal error: FunctionRef ID {} did not point to Stmt::Function.", stmt_id),
                                paren.line
                            ))
                        }
                    }
                    _ => Err(RuntimeError::new(
                        "Can only call functions and classes.".to_string(),
                        paren.line,
                    )),
                }
            }
        }
    }

    fn find_stmt_by_id(&self, target_id: usize) -> Option<&Stmt> {
        // Recursive helper to search within a slice of statements
        fn find_in_stmt_slice<'a>(stmts: &'a [Stmt], target_id: usize) -> Option<&'a Stmt> {
            for stmt in stmts {
                // Use helper to check the statement itself and recurse if necessary
                if let Some(found) = find_in_stmt(stmt, target_id) {
                    return Some(found);
                }
            }
            None
        }
        
        // Recursive helper to search a single statement and its children
        fn find_in_stmt<'a>(stmt: &'a Stmt, target_id: usize) -> Option<&'a Stmt> {
             // Check if the current statement is the target
             if (stmt as *const Stmt as usize) == target_id { 
                 return Some(stmt); 
             }
             
             // Recurse into nested statements based on type
             match stmt {
                 Stmt::Block(body) => find_in_stmt_slice(body, target_id),
                 Stmt::If(_, then_branch, else_branch) => {
                    // Dereference Box<Stmt> before recursing
                    find_in_stmt(&**then_branch, target_id).or_else(|| {
                        else_branch.as_ref().and_then(|eb| find_in_stmt(&**eb, target_id))
                    })
                 }
                 Stmt::While(_, body_stmt) => {
                     // Dereference Box<Stmt> before recursing
                     find_in_stmt(&**body_stmt, target_id)
                 }
                 Stmt::Function(_, _, body_vec) => find_in_stmt_slice(body_vec, target_id),
                  _ => None, // Other statement types don't contain nested statements to search
             }
        }

        // Start the search from the top-level statements
        find_in_stmt_slice(&self.source_statements, target_id)
    }
}

fn compare_equality(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(l), Value::Number(r)) => (l - r).abs() < f64::EPSILON,
        (Value::String(l), Value::String(r)) => l == r,
        (Value::Boolean(l), Value::Boolean(r)) => l == r,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}

fn compare_values(left: &Value, right: &Value, compare: fn(f64, f64) -> bool, line: usize) -> Result<Value, RuntimeError> {
    match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(compare(*l, *r))),
        _ => Err(RuntimeError::new("Operands must be numbers.".to_string(), line)),
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Boolean(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}

