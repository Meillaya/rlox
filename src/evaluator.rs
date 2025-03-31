use crate::parser::{Expr, LiteralValue, Stmt};
use crate::tokenizer::{Token, TokenType};
use crate::environment::Environment;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

// --- LoxCallable Trait ---
// Trait for anything callable in Lox (functions, classes, methods)
pub trait LoxCallable: fmt::Debug {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> Result<Value, RuntimeError>;
    // Removed to_string as Display is implemented on Value
}

// --- LoxClass Struct ---
#[derive(Debug, Clone)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, Rc<LoxFunction>>, // Store methods by name
}

impl LoxClass {
    fn new(name: String, methods: HashMap<String, Rc<LoxFunction>>) -> Self {
        LoxClass { name, methods }
    }

    // Method to find a method by name
    fn find_method(&self, name: &str) -> Option<Rc<LoxFunction>> {
        self.methods.get(name).cloned()
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        // If the class has an initializer, its arity is the initializer's arity
        // Otherwise, the arity is 0
        match self.find_method("init") {
            Some(initializer) => initializer.arity(),
            None => 0,
        }
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> Result<Value, RuntimeError> {
        // Create the instance
        let instance = LoxInstance::new(Rc::new(self.clone())); 
        let instance_value = Value::Instance(Rc::new(RefCell::new(instance)));
        
        // Look for an initializer and call it if it exists
        if let Some(initializer) = self.find_method("init") {
            // Create the bound method with 'this' pointing to our new instance
            let bound_initializer = initializer.bind(instance_value.clone());
            
            // Call the initializer
            bound_initializer.call(interpreter, arguments)?;
        } else if !arguments.is_empty() {
            // No initializer but arguments were provided
            return Err(RuntimeError::new(
                format!("Expected 0 arguments but got {}.", arguments.len()), 
                0
            ));
        }
        
        // Return the instance (not the result of the initializer)
        Ok(instance_value)
    }
}

// --- LoxInstance Struct ---
#[derive(Debug, Clone)]
pub struct LoxInstance {
    class: Rc<LoxClass>, // Instance holds a reference to its class
    fields: HashMap<String, Value>,
}

impl LoxInstance {
    fn new(class: Rc<LoxClass>) -> Self {
        LoxInstance { 
            class, 
            fields: HashMap::new() 
        }
    }

    // Method to get a property (field or method)
    fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            return Ok(value.clone());
        }
        if let Some(method) = self.class.find_method(&name.lexeme) {
            // Create the receiver Value::Instance correctly
            let receiver_instance = Value::Instance(Rc::new(RefCell::new(self.clone())));
            let bound_method = method.bind(receiver_instance); // bind method takes Value
            return Ok(Value::BoundMethod(Rc::new(bound_method)));
        }
        Err(RuntimeError::new(
            format!("Undefined property '{}'.", name.lexeme),
            name.line,
        ))
    }

    // Method to set a property
    fn set(&mut self, name: &Token, value: Value) {
        self.fields.insert(name.lexeme.clone(), value);
    }
}

// --- LoxFunction Struct ---
#[derive(Clone)] // Clone needed for methods map
pub struct LoxFunction {
    declaration: Rc<Stmt>, // Store the Stmt::Function directly
    closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    fn new(declaration: Rc<Stmt>, closure: Rc<RefCell<Environment>>) -> Self {
        LoxFunction { declaration, closure }
    }

    // Helper to extract parameters (assuming declaration is Stmt::Function)
    fn params(&self) -> Option<&Vec<Token>> {
        if let Stmt::Function(_, ref params, _) = *self.declaration {
            Some(params)
        } else {
            None
        }
    }
    
    // Helper to extract body (assuming declaration is Stmt::Function)
    fn body(&self) -> Option<&Vec<Stmt>> {
        if let Stmt::Function(_, _, ref body) = *self.declaration {
            Some(body)
        } else {
            None
        }
    }

    // Helper to get function name
    fn name(&self) -> Option<&str> {
         if let Stmt::Function(ref name_token, _, _) = *self.declaration {
            Some(&name_token.lexeme)
        } else {
            None
        }
    }

    // Method to bind `this` to the function, creating a BoundMethod
    fn bind(&self, instance: Value) -> BoundMethod {
        BoundMethod::new(instance, Rc::new(self.clone()))
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.params().map_or(0, |p| p.len())
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> Result<Value, RuntimeError> {
        let environment = Rc::new(RefCell::new(Environment::new_with_enclosing(Rc::clone(&self.closure))));
        
        if let Some(params) = self.params() {
             if params.len() != arguments.len() {
                // TODO: Improve error reporting with line number
                return Err(RuntimeError::new(format!("Expected {} arguments but got {}.", params.len(), arguments.len()), 0));
            }
            for (param, arg) in params.iter().zip(arguments) {
                environment.borrow_mut().define(param.lexeme.clone(), arg.clone());
            }
        } else {
             return Err(RuntimeError::new("Internal error: LoxFunction declaration is not Stmt::Function".to_string(), 0));
        }

        let body = self.body().ok_or_else(|| RuntimeError::new("Internal error: LoxFunction body not found".to_string(), 0))?;
        let is_initializer = self.name().map_or(false, |name| name == "init");

        match interpreter.execute_block(body, environment, false) { // Pass false for print_expr_result
            Ok(_) => {
                if is_initializer {
                    // For init methods, we return "this" which should be 
                    // the first variable in the closure's environment
                    if let Some(this_value) = self.closure.borrow().get_at(0, "this") {
                        Ok(this_value)
                    } else {
                        Ok(Value::Nil) // Fallback, should not happen
                    }
                } else {
                    Ok(Value::Nil) // Implicit return nil for regular functions
                }
            },
            Err(RuntimeError::Return(value)) => {
                if is_initializer {
                    // For init methods, we return "this" even if there's an explicit return
                    if let Some(this_value) = self.closure.borrow().get_at(0, "this") {
                        Ok(this_value)
                    } else {
                        Ok(Value::Nil) // Fallback, should not happen
                    }
                } else {
                    Ok(value) // Return the value for regular functions
                }
            },
            Err(e) => Err(e),
        }
    }
}

impl fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name().unwrap_or("<script>");
        write!(f, "<fn {}>", name)
    }
}

// --- BoundMethod Struct ---
#[derive(Clone)]
pub struct BoundMethod {
    receiver: Value, // The instance (`this`)
    method: Rc<LoxFunction>, // The actual method function
}

impl BoundMethod {
    fn new(receiver: Value, method: Rc<LoxFunction>) -> Self {
        BoundMethod { receiver, method }
    }
}

impl LoxCallable for BoundMethod {
    fn arity(&self) -> usize {
        self.method.arity()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> Result<Value, RuntimeError> {
        // Create a new environment for the method call, enclosing the method's original closure
        let environment = Rc::new(RefCell::new(Environment::new_with_enclosing(Rc::clone(&self.method.closure))));
        
        // Define "this" in the method's environment
        environment.borrow_mut().define("this".to_string(), self.receiver.clone());

        // Define parameters
        if let Some(params) = self.method.params() {
            if params.len() != arguments.len() {
                 // TODO: Improve error reporting with line number
                return Err(RuntimeError::new(format!("Expected {} arguments but got {}.", params.len(), arguments.len()), 0));
            }
            for (param, arg) in params.iter().zip(arguments) {
                environment.borrow_mut().define(param.lexeme.clone(), arg.clone());
            }
        } else {
             return Err(RuntimeError::new("Internal error: BoundMethod's method declaration is not Stmt::Function".to_string(), 0));
        }
        
        let body = self.method.body().ok_or_else(|| RuntimeError::new("Internal error: BoundMethod body not found".to_string(), 0))?;

        // Check if this is an initializer
        let is_initializer = self.method.name().map_or(false, |name| name == "init");

        // Execute the method body in the new environment
        match interpreter.execute_block(body, environment, false) { // Pass false for print_expr_result
            Ok(_) => {
                if is_initializer {
                    // For init() methods, always return 'this'
                    Ok(self.receiver.clone())
                } else {
                    // Regular methods return nil
                    Ok(Value::Nil)
                }
            },
            Err(RuntimeError::Return(value)) => {
                if is_initializer {
                    // Even with explicit return, init() returns 'this'
                    Ok(self.receiver.clone())
                } else {
                    Ok(value)
                }
            },
            Err(e) => Err(e),
        }
    }
}

impl fmt::Debug for BoundMethod {
     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Delegate Debug formatting to the underlying LoxFunction
        write!(f, "{:?}", self.method)
    }
}

// --- Updated Value Enum and Impls ---
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

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    NativeFunction(Rc<NativeFn>), // Use wrapper struct
    Function(Rc<LoxFunction>),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
    BoundMethod(Rc<BoundMethod>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Value::String(s) => f.debug_tuple("String").field(s).finish(),
            Value::Boolean(b) => f.debug_tuple("Boolean").field(b).finish(),
            Value::Nil => write!(f, "Nil"),
            Value::NativeFunction(_) => write!(f, "<native fn>"), // Keep simple debug
            Value::Function(func) => write!(f, "{:?}", func),
            Value::Class(class) => write!(f, "{:?}", class), // Use LoxClass Debug
            Value::Instance(instance) => write!(f, "{:?}", instance.borrow()), // Use LoxInstance Debug
            Value::BoundMethod(method) => write!(f, "{:?}", method), // Use BoundMethod Debug
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
            Value::Function(func) => write!(f, "<fn {}>", func.name().unwrap_or("_")),
            Value::Class(class) => write!(f, "{}", class.name),
            Value::Instance(instance) => write!(f, "{} instance", instance.borrow().class.name), // Access name via class field
            Value::BoundMethod(method) => write!(f, "{:?}", method.method), // Display as underlying function
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(l), Value::Number(r)) => (l - r).abs() < f64::EPSILON,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            // Use pointer equality for Rc-wrapped callables/objects
            (Value::NativeFunction(l), Value::NativeFunction(r)) => Rc::ptr_eq(l, r),
            (Value::Function(l), Value::Function(r)) => Rc::ptr_eq(l, r),
            (Value::Class(l), Value::Class(r)) => Rc::ptr_eq(l, r),
            (Value::Instance(l), Value::Instance(r)) => Rc::ptr_eq(l, r),
            (Value::BoundMethod(l), Value::BoundMethod(r)) => Rc::ptr_eq(l, r),
            _ => false, 
        }
    }
}

// --- Native Function Wrapper ---
#[derive(Clone)] // Needed for Rc
pub struct NativeFn(pub fn(&mut Interpreter, &[Value]) -> Result<Value, RuntimeError>); // Made field public

impl LoxCallable for NativeFn {
    fn arity(&self) -> usize { 0 } // Assuming arity 0 for simplicity now (e.g., clock)
    fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> Result<Value, RuntimeError> {
        // Basic arity check
        if !arguments.is_empty() {
             return Err(RuntimeError::new(format!("Native function expected 0 arguments but got {}.", arguments.len()), 0)); // TODO: Line number?
        }
        (self.0)(interpreter, arguments)
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
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
        
        // Define native clock function using the wrapper
        let clock_native = NativeFn(|_interpreter, _arguments| {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap();
            Ok(Value::Number(now.as_secs_f64()))
        });
        globals.borrow_mut().define("clock".to_string(), Value::NativeFunction(Rc::new(clock_native)));
        
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
    
    fn look_up_variable(&mut self, name: &Token, expr: &Expr) -> Result<Value, RuntimeError> {
        let expr_id = self.get_expr_id(expr);
        // Check locals first (variables resolved by the Resolver, potentially with a specific depth)
        if let Some(distance) = self.locals.get(&expr_id) {
            // If resolved, get it from the correct environment distance relative to the current one
            self.environment.borrow().get_at(*distance, &name.lexeme)
                 .ok_or_else(|| RuntimeError::new(format!("Undefined variable '{}' (at resolved distance {}). Resolver bug?", name.lexeme, distance), name.line))
        } else {
            // If the variable wasn't resolved by the Resolver (e.g., could be global, or like 'Foo' inside the method referring to the class scope),
            // perform a standard lookup starting from the current environment and walking up the enclosure chain.
            // This will find globals if the variable isn't found in any enclosing scope.
            self.environment.borrow().get(name) // Use standard lookup which walks the environment chain.
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
            Stmt::Function(name_token, _, _) => {
                // Preserve live reference for globals, snapshot for others
                let closure = if Rc::ptr_eq(&self.environment, &self.globals) {
                    Rc::clone(&self.globals)
                } else {
                    // For non-global environments, capture snapshot
                    let env_snapshot = self.environment.borrow().clone();
                    Rc::new(RefCell::new(env_snapshot))
                };
                
                let function = LoxFunction::new(Rc::new(stmt.clone()), closure);
                self.environment.borrow_mut().define(name_token.lexeme.clone(), Value::Function(Rc::new(function)));
                Ok(())
            },
            Stmt::Class { name, methods } => {
                // Define the class name first (allows classes to refer to themselves)
                self.environment.borrow_mut().define(name.lexeme.clone(), Value::Nil); // Temporarily Nil
                
                let mut class_methods = HashMap::new();
                for method_stmt_variant in methods {
                    // We expect Stmt::Function here, clone it for Rc
                    if let Stmt::Function(method_name, _, _) = method_stmt_variant {
                         let method_stmt_rc = Rc::new(method_stmt_variant.clone());
                         // Methods capture the environment where the CLASS is defined
                         let function = LoxFunction::new(method_stmt_rc, Rc::clone(&self.environment));
                         class_methods.insert(method_name.lexeme.clone(), Rc::new(function));
                    } else {
                         return Err(RuntimeError::new(format!("Invalid statement in class '{}' body. Expected method.", name.lexeme), name.line));
                    }
                }
                
                let klass = LoxClass::new(name.lexeme.clone(), class_methods);
                let class_value = Value::Class(Rc::new(klass));

                // Now assign the actual class value, overwriting the temporary Nil
                self.environment.borrow_mut().assign(name, class_value)?;
                
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
    
    fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
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
                    _ => Ok(Value::String("Unimplemented binary operator".to_string())), // Error or unreachable
                }
            },
            Expr::Variable(name) => {
                self.look_up_variable(name, expr)
            },
            Expr::Assign(name, value_expr) => {
                let value = self.evaluate(value_expr)?;
                let expr_id = self.get_expr_id(expr);
                if let Some(distance) = self.locals.get(&expr_id) {
                    // Assign to a resolved variable at a specific depth
                    self.environment.borrow_mut().assign_at(*distance, name, value.clone())?;
                } else {
                    // If assignment isn't resolved, assign in the current environment chain (walking up).
                    // This handles assigning to globals if the variable isn't found in any enclosing scope.
                    self.environment.borrow_mut().assign(name, value.clone())?;
                }
                Ok(value)
            },
            Expr::Logical(left, operator, right) => {
                let left_val = self.evaluate(left)?;
                if operator.token_type == TokenType::Or {
                    if is_truthy(&left_val) {
                        return Ok(left_val);
                    }
                } else {
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
                // Use dynamic dispatch via the LoxCallable trait
                match callee_val {
                    Value::Function(func) => func.call(self, &arg_values),
                    Value::Class(class) => class.call(self, &arg_values),
                    Value::BoundMethod(method) => method.call(self, &arg_values),
                    Value::NativeFunction(native_fn) => native_fn.call(self, &arg_values),
                    _ => Err(RuntimeError::new(
                        "Can only call functions, classes, and methods.".to_string(), // Updated error message
                        paren.line,
                    )),
                }
            },
            Expr::Get(object, name) => {
                let object_val = self.evaluate(object)?;
                if let Value::Instance(instance_rc) = object_val {
                    // Delegate to LoxInstance::get
                    instance_rc.borrow().get(name)
                } else {
                    Err(RuntimeError::new("Only instances have properties.".to_string(), name.line))
                }
            },
            Expr::Set(object, name, value) => {
                let object_val = self.evaluate(object)?;
                if let Value::Instance(instance_rc) = object_val {
                    let value_to_set = self.evaluate(value)?;
                    instance_rc.borrow_mut().set(name, value_to_set.clone());
                    Ok(value_to_set) 
                } else {
                    Err(RuntimeError::new("Only instances have fields.".to_string(), name.line))
                }
            },
            Expr::This(keyword) => {
                self.look_up_variable(keyword, expr)
            }
        }
    }
}

fn compare_equality(left: &Value, right: &Value) -> bool {
    left == right
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
        Value::Instance(_) => true,
        _ => true,
    }
}

