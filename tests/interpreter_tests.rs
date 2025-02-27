use std::cell::RefCell;
use std::rc::Rc;
use crate::parser::{Parser, Stmt, LiteralValue};
use crate::tokenizer::Tokenizer;
use crate::evaluator::{self, Environment, Value, RuntimeError};
use crate::resolver::Resolver;

// Helper function to run a single expression and return its evaluated value.
fn run_expr(source: &str) -> Result<Value, String> {
    // Tokenize source
    let mut tokenizer = Tokenizer::new(source);
    let tokens = tokenizer.scan_tokens();
    if tokenizer.has_error {
        return Err("Tokenization error".to_string());
    }

    // Parse tokens into statements
    let mut parser = Parser::new(tokens);
    let statements = parser.parse().map_err(|e| e.to_string())?;
    if statements.is_empty() {
        return Err("No statements found".to_string());
    }

    // Run resolver
    let mut resolver = Resolver::new();
    resolver.resolve(&statements);
    if !resolver.errors.is_empty() {
        return Err(resolver.errors.join("\n"));
    }

    // Create a new environment with natives
    let env = Rc::new(RefCell::new(Environment::new()));
    env.borrow_mut().define_natives();

    // Expect the first statement to be an expression
    if let Stmt::Expression(expr) = &statements[0] {
        evaluator::evaluate(expr, Rc::clone(&env), &resolver.locals)
            .map_err(|e| match e {
                RuntimeError::Error { message, line: _ } => message,
                RuntimeError::Return(_) => "Unexpected return".to_string(),
            })
    } else {
        Err("First statement is not an expression.".to_string())
    }
}

// Helper function to run an entire program (multiple statements) and collect printed outputs.
// For simplicity, we'll assume programs use print statements to output values and we capture them.
// Here we simulate execution by evaluating all statements and if a statement is an expression, we capture its value.
fn run_program(source: &str) -> Result<Vec<Value>, String> {
    let mut tokenizer = Tokenizer::new(source);
    let tokens = tokenizer.scan_tokens();
    if tokenizer.has_error {
        return Err("Tokenization error".to_string());
    }

    let mut parser = Parser::new(tokens);
    let statements = parser.parse().map_err(|e| e.to_string())?;

    let mut resolver = Resolver::new();
    resolver.resolve(&statements);
    if !resolver.errors.is_empty() {
        return Err(resolver.errors.join("\n"));
    }

    let env = Rc::new(RefCell::new(Environment::new()));
    env.borrow_mut().define_natives();
    let mut outputs = Vec::new();
    for stmt in statements {
        match evaluator::execute_stmt(&stmt, true, Rc::clone(&env), &resolver.locals) {
            Ok(_) => {},
            Err(RuntimeError::Return(value)) => outputs.push(value),
            Err(RuntimeError::Error { message, line: _ }) => return Err(message),
        }
    }
    Ok(outputs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic() {
        let source = "1 + 2 * 3 - 4 / 2";
        let result = run_expr(source).expect("Failed to evaluate expression");
        if let Value::Number(n) = result {
            assert!((n - 1.0 - 2.0 * 3.0 + 4.0/2.0).abs() < f64::EPSILON);
        } else {
            panic!("Expected a number");
        }
    }

    #[test]
    fn test_variable_declaration_and_usage() {
        let source = "var a = 10; a;";
        let result = run_expr(source).expect("Failed to evaluate variable usage");
        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn test_block_scoping() {
        let source = "var a = 1; { var a = 2; } a;";
        let result = run_expr(source).expect("Failed block scoping");
        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn test_function_definition_and_call() {
        let source = "fun add(a, b) { a + b; } add(3, 4);";
        let result = run_expr(source).expect("Failed to evaluate function call");
        assert_eq!(result, Value::Number(7.0));
    }

    #[test]
    fn test_logical_and_or() {
        // Test 'and'
        let result_and = run_expr("true and false").expect("Failed logical and");
        assert_eq!(result_and, Value::Boolean(false));
        
        // Test 'or'
        let result_or = run_expr("true or false").expect("Failed logical or");
        assert_eq!(result_or, Value::Boolean(true));
    }

    #[test]
    fn test_if_statement() {
        let source = "if (true) { 42; } else { 0; }";
        let result = run_expr(source).expect("Failed if statement");
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_while_loop() {
        // Use a while loop to sum numbers from 0 to 4
        let source = "var sum = 0; var i = 0; while (i < 5) { sum = sum + i; i = i + 1; } sum;";
        let result = run_expr(source).expect("Failed while loop");
        // Sum of 0+1+2+3+4 = 10
        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn test_native_function_clock() {
        // Just ensure that clock returns a number (time in seconds).
        let source = "clock();";
        let result = run_expr(source).expect("Failed clock function");
        if let Value::Number(n) = result {
            assert!(n > 0.0);
        } else {
            panic!("clock() did not return a number");
        }
    }

    #[test]
    fn test_error_on_self_reference_in_initializer() {
        // Variable referenced in its own initializer should trigger a resolver error.
        let source = "var a = a;";
        let mut tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.scan_tokens();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().expect("Parsing failed");
        let mut resolver = Resolver::new();
        resolver.resolve(&statements);
        assert!(!resolver.errors.is_empty(), "Expected resolver error for self-reference");
    }

    #[test]
    fn test_complex_program() {
        let source = r#"
            var fib = fun(n) {
                if (n < 2) { n; } else { fib(n - 1) + fib(n - 2); }
            };
            fib(6);
        "#;
        let result = run_expr(source).expect("Failed complex program");
        // Fibonacci sequence: fib(6) should be 8 (0,1,1,2,3,5,8)
        assert_eq!(result, Value::Number(8.0));
    }

    // Additional tests can be added to cover more edge cases
} 