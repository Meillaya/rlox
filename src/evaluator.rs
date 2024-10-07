use crate::parser::{Expr, LiteralValue};
use crate::tokenizer::TokenType;
use std::fmt;

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub line: usize,
}

impl RuntimeError {
    fn new(message: String, line: usize) -> Self {
        RuntimeError { message, line }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
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

pub fn evaluate(expr: &Expr) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Literal(literal) => Ok(match literal {
            LiteralValue::Boolean(value) => Value::Boolean(*value),
            LiteralValue::Number(value) => Value::Number(*value),
            LiteralValue::String(value) => Value::String(value.clone()),
            LiteralValue::Nil => Value::Nil,
        }),
        Expr::Grouping(expr) => evaluate(expr),
        Expr::Unary(operator, expr) => {
            let right = evaluate(expr)?;
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
            let left = evaluate(left)?;
            let right = evaluate(right)?;
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
                TokenType::Greater => compare_values(&left, &right, |a, b| a > b),
                TokenType::GreaterEqual => compare_values(&left, &right, |a, b| a >= b),
                TokenType::Less => compare_values(&left, &right, |a, b| a < b),
                TokenType::LessEqual => compare_values(&left, &right, |a, b| a <= b),
                TokenType::EqualEqual => compare_equality(&left, &right),
                TokenType::BangEqual => {
                    let result = compare_equality(&left, &right)?;
                    Ok(Value::Boolean(!is_truthy(&result)))
                },
                _ => Ok(Value::String("Unimplemented".to_string())),
            }
        },
    }
}



fn compare_equality(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
    Ok(Value::Boolean(left == right))
}

fn compare_values(left: &Value, right: &Value, compare: fn(f64, f64) -> bool) -> Result<Value, RuntimeError> {
    if is_number(left) && is_number(right) {
        Ok(Value::Boolean(compare(get_number(left)?, get_number(right)?)))
    } else {
        Err(RuntimeError::new("Operands must be numbers.".to_string(), 0))
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Boolean(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Expr;
    use crate::tokenizer::Token;

    fn create_binary_expr(left: Expr, op: TokenType, right: Expr) -> Expr {
        Expr::Binary(
            Box::new(left),
            Token { token_type: op, lexeme: String::new(), literal: None, line: 1 },
            Box::new(right)
        )
    }

    #[test]
    fn test_literal_evaluation() {
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::Number(42.0))).unwrap(), Value::Number(42.0));
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::String("hello".to_string()))).unwrap(), Value::String("hello".to_string()));
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::Boolean(true))).unwrap(), Value::Boolean(true));
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::Nil)).unwrap(), Value::Nil);
    }

    #[test]
    fn test_arithmetic_operations() {
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::Plus,
            Expr::Literal(LiteralValue::Number(3.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Number(8.0));

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(10.0)),
            TokenType::Minus,
            Expr::Literal(LiteralValue::Number(4.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Number(6.0));

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(3.0)),
            TokenType::Star,
            Expr::Literal(LiteralValue::Number(4.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Number(12.0));

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(15.0)),
            TokenType::Slash,
            Expr::Literal(LiteralValue::Number(3.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Number(5.0));
    }

    #[test]
    fn test_comparison_operations() {
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::Greater,
            Expr::Literal(LiteralValue::Number(3.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Boolean(true));

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::LessEqual,
            Expr::Literal(LiteralValue::Number(5.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Boolean(true));

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(10.0)),
            TokenType::Less,
            Expr::Literal(LiteralValue::Number(20.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Boolean(true));
    }

    #[test]
    fn test_equality_operations() {
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(25.0)),
            TokenType::EqualEqual,
            Expr::Literal(LiteralValue::String("25".to_string()))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Boolean(false));

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(25.0)),
            TokenType::EqualEqual,
            Expr::Literal(LiteralValue::Number(25.0))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Boolean(true));

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::String("hello".to_string())),
            TokenType::BangEqual,
            Expr::Literal(LiteralValue::String("world".to_string()))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Boolean(true));
    }

    #[test]
    fn test_unary_operations() {
        let expr = Expr::Unary(
            Token { token_type: TokenType::Minus, lexeme: String::new(), literal: None, line: 1 },
            Box::new(Expr::Literal(LiteralValue::Number(5.0)))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Number(-5.0));

        let expr = Expr::Unary(
            Token { token_type: TokenType::Bang, lexeme: String::new(), literal: None, line: 1 },
            Box::new(Expr::Literal(LiteralValue::Boolean(true)))
        );
        assert_eq!(evaluate(&expr).unwrap(), Value::Boolean(false));
    }

    #[test]
    fn test_grouping() {
        let expr = Expr::Grouping(Box::new(Expr::Literal(LiteralValue::Number(42.0))));
        assert_eq!(evaluate(&expr).unwrap(), Value::Number(42.0));
    }

    #[test]
    fn test_runtime_errors() {
        // Division by zero
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::Slash,
            Expr::Literal(LiteralValue::Number(0.0))
        );
        assert!(evaluate(&expr).is_err());

        // Invalid arithmetic operation
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::String("hello".to_string())),
            TokenType::Minus,
            Expr::Literal(LiteralValue::Number(5.0))
        );
        assert!(evaluate(&expr).is_err());

        // Invalid comparison
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::String("hello".to_string())),
            TokenType::Greater,
            Expr::Literal(LiteralValue::Number(5.0))
        );
        assert!(evaluate(&expr).is_err());

        // Unary minus on non-number
        let expr = Expr::Unary(
            Token { token_type: TokenType::Minus, lexeme: String::new(), literal: None, line: 1 },
            Box::new(Expr::Literal(LiteralValue::String("hello".to_string())))
        );
        assert!(evaluate(&expr).is_err());
    }
}
