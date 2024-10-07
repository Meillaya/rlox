use crate::parser::{Expr, LiteralValue};
use crate::tokenizer::TokenType;

pub fn evaluate(expr: &Expr) -> String {

    match expr {
        Expr::Literal(literal) => match literal {
            LiteralValue::Boolean(value) => value.to_string(),
            LiteralValue::Number(value) => {
                if value.fract() == 0.0 {
                    format!("{}", value.trunc())
                } else {
                    format!("{:?}", value)
                }
            },
            LiteralValue::String(value) => format!("{}", value),
            LiteralValue::Nil => "nil".to_string(),
        },
        Expr::Grouping(expr) => evaluate(expr),
        Expr::Unary(operator, expr) => {
            let right = evaluate(expr);
            match operator.token_type.clone() {
                TokenType::Minus => {
                    if let Ok(value) = right.parse::<f64>() {
                        (-value).to_string()
                    } else {
                        "NaN".to_string()
                    }
                },
                TokenType::Bang => {
                    (!is_truthy(&right)).to_string()
                },
                _ => "Unimplemented".to_string(),
            }
        },
        Expr::Binary(left, operator, right) => {
            let left = evaluate(left);
            let right = evaluate(right);
            match operator.token_type.clone() {
                TokenType::Plus => {
                    if let (Ok(left_num), Ok(right_num)) = (left.parse::<f64>(), right.parse::<f64>()) {
                        (left_num + right_num).to_string()
                    } else  {
                        format!("{}{}", left, right)
                    }
                },
                TokenType::Minus => {
                    if let (Ok(left_num), Ok(right_num)) = (left.parse::<f64>(), right.parse::<f64>()) {
                        (left_num - right_num).to_string()
                    } else  {
                        "NaN".to_string()
                    }
                },
                TokenType::Star => {
                    if let (Ok(left_num), Ok(right_num)) = (left.parse::<f64>(), right.parse::<f64>()) {
                        (left_num * right_num).to_string()
                    } else  {
                        "NaN".to_string()
                    }
                },
                TokenType::Slash => {
                    if let (Ok(left_num), Ok(right_num)) = (left.parse::<f64>(), right.parse::<f64>()) {
                        (left_num / right_num).to_string()
                    } else  {
                        "NaN".to_string()
                    }
                },
                TokenType::Greater => {
                    compare_values(&left, &right, |a, b| a > b)
                },
                TokenType::GreaterEqual => {
                    compare_values(&left, &right, |a, b| a >= b)
                },
                TokenType::Less => {
                    compare_values(&left, &right, |a, b| a < b)
                },
                TokenType::LessEqual => {
                    compare_values(&left, &right, |a, b| a <= b)
                },
                TokenType::EqualEqual => {
                    compare_equality(&left, &right)
                },
                TokenType::BangEqual => {
                    (!compare_equality(&left, &right).parse::<bool>().unwrap_or(false)).to_string()
                },
                _ => "Unimplemented".to_string(),
            }
        },
     _ => "Unimplemented".to_string(),
    }
}

fn compare_equality(left: &str, right: &str) -> String {
    println!("Comparing: '{}' and '{}'", left, right);
    // Try parsing both as numbers
    match (left.parse::<f64>(), right.parse::<f64>()) {
        (Ok(left_num), Ok(right_num)) => {
            // Both are valid numbers, compare them
            println!("Both are numbers: {} and {}", left_num, right_num);
            let result = left_num == right_num;
            println!("Numeric comparison result: {}", result);
            result.to_string()
        },
        (Ok(left_num), Err(_)) => {
            // Left is a number, right is a string
            println!("Left is number: {}, Right is string: '{}'", left_num, right);
            let result = format!("{}", left_num) == right;
            println!("String comparison result: {}", result);
            result.to_string()
        },
        (Err(_), Ok(right_num)) => {
            // Left is a string, right is a number
            println!("Left is string: '{}', Right is number: {}", left, right_num);
            let result = left == format!("{}", right_num);
            println!("String comparison result: {}", result);
            result.to_string()
        },
        (Err(_), Err(_)) => {
            // Neither are numbers, compare as strings
            println!("Both are strings: '{}' and '{}'", left, right);
            let result = left == right;
            println!("String comparison result: {}", result);
            result.to_string()
        }
    }
}





fn compare_values(left: &str, right: &str, compare: fn(f64, f64) -> bool) -> String {
    if let (Ok(left_num), Ok(right_num)) = (left.parse::<f64>(), right.parse::<f64>()) {
        compare(left_num, right_num).to_string()
    } else {
        "false".to_string()
    }
}
fn is_truthy(value: &str) -> bool {
    match value {
        "false" | "nil" => false,
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
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::Number(42.0))), "42");
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::String("hello".to_string()))), "hello");
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::Boolean(true))), "true");
        assert_eq!(evaluate(&Expr::Literal(LiteralValue::Nil)), "nil");
    }

    #[test]
    fn test_arithmetic_operations() {
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::Plus,
            Expr::Literal(LiteralValue::Number(3.0))
        );
        assert_eq!(evaluate(&expr), "8");

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(10.0)),
            TokenType::Minus,
            Expr::Literal(LiteralValue::Number(4.0))
        );
        assert_eq!(evaluate(&expr), "6");

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(3.0)),
            TokenType::Star,
            Expr::Literal(LiteralValue::Number(4.0))
        );
        assert_eq!(evaluate(&expr), "12");

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(15.0)),
            TokenType::Slash,
            Expr::Literal(LiteralValue::Number(3.0))
        );
        assert_eq!(evaluate(&expr), "5");
    }

    #[test]
    fn test_comparison_operations() {
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::Greater,
            Expr::Literal(LiteralValue::Number(3.0))
        );
        assert_eq!(evaluate(&expr), "true");

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::LessEqual,
            Expr::Literal(LiteralValue::Number(5.0))
        );
        assert_eq!(evaluate(&expr), "true");

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(10.0)),
            TokenType::Less,
            Expr::Literal(LiteralValue::Number(20.0))
        );
        assert_eq!(evaluate(&expr), "true");
    }

    #[test]
    fn test_equality_operations() {
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(25.0)),
            TokenType::EqualEqual,
            Expr::Literal(LiteralValue::String("25".to_string()))
        );
        assert_eq!(evaluate(&expr), "false");

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(25.0)),
            TokenType::EqualEqual,
            Expr::Literal(LiteralValue::Number(25.0))
        );
        assert_eq!(evaluate(&expr), "true");

        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::String("hello".to_string())),
            TokenType::BangEqual,
            Expr::Literal(LiteralValue::String("world".to_string()))
        );
        assert_eq!(evaluate(&expr), "true");
    }

    #[test]
    fn test_unary_operations() {
        let expr = Expr::Unary(
            Token { token_type: TokenType::Minus, lexeme: String::new(), literal: None, line: 1 },
            Box::new(Expr::Literal(LiteralValue::Number(5.0)))
        );
        assert_eq!(evaluate(&expr), "-5");

        let expr = Expr::Unary(
            Token { token_type: TokenType::Bang, lexeme: String::new(), literal: None, line: 1 },
            Box::new(Expr::Literal(LiteralValue::Boolean(true)))
        );
        assert_eq!(evaluate(&expr), "false");
    }

    #[test]
    fn test_grouping() {
        let expr = Expr::Grouping(Box::new(Expr::Literal(LiteralValue::Number(42.0))));
        assert_eq!(evaluate(&expr), "42");
    }

    #[test]
    fn test_runtime_errors() {
        // Division by zero
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::Number(5.0)),
            TokenType::Slash,
            Expr::Literal(LiteralValue::Number(0.0))
        );
        assert_eq!(evaluate(&expr), "inf");

        // Invalid arithmetic operation
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::String("hello".to_string())),
            TokenType::Minus,
            Expr::Literal(LiteralValue::Number(5.0))
        );
        assert_eq!(evaluate(&expr), "NaN");

        // Invalid comparison
        let expr = create_binary_expr(
            Expr::Literal(LiteralValue::String("hello".to_string())),
            TokenType::Greater,
            Expr::Literal(LiteralValue::Number(5.0))
        );
        assert_eq!(evaluate(&expr), "false");
    }
}
