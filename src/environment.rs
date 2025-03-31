use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::tokenizer::Token;
use crate::evaluator::{Value, RuntimeError};

#[derive(Debug, PartialEq)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name_token: &Token) -> Result<Value, RuntimeError> {
        if let Some(value) = self.values.get(&name_token.lexeme) {
            Ok(value.clone())
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.borrow().get(name_token)
        } else {
            Err(RuntimeError::Error {
                message: format!("Undefined variable '{}'.", name_token.lexeme),
                line: name_token.line,
            })
        }
    }

    pub fn get_at(&self, distance: usize, name: &str) -> Option<Value> {
        if distance == 0 {
            self.values.get(name).cloned()
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get_at(distance - 1, name)
        } else {
            None
        }
    }

    pub fn assign(&mut self, name_token: &Token, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name_token.lexeme) {
            self.values.insert(name_token.lexeme.clone(), value);
            Ok(())
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.borrow_mut().assign(name_token, value)
        } else {
            Err(RuntimeError::Error {
                message: format!("Undefined variable '{}'.", name_token.lexeme),
                line: name_token.line,
            })
        }
    }

    pub fn assign_at(&mut self, distance: usize, name_token: &Token, value: Value) -> Result<(), RuntimeError> {
        if distance == 0 {
            if self.values.contains_key(&name_token.lexeme) {
                self.values.insert(name_token.lexeme.clone(), value);
                Ok(())
            } else {
                Err(RuntimeError::Error {
                    message: format!("Undefined variable '{}' at the specified scope.", name_token.lexeme),
                    line: name_token.line,
                })
            }
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign_at(distance - 1, name_token, value)
        } else {
            Err(RuntimeError::Error {
                message: format!("Cannot assign at distance {}.", distance),
                line: name_token.line,
            })
        }
    }

    pub fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define_natives(&mut self) {
        self.define("clock".to_string(), Value::NativeFunction(|| {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap();
            Value::Number(now.as_secs_f64())
        }));
    }
}
