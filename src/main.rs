use std::cell::RefCell;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;
use std::rc::Rc;

mod tokenizer;
mod parser;
mod evaluator;

use tokenizer::{Tokenizer, TokenType, Token};
use parser::{Parser, print_ast};
use evaluator::{Environment,execute_stmt};

fn read_and_tokenize(filename: &str) -> Result<Vec<Token>, String> {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
        process::exit(1);
    });

    if file_contents.is_empty() {
        return Ok(vec![Token {
            token_type: TokenType::EOF,
            lexeme: String::new(),
            literal: None,
            line: 1,
        }]);
    }

    let mut tokenizer = Tokenizer::new(&file_contents);
    let tokens = tokenizer.scan_tokens();

    if tokenizer.has_error {
        return Err("Tokenization error".to_string());
    } else {
        Ok(tokens)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                process::exit(1);
            });

            if !file_contents.is_empty() {
                
                let mut tokenizer = Tokenizer::new(&file_contents);

                let tokens = tokenizer.scan_tokens();
                for token in tokens {

                    if token.token_type != TokenType::WhiteSpace{
                        println!("{} {} {}", token.token_type, token.lexeme, token.literal.as_deref().unwrap_or("null"));
                    }
                }
            if tokenizer.has_error {
                    std::process::exit(65);
                }
            } else {
                println!("EOF  null");
            }
        },
        "parse" => {
            match read_and_tokenize(filename) {
                Ok(tokens) => {
                    let mut parser = Parser::new(tokens);
                    match parser.parse() {
                        Ok(statements) => {
                            if let Some(stmt) = statements.first() {
                                if let parser::Stmt::Expression(expr) = stmt {
                                    println!("{}", print_ast(expr));
                                } else {
                                    println!("First statement is not an expression");
                                }
                            } else {
                                println!("No statements to print");
                            }
                        },
                        Err(error) => {
                            eprintln!("Error: {}", error);
                            process::exit(65);
                        }
                    }
                },
                Err(error) => {
                    eprintln!("Error: {}", error);
                    process::exit(65);
                }
            }
        },
        "evaluate" => {
            match read_and_tokenize(filename) {
                Ok(tokens) => {
                    let mut parser = Parser::new(tokens);
                    match parser.parse() {
                        Ok(statements) => {
                            let env = Rc::new(RefCell::new(Environment::new()));
                            env.borrow_mut().define_natives(); // Add this line
                            for stmt in statements {
                                match execute_stmt(&stmt, true, Rc::clone(&env)) {
                                    Ok(_) => {},
                                    Err(runtime_error) => {
                                        eprintln!("{} [line {}]", runtime_error.message, runtime_error.line);
                                        process::exit(70);
                                    }
                                }
                            }
                        },
                        Err(error) => {
                            eprintln!("Error: {}", error);
                            process::exit(65);
                        }
                    }
                },
                Err(error) => {
                    eprintln!("Error: {}", error);
                    process::exit(65);
                }
            }
        },
        "run" => {
            match read_and_tokenize(filename) {
                Ok(tokens) => {
                    let mut parser = Parser::new(tokens);
                    match parser.parse() {
                        Ok(statements) => {
                            let env = Rc::new(RefCell::new(Environment::new()));
                            env.borrow_mut().define_natives(); // Add this line
                            for stmt in statements {
                                match execute_stmt(&stmt, false, Rc::clone(&env)) {
                                    Ok(_) => {},
                                    Err(runtime_error) => {
                                        eprintln!("{} [line {}]", runtime_error.message, runtime_error.line);
                                        process::exit(70);
                                    }
                                }
                            }
                        },
                        Err(error) => {
                            eprintln!("Error: {}", error);
                            process::exit(65);
                        }
                    }
                },
                Err(error) => {
                    eprintln!("Error: {}", error);
                    process::exit(65);
                }
            }
        },
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            process::exit(1);
        }
    }
}