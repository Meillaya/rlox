use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;

mod tokenizer;
mod parser;

use tokenizer::{Tokenizer, TokenType};
use parser::{Parser, print_ast};

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
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                process::exit(1);
            });

            if !file_contents.is_empty() {
                let mut tokenizer = Tokenizer::new(&file_contents);
                let tokens = tokenizer.scan_tokens();

                if tokenizer.has_error {
                    process::exit(65);
                }

                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(expr) => println!("{}", print_ast(&expr)),
                    Err(error) => {
                        eprintln!("Error: {}", error);
                        process::exit(65);
                    }
                }
            } else {
                println!("EOF");
            }
        },
    "evaluate" => {
        

        },
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            process::exit(1);
        }
    }
}