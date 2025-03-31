# Lox Interpreter in Rust

A complete implementation of the Lox programming language interpreter in Rust, based on the design described in the book "Crafting Interpreters" by Robert Nystrom.

## Overview

This project implements a fully-featured interpreter for the Lox programming language, a dynamically-typed scripting language with C-like syntax. The implementation follows the tree-walk interpreter design described in the first part of "Crafting Interpreters", but implemented in Rust instead of Java.

### Lox Language Features

- Dynamically typed
- Automatic memory management
- C-like syntax
- Variables and expressions
- Control flow (if/else, while, for)
- Functions with closures
- Classes with inheritance
- Methods and properties
- First-class functions

## Getting Started

### Prerequisites

- Rust toolchain (rustc, cargo)

### Building the Project

```bash
cargo build --release
```

### Running a Lox Script

```bash
cargo run -- run path/to/your/script.lox
```

### Additional Commands

```bash
# Tokenize a Lox script (showing the tokens)
cargo run -- tokenize path/to/script.lox

# Parse a Lox script (showing the AST)
cargo run -- parse path/to/script.lox

# Evaluate a Lox script (with expression printouts)
cargo run -- evaluate path/to/script.lox
```


## Technical Implementation Details

The interpreter is implemented in several distinct phases, following the architecture described in "Crafting Interpreters":

### 1. Scanning/Tokenization (tokenizer.rs)

The tokenizer converts the source code into a sequence of tokens. Each token has:
- Token type (identifier, keyword, operator, etc.)
- Lexeme (the actual string content)
- Literal value (for strings and numbers)
- Line number (for error reporting)

The scanner handles lexical analysis, including:
- Keywords and identifiers
- String and number literals
- Operators and punctuation
- Comments (single line `//` style)
- Whitespace handling

### 2. Parsing (parser.rs)

The parser converts the token stream into an Abstract Syntax Tree (AST). It implements a recursive descent parser for handling:
- Expressions (binary, unary, grouping, literals, etc.)
- Statements (print, expression, variable declaration, etc.)
- Control flow (if, while, for)
- Functions and returns
- Classes with inheritance

The parser generates two primary AST node types:
- `Expr` - Various expression types (literals, binary operations, etc.)
- `Stmt` - Statement types (blocks, variable declarations, etc.)

### 3. Static Analysis (resolver.rs)

Before execution, the resolver performs static analysis to:
- Resolve variable scopes
- Detect variable usage errors
- Prepare environment lookups for optimization
- Validate class and function declarations
- Check for 'this' and 'super' usage in appropriate contexts

The resolver creates a mapping of variable references to their scope depths, which the interpreter uses for efficient variable lookups.

### 4. Runtime Environment (environment.rs)

The environment handles variable storage and lookup at runtime:
- Hierarchical scopes with enclosing environments
- Variable definition and assignment
- Direct lookup by name or by resolved scope distance
- Error reporting for undefined variables

### 5. Evaluation (evaluator.rs)

The evaluator executes the AST, implementing:
- Expression evaluation
- Statement execution
- Function calls and returns
- Method calls and property access
- Class instantiation and inheritance
- Native functions (e.g., 'clock()')
- Runtime error handling

Key features of the evaluator:
- Environment-based lexical scoping
- First-class functions with closures
- Dynamic dispatch for callable objects
- Reference-counting (Rc<T>) for memory management
- Class inheritance and super calls

## Implementation Insights

### Memory Management

The interpreter uses Rust's ownership system and smart pointers:
- `Rc<T>` for shared ownership (environments, functions, etc.)
- `RefCell<T>` for interior mutability (environments at runtime)
- `Box<T>` for recursive AST structures

### Closures

Functions in Lox capture their environment, allowing for closures:
- Each function stores its declaration and the environment when it was defined
- When called, the function creates a new environment chained to its closure environment

### Classes and Inheritance

The class system supports:
- Method definitions
- Instances with fields
- Special 'init' constructor method
- Single inheritance via 'super'
- Method overriding
- Method binding to 'this'

### Error Handling

The interpreter provides detailed error handling:
- Syntax errors with line numbers
- Runtime errors with descriptive messages
- Static analysis errors for scope-related issues

## References

- [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom
- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [Rust Reference](https://doc.rust-lang.org/reference/)
- [Codecrafters](https://app.codecrafters.io/courses/interpreter) For testing
