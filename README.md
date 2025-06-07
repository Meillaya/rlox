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

## References

- [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom
- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [Rust Reference](https://doc.rust-lang.org/reference/)
- [Codecrafters](https://app.codecrafters.io/courses/interpreter) For testing
