# Rust Interpreter Implementation

A robust interpreter implementation in Rust that supports expression evaluation, variable management, and control flow statements.

## Features

- **Expression Evaluation**
  - Arithmetic operations (+, -, *, /)
  - String concatenation
  - Comparison operators (<, >, <=, >=, ==, !=)
  - Logical operators (!, or)
  - Grouping expressions with parentheses

- **Variable Management**
  - Variable declaration and initialization
  - Variable assignment
  - Scoped variables with block support
  - Lexical scoping

- **Control Flow**
  - If-else statements
  - Block statements with local scope
  - Print statements

- **Type System**
  - Numbers (64-bit floating point)
  - Strings
  - Booleans
  - Nil values

## Setup


1. Ensure you have Rust 1.77 or later installed
2. Clone the repository
3. Build the project:


```
cargo build --release
```

## Usage Examples

### Basic Arithmetic

// Addition and multiplication
```print 2 + 3 * 4;  // Outputs: 14```

// String concatenation
```print "Hello " + "World";  // Outputs: Hello World```


### Variables

```
var x = 10;
var y = 20;
print x + y;  // Outputs: 30

// Variable scoping
{
    var x = 5;
    print x;  // Outputs: 5
}
print x;  // Outputs: 10
```


### Control Flow

```
var condition = true;
if (condition) {
    print "Condition is true";
} else {
    print "Condition is false";
}

// Logical operators
if (true or false) {
    print "At least one is true";
}
```

## Error Handling

The interpreter provides clear error messages for common issues:

- Division by zero
- Undefined variables
- Type mismatches
- Invalid operations

## Running the Interpreter

Use the provided script to run your program:


```./your_program.sh <command> <filename>```


Available commands:

- tokenize: Display tokens from source file
- parse: Show AST representation
- evaluate: Execute and show expression results
- run: Execute the program

## Development

The interpreter is built with a clear separation of concerns:

- Tokenizer: Lexical analysis
- Parser: Syntax analysis and AST construction
- Evaluator: Expression evaluation and statement execution
- Environment: Variable and scope management