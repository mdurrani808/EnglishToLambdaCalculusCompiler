# Lambda Calculus Interpreter and English-to-Lambda Compiler

This project implements a comprehensive Lambda Calculus interpreter and an English-to-Lambda Calculus compiler. 

## Project Overview

The project consists of three main components:

1. **Lexer (Tokenizer)**: Converts input strings into token lists for both Lambda Calculus and English expressions.
2. **Parser**: Generates Abstract Syntax Trees (ASTs) from token lists.
3. **Evaluator/Generator**: Includes an interpreter for Lambda Calculus and a compiler for English-to-Lambda Calculus conversion.

## Key Features

- **Lambda Calculus Interpreter**:
  - Alpha equivalence checking
  - Beta reduction to normal form
  - Lazy and eager evaluation strategies
- **English-to-Lambda Calculus Compiler**:
  - Implements Church encodings for boolean logic
  - Converts between English expressions and Lambda Calculus
- **Custom Parser and Lexer**:
  - Handles both Lambda Calculus and English syntax
  - Implements LL(1) parsing

## Technical Highlights

- **Language**: OCaml
- **Parsing Technique**: LL(1) parsing
- **Evaluation Strategies**: Both lazy and eager evaluation implemented
- **Type System**: Leverages OCaml's strong type system for robust implementation

## Code Structure

- `lexer.ml`: Implementation of lexical analysis
- `parser.ml`: Parsing logic for both languages
- `eval.ml`: Evaluation and compilation functions
- `lccTypes.ml`: Type definitions for ASTs and tokens

## Future Enhancements

- Extend the language to include more complex constructs
- Implement optimizations for beta reduction
- Add a REPL (Read-Eval-Print Loop) interface for interactive use