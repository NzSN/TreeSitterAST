# TreeSitterAST

A Haskell-based code generator that creates strongly-typed Abstract Syntax Tree (AST) definitions and processing code from Tree-sitter grammar definitions.

## Overview

TreeSitterAST parses Tree-sitter's `node-types.json` and `grammar.json` files and generates TypeScript code for working with AST nodes. The tool is language-agnostic and can process any Tree-sitter grammar, though the provided sample files are for JavaScript. It provides two main modes:

1. **AST Processor (`--ast-proc`)**: Generates TypeScript class declarations and node processors from `node-types.json`
2. **Code Generator (`--code-gen`)**: Generates TypeScript classes and grammar representations from `grammar.json`

## Features

- **Strongly-typed AST generation**: Creates TypeScript classes with proper type annotations
- **Grammar parsing**: Comprehensive parsing of Tree-sitter grammar JSON files
- **Template-based code generation**: Flexible template system for output customization
- **Inference system**: Evaluates grammar nodes to generate corresponding expressions
- **Modular design**: Separated parsing, generation, and template components
- **Test coverage**: Comprehensive test suite for core functionality

## Installation

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) 9.6 or later
- [Cabal](https://www.haskell.org/cabal/) 3.10 or later

### Building from Source

```bash
# Clone the repository
git clone <repository-url>
cd TreeSitterAST

# Build the project
cabal build

# Install the executable
cabal install
```

> **Note**: After `cabal build`, the executable is placed in `dist-newstyle/build/<arch>/<ghc-version>/TreeSitterAST-0.1.0.0/x/TreeSitterAST/build/TreeSitterAST/`. Use `cabal run TreeSitterAST` to run the executable directly, or `cabal install` to install it to your `~/.cabal/bin` directory.

## Usage

### Basic Commands

```bash
# Generate AST node declarations and processors from node-types.json
./TreeSitterAST --ast-proc sample/node-types.json

# Generate grammar-based code from grammar.json
./TreeSitterAST --code-gen sample/grammar.json
```

### Command-line Options

- `--ast-proc`: Generate both `node_processor.ts` and `node_declare.ts` from `node-types.json`
- `--code-gen`: Generate `grammar_nodes.txt` and `grammar_classes.ts` from `grammar.json`

### Output Files

#### AST Processor Mode (`--ast-proc`)
- `node_declare.ts`: TypeScript class declarations for AST nodes
- `node_processor.ts`: TypeScript node processors for AST traversal

#### Code Generator Mode (`--code-gen`)
- `grammar_nodes.txt`: Text representation of parsed grammar
- `grammar_classes.ts`: TypeScript classes for syntactic nodes

> **Note**: The repository already includes example generated files (`grammar_classes.ts` and `grammar_nodes.txt` in the root directory) from a previous run using the sample grammar.

## Project Structure

```
TreeSitterAST/
├── app/                    # Application entry points
│   ├── Main.hs            # Main executable
│   └── Args.hs            # Command-line argument parsing
├── src/                    # Core source code
│   ├── Fundamentals/      # Basic utilities and inference logic
│   ├── ProgBuilder/       # Program builder components
│   ├── Template/          # Code generation templates
│   ├── TypedASTGenerator/ # AST generation logic
│   ├── TreeSitterNodes.hs           # Parser for node-types.json
│   ├── TreeSitterGrammarNodes.hs    # Parser for grammar.json
│   └── Utility.hs
├── test/                   # Test suite
├── sample/                 # Example input files
│   ├── node-types.json    # Tree-sitter node type definitions (64KB)
│   └── grammar.json       # Tree-sitter grammar definition (JavaScript, 175KB)
├── grammar_classes.ts     # Generated output (TypeScript classes)
├── grammar_nodes.txt      # Generated output (grammar representation)
├── README.md              # This documentation file
├── LICENSE                # BSD 3-Clause License
└── TreeSitterAST.cabal    # Haskell package configuration
```

## Sample Input and Output

### Input Files

The `sample/` directory contains example input files:

- `sample/node-types.json`: Tree-sitter node type definitions (64KB)
- `sample/grammar.json`: Tree-sitter JavaScript grammar definition (175KB)

### Generated Output Example

```typescript
// Excerpt from grammar_classes.ts
export class SyntaticNode {
    evaluate(): string {
        throw Error("Interior or Leaf should implement evaluate().");
    }
}

export class SyntaticLeaf extends SyntaticNode {
    value_: string;
    constructor(value: string) {
        super();
        this.value_ = value;
    }
    evaluate(): string {
        return this.value_;
    }
}

export class With_statement_T extends SyntaticInterior {
    object_0_i: Parenthesized_expression_T;
    body_0_i: Statement_T;
    constructor() { super(); }
}
```

## Development

### Building and Testing

```bash
# Build the project
cabal build

# Run tests
cabal test

# Run specific test suites
cabal test TreeSitterAST-test
```

### Dependencies

Key Haskell dependencies include:
- `aeson` for JSON parsing
- `text` and `bytestring` for string handling
- `transformers` and `mtl` for monad transformers
- `containers` for data structures
- `formatting` for text formatting
- `template-haskell` for code generation
- `optparse-applicative` for command-line parsing

### Recent Development Focus

Recent commits show active development on:
- Inference logic for evaluating grammar nodes into expressions
- Grammar node traversal capabilities
- Property conflict resolution during code generation
- Alias resolution functions

## License

BSD 3-Clause License

Copyright (c) 2025, NzSN

See [LICENSE](LICENSE) file for full license text.

## Author

NzSN (nzsn0101@gmail.com)
