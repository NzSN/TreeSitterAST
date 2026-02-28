# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

TreeSitterAST is a Haskell-based code generator that creates strongly-typed Abstract Syntax Tree (AST) definitions and processing code from Tree-sitter grammar definitions. It parses Tree-sitter's `node-types.json` and `grammar.json` files and generates TypeScript code for working with AST nodes. The tool is language-agnostic and can process any Tree-sitter grammar.

Two main modes:
- **AST Processor (`--ast-proc`)**: Generates TypeScript class declarations and node processors from `node-types.json`
- **Code Generator (`--code-gen`)**: Generates TypeScript classes and grammar representations from `grammar.json`

## Common Commands

### Building and Running

```bash
# Build the project
cabal build

# Run the executable (after building)
cabal run TreeSitterAST -- --ast-proc sample/node-types.json
cabal run TreeSitterAST -- --code-gen sample/grammar.json

# Install the executable globally
cabal install
```

### Testing

```bash
# Run all tests
cabal test

# Run specific test suite (all tests are under one suite)
cabal test TreeSitterAST-test

# Build and run tests in one step
cabal test --test-show-details=streaming
```

### Development

```bash
# Start a REPL with the library loaded
cabal repl lib:TreeSitterAST

# Run the executable with arguments in the REPL
cabal run TreeSitterAST -- --ast-proc sample/node-types.json

# Clean build artifacts
cabal clean
```

### Single Test Execution

To run a single test module, you can use `cabal test` with the `--test-option` flag, but the test suite is structured as a single Tasty test group. Instead, run the test binary directly after building:

```bash
cabal build
dist-newstyle/build/x86_64-linux/ghc-<version>/TreeSitterAST-0.1.0.0/t/TreeSitterAST-test/build/TreeSitterAST-test/TreeSitterAST-test
```

## Architecture

The system is organized into several layers:

### 1. Parsing Layer
- **`TreeSitterNodes`**: Parses `node-types.json` into a structured Haskell type `Node` (leaf/interior nodes with metadata).
- **`TreeSitterGrammarNodes`**: Parses `grammar.json` into a `Grammar` type with `GrammarNode` hierarchy (SEQ, CHOICE, SYMBOL, etc.). Includes alias resolution, trimming externals, and traversal utilities.

### 2. Generation Layer
- **`TypedASTGenerator.NodeDescription`**: Generates `node_declare.ts` – TypeScript class declarations for AST nodes.
- **`TypedASTGenerator.NodeProcessorDescription`**: Generates `node_processor.ts` – TypeScript node processors for AST traversal.
- **`ProgBuilder.ECMA.ProgBuilderForECMA`**: Generates `grammar_classes.ts` – TypeScript classes for syntactic nodes derived from grammar.

### 3. Template System
- **`Template.Template`**: Generic template wrapper around `Formatting` library for building reusable code templates.
- **`Template.TypeScriptTemplate`**: TypeScript‑specific templates (class declarations, method signatures, imports, etc.) used by the generators.

### 4. Inference System
- **`Fundamentals.Inference`**: Evaluates grammar nodes into corresponding expressions, used during code generation.
- **`Fundamentals.File`**: Utilities for file I/O and path handling.

### 5. Utilities
- **`Utility`**: Common helper functions.
- **`TypedASTGenerator.NodeDescriptionHelper`**: Shared helpers for node description generation.

## Key Data Types

- **`TreeSitterNodes.Node`**: Represents a node from `node-types.json`; either `Leaf NodeInfo` or `Interior NodeInfo Children (Maybe FieldName) (Maybe Precedence)`.
- **`TreeSitterGrammarNodes.GrammarNode`**: Recursive grammar node type with variants: `Leaf`, `Seq`, `Choice`, `Symbol`, `Repeat`, `Repeat1`, `Optional`, `Field`, `Alias`, `Blank`, `Precedence`, `Immmediate`.
- **`Grammar`**: Map from symbol names to `GrammarNode`s, plus external rules and precedence sets.

## Workflow

1. **Input**: Provide a `node-types.json` or `grammar.json` file (e.g., from a Tree‑sitter grammar).
2. **Parsing**: The appropriate parser (`TreeSitterNodes` or `TreeSitterGrammarNodes`) reads the JSON and produces a Haskell value.
3. **Generation**: The generator (`NodeDescription`, `NodeProcessorDescription`, `ProgBuilderForECMA`) walks the parsed structure and applies TypeScript templates.
4. **Output**: Writes generated `.ts` files to the current directory.

## Project Structure

```
TreeSitterAST/
├── app/
│   ├── Main.hs          # Entry point, dispatches by mode
│   └── Args.hs          # Command‑line argument parsing
├── src/
│   ├── Fundamentals/    # Inference, file utilities
│   ├── ProgBuilder/     # Grammar‑based code generation
│   ├── Template/        # Template system
│   ├── TypedASTGenerator/ # AST‑node‑based generation
│   ├── TreeSitterNodes.hs          # node‑types.json parser
│   ├── TreeSitterGrammarNodes.hs   # grammar.json parser
│   └── Utility.hs
├── test/                # Tasty test suite
├── sample/              # Example input files
│   ├── node-types.json
│   └── grammar.json
└── hie.yaml            # Haskell IDE engine configuration
```

## Notes for Developers

- The codebase uses **GHC 9.14.1** and **GHC2024** language standard.
- All modules are exported in the library (see `TreeSitterAST.cabal`).
- The test suite uses **Tasty** with **HUnit** and **QuickCheck**.
- When adding new grammar node types, extend `TreeSitterGrammarNodes.GrammarNode` and update the parser (`parseNodeFromJSON`), inference rules (`Fundamentals.Inference`), and the ProgBuilder.
- The template system relies on the `formatting` library; new templates should be added to `Template.TypeScriptTemplate`.
- The repository includes a sample JavaScript grammar; you can replace it with any other Tree‑sitter grammar JSON files.

## Dependencies

Key Haskell dependencies (listed in `.cabal` file):
- `aeson` – JSON parsing
- `text`, `bytestring` – string handling
- `containers` – Map/Set data structures
- `formatting` – text templating
- `optparse-applicative` – command‑line parsing
- `template-haskell` – (potential future use)
- `tasty`, `HUnit`, `QuickCheck` – testing

## Recent Development Focus

Recent commits have focused on:
- Inference rules for evaluating grammar nodes into expressions
- Grammar node traversal (`Traversable` instance)
- Property conflict resolution during code generation
- Alias resolution functions