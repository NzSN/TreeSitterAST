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
# Specify output directory with --output-dir (or -o)
cabal run TreeSitterAST -- --ast-proc sample/node-types.json --output-dir generated

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

# Run the executable with arguments
cabal run TreeSitterAST -- --ast-proc sample/node-types.json
cabal run TreeSitterAST -- --code-gen sample/grammar.json
# Specify output directory with --output-dir (or -o)
cabal run TreeSitterAST -- --ast-proc sample/node-types.json --output-dir generated

# Clean build artifacts
cabal clean

# Check project dependencies
cabal freeze

# Update dependencies
cabal update
```

### Single Test Execution

The test suite is structured as a single Tasty test group, so `cabal test` runs all tests. To run a specific test module or pattern, run the test binary directly after building:

```bash
# Build the project first
cabal build

# Run all tests via the test binary
dist-newstyle/build/x86_64-linux/ghc-*/TreeSitterAST-0.1.0.0/t/TreeSitterAST-test/build/TreeSitterAST-test/TreeSitterAST-test

# Run tests matching a pattern (e.g., only inference tests)
dist-newstyle/build/x86_64-linux/ghc-*/TreeSitterAST-0.1.0.0/t/TreeSitterAST-test/build/TreeSitterAST-test/TreeSitterAST-test -p "Inference"

# List available test patterns
dist-newstyle/build/x86_64-linux/ghc-*/TreeSitterAST-0.1.0.0/t/TreeSitterAST-test/build/TreeSitterAST-test/TreeSitterAST-test --list-tests
```

## Architecture

The system is organized into several layers:

### 1. Parsing Layer
- **`TreeSitterNodes`**: Parses `node-types.json` into a structured Haskell type `Node` (leaf/interior nodes with metadata).
- **`TreeSitterGrammarNodes`**: Parses `grammar.json` into a `Grammar` type with `GrammarNode` hierarchy (SEQ, CHOICE, SYMBOL, etc.). Includes alias resolution, trimming externals, and traversal utilities.

### 2. Generation Layer
- **`TypedASTGenerator.NodeDescription`**: Generates `node_declare.ts` – TypeScript class declarations for AST nodes.
- **`TypedASTGenerator.NodeProcessorDescription`**: Generates `node_processor.ts` – TypeScript node processors for AST traversal.
- **`ProgBuilder.ECMA.ProgBuilderForECMA`**: Generates `<grammar_name>_rep.ts` – TypeScript classes for syntactic nodes derived from grammar.

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
- **`Property`** (in `ProgBuilderDescription`): Represents extracted properties from grammar nodes - `StrProp`, `SymbolProp`, or `NamedProp` with generation hints.
- **`Field`** (in `ProgBuilderForECMA`): Represents TypeScript class fields - `Field`, `SumField`, or `EmptyField` with names and types.
- **`GenerationStrategy`** (in `Fundamentals.Generation`): Controls sentence generation - `RandomStrategy`, `GuidedStrategy`, or `ExhaustiveStrategy`.

## Data Flow Patterns

### AST Processor Mode (`--ast-proc`)
```
node-types.json → TreeSitterNodes.parse → [Node] →
  NodeDescription.generate → node_declare.ts
  NodeProcessorDescription.generate → node_processor.ts
```

### Code Generator Mode (`--code-gen`)
```
grammar.json → TreeSitterGrammarNodes.parseGrammarFromJSON → Grammar →
  Fundamentals.Inference.trans (transform CHOICE nodes) →
  ProgBuilderDescription.propsOfNode (extract properties) →
  ProgBuilderForECMA.fieldsFromProperties (convert to fields) →
  ProgBuilderForECMA.build (generate class with evaluate() and factories) →
  <grammar_name>_rep.ts
```

### Property to Field Conversion
The critical conversion happens in `fieldsFromProperties`:
1. `[Property]` from `propsOfNode` (preserving original order)
2. Filter out `StrProp` (string literals, no field needed)
3. Convert remaining to `Field` with sequential suffixes (`_0`, `_1`, etc.)
4. All generation components use same `[Field]` list for consistency

## Workflow

1. **Input**: Provide a `node-types.json` or `grammar.json` file (e.g., from a Tree‑sitter grammar).
2. **Parsing**: The appropriate parser (`TreeSitterNodes` or `TreeSitterGrammarNodes`) reads the JSON and produces a Haskell value.
3. **Generation**: The generator (`NodeDescription`, `NodeProcessorDescription`, `ProgBuilderForECMA`) walks the parsed structure and applies TypeScript templates.
4. **Output**: Writes generated files to the current directory:
   - `--ast-proc` mode: `node_declare.ts` and `node_processor.ts`
   - `--code-gen` mode: `<grammar_name>_rep.ts` and `<grammar_name>_grammar.txt` (where `grammar_name` is the `name` field from the grammar.json file)

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

- The codebase uses **GHC2024** language standard and targets GHC 9.14.1 (based on `base <= 4.22.0.0` constraint).
- All modules are exported in the library (see `TreeSitterAST.cabal`).
- The test suite uses **Tasty** with **HUnit** and **QuickCheck**. Tests are organized into a single test group in `test/Main.hs`.
- When adding new grammar node types, extend `TreeSitterGrammarNodes.GrammarNode` and update:
  1. The parser (`parseNodeFromJSON` in `TreeSitterGrammarNodes`)
  2. Inference rules (`Fundamentals.Inference`)
  3. The ProgBuilder (`ProgBuilder.ECMA.ProgBuilderForECMA`)
- The template system relies on the `formatting` library; new templates should be added to `Template.TypeScriptTemplate`.
- The repository includes a sample JavaScript grammar; you can replace it with any other Tree‑sitter grammar JSON files.
- The executable has two modes: `--ast-proc` for node-type processing and `--code-gen` for grammar-based code generation (see `app/Args.hs`).

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
- **Sentence Generation System**: Complete implementation of `Fundamentals.Generation` with random, guided, and exhaustive strategies for generating sentences from grammar definitions.
- **Code Generator Enhancements**: `ProgBuilder.ECMA.ProgBuilderForECMA` now generates functional `evaluate()` methods and factory methods (`static createX()`) in TypeScript classes.
- **String Escaping Fix**: Added proper escaping of special characters in generated TypeScript string literals (double quotes and backslashes).
- **New Modules**: Added `ProgBuilder.GenerationDriver` for orchestration and `Validation.Validator` for sentence validation.
- **Property System**: Enhanced `ProgBuilder.ProgBuilderDescription` with `GenerationHint` type for smarter code generation.
- **Bug Fixes**: Fixed class name mismatches, parameter formatting, and property reference issues in generated TypeScript code.
- **Field Generation Fix**: Resolved type mismatches in constructor parameters where field names were incorrectly used as types instead of field content types (e.g., `Value_T` vs `Expression_T`).
- **Union Type Evaluate Fix**: Added type checking for string types in union types within `evaluate()` methods (e.g., `Identifier_T | string | _destructuring_pattern_T`).

## Architecture Documents

For detailed architecture understanding, refer to:
- `spec/SENTENCE_GENERATION_ARCHITECTURE.md` - Overall system design for sentence generation
- `spec/IMPLEMENTATION_SUMMARY.md` - Complete implementation plan and component breakdown
- `spec/FIELD_GENERATION_FIX_PROGRESS.md` - Documentation of field generation fixes
- `spec/UNION_TYPE_EVALUATE_FIX.md` - Fix for string type handling in evaluate() methods
- `spec/CHOICE_REPETITION_HANDLING.md` - Algorithms for choice resolution and repetition
- `spec/EVALUATE_IMPLEMENTATIONS.md` - Detailed evaluate() method designs
- `spec/GENERATION_DRIVER_VALIDATION.md` - Driver system and validation infrastructure

All generated documentation markdown files should be placed in the `spec/` directory.

## Generated Code Capabilities

The Code Generator (`--code-gen` mode) now produces TypeScript classes that can:
1. **Generate sentences** via `evaluate()` methods that traverse grammar nodes
2. **Create instances** via static factory methods (e.g., `static createYield_expression_t()`)
3. **Handle all grammar constructs**: SEQ (concatenation), CHOICE (alternatives), REPEAT/REPEAT1, SYMBOL (references), FIELD, ALIAS, etc.
4. **Include string literals** directly in generated output (e.g., `"yield"` in `Yield_expression_T.evaluate()`)
5. **Properly escape special characters** in string literals (double quotes as `\"`, backslashes as `\\`)
6. **Support random generation strategy** with configurable depth limits

Example generated class:
```typescript
export class Yield_expression_T extends SyntaticInterior {
  expression_0_i: Expression_T;
  constructor(expression_0: Expression_T) {
    super();
    this.expression_0_i = expression_0;
  }
  evaluate(): string {
    return "yield" + this.expression_0_i.evaluate();
  }
  static createYield_expression_t(expression_0: Expression_T): Yield_expression_T {
    const instance = new Yield_expression_T();
    instance.expression_0_i = expression_0;
    return instance;
  }
}

## Common Issues and Solutions

### Type Mismatches in Constructor Parameters
**Problem**: Constructor parameters have wrong types (e.g., `Value_T` instead of `Expression_T`).
**Cause**: Using field name as type instead of field content type.
**Solution**: Ensure `evalFieldType` extracts content type, not field name. Field nodes should use the type of their content, not the field name.

### Inconsistent Field Naming
**Problem**: Field declarations, constructor assignments, and evaluate() references use different naming.
**Cause**: Different components using different indexing logic.
**Solution**: Use `fieldsFromProperties` as single source of truth for all components.

### String Literal Escaping
**Problem**: Generated TypeScript has unescaped quotes or backslashes.
**Solution**: Use `escapeTypeScriptString` in `generateMemberCall` to properly escape `"` as `\"` and `\` as `\\`.

### Property Extraction Issues
**Problem**: Missing or incorrect properties in generated classes.
**Debug**: Check `propsOfNode` in `ProgBuilderDescription` to see what properties are extracted from grammar nodes.

### Test Pattern Execution
**Problem**: Running specific test patterns fails.
**Solution**: Build first with `cabal build`, then run test binary directly with pattern:
```bash
dist-newstyle/build/x86_64-linux/ghc-*/TreeSitterAST-0.1.0.0/t/TreeSitterAST-test/build/TreeSitterAST-test/TreeSitterAST-test -p "PatternName"
```

### Union Type Evaluate Issues
**Problem**: `evaluate()` method tries to call `.evaluate()` on string values in union types (e.g., `Identifier_T | string | _destructuring_pattern_T`).
**Cause**: Generated code doesn't check type before calling `.evaluate()`.
**Solution**: Use `evalFieldExpr` which generates type checking: `(typeof this.field === 'string' ? this.field : this.field.evaluate())`.
**Reference**: See `spec/UNION_TYPE_EVALUATE_FIX.md` for detailed implementation.

### Field Generation Consistency
**Key Principle**: All generation components (constructor, evaluate(), factory methods) must use the same `[Field]` list from `fieldsFromProperties` to ensure consistent naming and typing.
```