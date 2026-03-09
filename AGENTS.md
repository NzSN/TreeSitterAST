# TREE-SITTER AST KNOWLEDGE BASE

**Generated:** 2026-03-09 10:28:38  
**Commit:** c7c3e85  
**Branch:** main  

TreeSitterAST is a Haskell-based code generator that creates TypeScript AST definitions from Tree-sitter grammar files.

- **Language**: Haskell (GHC2024 standard)
- **Build System**: Cabal
- **Testing Framework**: Tasty with HUnit and QuickCheck

## Hierarchical Documentation

This is the root AGENTS.md file. For detailed documentation on specific directories, see:
- [src/AGENTS.md](src/AGENTS.md) - Core library modules and conventions
- [test/AGENTS.md](test/AGENTS.md) - Test patterns and runner structure
- [src/ProgBuilder/AGENTS.md](src/ProgBuilder/AGENTS.md) - Code generation patterns and deep module conventions
- [src/Template/AGENTS.md](src/Template/AGENTS.md) - Template system documentation
- [src/TypedASTGenerator/AGENTS.md](src/TypedASTGenerator/AGENTS.md) - Node description patterns
- [app/AGENTS.md](app/AGENTS.md) - Entry point and CLI documentation
- [sample/AGENTS.md](sample/AGENTS.md) - Example files and usage

## Project Discovery Insights

Based on comprehensive analysis of the codebase:

### Key Architecture Patterns
1. **Two-layer description system**: `ProgBuilderDescription.hs` defines high-level `Property` models with `GenerationHint`, which `FieldConversion.hs` converts to concrete `Field` representations
2. **CHOICE node handling**: Sophisticated branch unification via `uniqueBranch` and `convergeNamedProp` algorithms in `ProgBuilderDescription.hs`
3. **Annotated field model**: `AnnoatedField` (note spelling) with suffix-based naming (`_i`, `_0`, `_1`) for TypeScript field generation
4. **ECMA specialization**: Dedicated `ProgBuilderForECMA.hs` for TypeScript-specific code generation with `undefined` injection for CHOICE nodes
5. **Template-based generation**: Uses `Template.Template` (`TT`) and `Template.TypeScriptTemplate` (`TTS`) for structured code emission

### Complexity Hotspots
- **`src/ProgBuilder/ECMA/ProgBuilderForECMA.hs`** - Highest branching complexity (40+ case/if/where occurrences)
- **`src/TreeSitterGrammarNodes.hs`** - Complex grammar parsing and transformations
- **`src/Fundamentals/Inference.hs`** - Inference logic with multiple branching patterns
- **`src/ProgBuilder/ProgBuilderDescription.hs`** - Property models and branch unification algorithms
- **`src/Template/TypeScriptTemplate.hs`** - Template generation logic

### Anti-Patterns Found
1. **"Should never happen"** - `src/ProgBuilder/ProgBuilderDescription.hs:221` - Comment indicates exceptional case
2. **"No identifier: always use index"** - `src/Fundamentals/Inference.hs:226` - Coding convention comment
3. **Committed build artifacts** - `dist-newstyle/` directory contains Cabal build outputs (should be gitignored)
4. **Missing CI configuration** - No `.github/workflows/` directory present

### Build/CI Configuration
- **Cabal configuration**: `TreeSitterAST.cabal` with GHC2024, `-Wall` warnings, explicit module exports
- **Project settings**: `cabal.project` enables tests and allows newer dependencies
- **No lint/style configs**: No `.hlint.yaml` or `.stylish-haskell.yaml` present
- **IDE support**: `hie.yaml` for Haskell IDE Engine cradle configuration
- **Missing CI**: No GitHub Actions workflows configured

### Entry Points
- **Executable**: `app/Main.hs` - CLI entry point with argument parsing (dispatches to `astGen` or `progBuilderGen`)
- **Test runner**: `test/Main.hs` - Centralized test harness using Tasty framework
- **Argument parsing**: `app/Args.hs` - Uses `optparse-applicative` for CLI argument handling

### Test Patterns
- **Centralized harness**: `test/Main.hs` wires all Spec modules via Test.Tasty
- **Module exports**: Each test module exports single `TestTree` symbol (e.g., `inference_spec`, `template_testcase`)
- **Test data**: Mix of `sample/` fixtures and inline JSON strings
- **Naming conventions**: `Spec.hs` or `Test.hs` suffixes for test modules

---

## Build & Run Commands

### Building

```bash
# Build the project
cabal build

# Clean and rebuild
cabal clean && cabal build

# Build with verbose output
cabal build -v
```

### Running the Executable

```bash
# AST processor mode (generates node_declare.ts, node_processor.ts)
cabal run TreeSitterAST -- --ast-proc sample/node-types.json

# Code generator mode (generates <grammar_name>_rep.ts, <grammar_name>_grammar.txt)
cabal run TreeSitterAST -- --code-gen sample/grammar.json

# Specify output directory
cabal run TreeSitterAST -- --ast-proc sample/node-types.json --output-dir generated
```

### REPL Development

```bash
# Start REPL with library loaded
cabal repl lib:TreeSitterAST

# Start REPL with test module loaded
cabal repl test:TreeSitterAST-test
```

---

## Test Commands

```bash
# Run all tests with streaming output
cabal test --test-show-details=streaming

# Run tests with verbosity
cabal test -v
```

### Running Single Tests

After building, find the test binary and run with pattern matching:

```bash
# Find the test binary
TEST_BIN=$(find dist-newstyle -name "TreeSitterAST-test" -type f -executable | head -1)

# Run all tests via binary
$TEST_BIN

# Run tests matching a pattern (case-insensitive)
$TEST_BIN -p "Inference"

# List available test patterns
$TEST_BIN --list-tests

# Run specific test file
$TEST_BIN -p "Template"

# Run HUnit tests only
$TEST_BIN -p "Test.*HUnit"
```

### QuickCheck Tests

QuickCheck properties are automatically discovered and run by Tasty:

```bash
# Run with specific number of tests (default is 100)
$TEST_BIN --quickcheck-tests=500

# Run only QuickCheck properties
$TEST_BIN -p "QuickCheck"
```

---

## Code Style Guidelines

### Language & Extensions
- **Language Standard**: GHC2024
- **Common Extensions**: `OverloadedStrings`, `LambdaCase`, `RecordWildCards`, `PatternGuards`, `OverloadedRecordDot`
- **GHC Flags**: `-Wall` (warnings as errors, enforced in common stanza)

### Module Declaration

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ModuleName
  ( Export1 (..),
    Export2,
    export3,
  )
where
```

### Imports

- **Standard library**: Unqualified
- **Third-party**: Qualified with prefix (e.g., `qualified Data.Aeson as Aeson`)
- **Internal modules**: Qualified with prefix (e.g., `import TreeSitterGrammarNodes qualified as TSGN`)
- **Ordering**: Group by (1) standard library, (2) third-party, (3) internal; alphabetically within groups
- **Common patterns**: `Data.Text.Lazy qualified as T`, `Data.List as L`, `Data.Map qualified as Map`

```haskell
-- Standard library (unqualified)
import Control.Monad.Trans.Class
import Data.Map

-- Third-party (qualified with prefix)
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Text.Lazy qualified as T

-- Internal modules (qualified with prefix)
import TreeSitterGrammarNodes qualified as TSGN
import ProgBuilder.Types
```

### Data Types

- Use record syntax for structs with 2+ fields
- Derive `Show`, `Generic` by default; add `Eq`, `Ord` as needed

```haskell
data NodeInfo = NodeInfo
  { node_type :: String,
    named :: Bool
  }
  deriving (Show, Generic)

data TransitionPoint a
  = SEQ {seqNode :: a}
  | CHOICE {ruleName :: Text, choiceIdentifier :: a, alternativeIndex :: Int}
  deriving (Eq, Ord, Show)
```

### Naming Conventions

- **Modules**: `CamelCase` (e.g., `TreeSitterNodes`)
- **Functions**: `camelCase` (e.g., `parse_node_types`)
- **Types/Classes**: `CamelCase` (e.g., `NodeInfo`)
- **Records/Fields**: `camelCase` (e.g., `node_type`)

### Type Signatures

Always provide explicit type signatures for top-level functions:

```haskell
parse_node_types :: String -> MaybeT IO [Node]
parse_node_types path = do ...
```

### Error Handling

- **Prefer**: `MaybeT IO` for parsing operations (graceful failure)
- **Avoid**: `error` in library code; use in CLI only
- **JSON Parsing**: Use `FromJSON` with `prependFailure`/`typeMismatch`

```haskell
instance FromJSON NodeInfo where
  parseJSON (Object v) = NodeInfo <$> v .: "type" <*> v .: "named"
  parseJSON invalid = prependFailure "Parsing NodeInfo failed" (typeMismatch "Object" invalid)
```

### Pattern Matching

Use `LambdaCase` for simple case expressions:

```haskell
transform mode input outDir = case mode of
  AstProc -> astGen input outDir
  CodeGen -> progBuilderGen input outDir
```

### Monadic Style

- Use `do`-notation for clarity
- Use `(<$>)` and `(<*>)` for Applicative lifting

---

## Testing Patterns

### HUnit Tests

```haskell
{-# LANGUAGE OverloadedStrings #-}
module MyModuleSpec (my_spec) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import MyModule

my_spec :: TestTree
my_spec = testGroup "MyModule Tests"
  [ testCase "description" $ do
      actual @?= expected
  ]
```

### QuickCheck Properties

```haskell
import Test.Tasty.QuickCheck

my_prop_spec :: TestTree
my_prop_spec = testGroup "QuickCheck Properties"
  [ testProperty "propName" $ \input ->
      myFunction input `shouldSatisfy` someProperty
  ]
```

### Test Organization

Tests are in `test/` directory and registered in `test/Main.hs`:

```haskell
-- test/Main.hs
module Main (main) where

import Test.Tasty
import MyModuleSpec (my_spec)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [my_spec]
```

---

## Project Structure

```
TreeSitterAST/
├── app/                    # Executable entry points
│   ├── Main.hs            # CLI entry point
│   └── Args.hs            # Command-line argument parsing
├── src/                    # Core library
│   ├── Fundamentals/      # Inference logic, utilities
│   │   └── Inference.hs
│   ├── ProgBuilder/       # Code generation
│   │   ├── Types.hs
│   │   ├── FieldConversion.hs
│   │   ├── ProgBuilderDescription.hs
│   │   └── ECMA/
│   │       └── ProgBuilderForECMA.hs
│   ├── Template/          # Template system
│   │   ├── Template.hs
│   │   └── TypeScriptTemplate.hs
│   ├── TypedASTGenerator/ # AST node generators
│   │   ├── NodeDescription.hs
│   │   ├── NodeDescriptionHelper.hs
│   │   └── NodeProcessorDescription.hs
│   ├── TreeSitterNodes.hs         # Parser for node-types.json
│   ├── TreeSitterGrammarNodes.hs  # Parser for grammar.json
│   └── Utility.hs
├── test/                   # Test suite
│   ├── Main.hs
│   ├── Fundamentals/
│   │   └── InferenceSpec.hs
│   ├── Template/
│   │   └── TypeScriptTemplateSpec.hs
│   └── ...
├── sample/                 # Example input files
│   ├── node-types.json
│   └── grammar.json
└── TreeSitterAST.cabal
```

---

## Key Patterns

### Adding New GrammarNode Types

1. Add variant to `TreeSitterGrammarNodes.GrammarNode`
2. Update parser (`parseNodeFromJSON`)
3. Add inference rule in `Fundamentals.Inference`
4. Add generation logic in `ProgBuilder.ECMA.ProgBuilderForECMA`

### Property to Field Conversion

All generation must use `fieldsFromProperties` as single source of truth.

### JSON Parsing Pattern

Use this pattern for parsing JSON with good error messages:

```haskell
instance FromJSON MyType where
  parseJSON (Object v) = MyType
    <$> v .: "field1"
    <*> v .: "field2"
  parseJSON invalid = prependFailure "Parsing MyType failed" (typeMismatch "Object" invalid)
```

---

## Common Issues

- **Type mismatches**: Ensure `evalFieldType` extracts content type, not field name
- **Inconsistent field naming**: Use `fieldsFromProperties` consistently for field naming
- **String escaping**: Use `escapeTypeScriptString` for `"` and `\` characters
- **MaybeT failure**: Remember to use `MaybeT $ return Nothing` or `mzero`
- **"Should never happen" cases**: Handle exceptional cases explicitly rather than using comments
- **Output location confusion**: README vs CLI behavior mismatch - use `--output-dir` consistently

---

## Development Workflow

1. **REPL-first development**: Use `cabal repl lib:TreeSitterAST` to test functions
2. **Incremental building**: Build after each significant change
3. **Test-driven**: Write tests before implementing new features
4. **Type safety**: Never suppress type errors with `as any` or `@ts-ignore`
5. **Property to field conversion**: Always use `fieldsFromProperties` as single source of truth
6. **CHOICE node handling**: Follow suffix-based naming (`_i`, `_0`, `_1`) and `undefined` injection patterns
7. **JSON parsing**: Use `FromJSON` with `prependFailure`/`typeMismatch` for good error messages

### Adding New GrammarNode Types
1. Add variant to `TreeSitterGrammarNodes.GrammarNode`
2. Update parser (`parseNodeFromJSON`)
3. Add inference rule in `Fundamentals.Inference`
4. Add generation logic in `ProgBuilder.ECMA.ProgBuilderForECMA`

### Adding New Tests
1. Create test module with `Spec.hs` or `Test.hs` suffix
2. Export single `TestTree` symbol (e.g., `feature_spec`)
3. Add module to `other-modules` in `TreeSitterAST.cabal`
4. Import and wire in `test/Main.hs`

---

## Dependencies (Key)

- `aeson` — JSON parsing
- `text` / `bytestring` — String handling
- `transformers` / `mtl` — Monad transformers
- `containers` — Data structures
- `tasty` / `tasty-hunit` / `tasty-quickcheck` — Testing
- `optparse-applicative` — CLI parsing
