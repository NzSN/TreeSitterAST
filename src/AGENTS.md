# src/AGENTS.md

Core library modules for TreeSitterAST - TypeScript AST generation from Tree-sitter grammars.

## Module Organization

### Core Parsing Modules
- **`TreeSitterNodes.hs`** - Parser for `node-types.json` files
- **`TreeSitterGrammarNodes.hs`** - Parser for `grammar.json` files
- **`Utility.hs`** - Shared utilities and helper functions

### Fundamentals Module
- **`Fundamentals/Inference.hs`** - Inference logic for evaluating grammar nodes into expressions

### Code Generation Pipeline
- **`ProgBuilder/`** - Code generation components
  - `Types.hs` - Core data types: `Field`, `SumField`, `EmptyField`, `AnnoatedField`
  - `ProgBuilderDescription.hs` - High-level property models with `GenerationHint`
  - `FieldConversion.hs` - Converts properties to concrete `Field` representations
  - `ECMA/ProgBuilderForECMA.hs` - TypeScript-specific code generation

### Template System
- **`Template/`** - Template-based code generation
  - `Template.hs` - Foundation template system
  - `TypeScriptTemplate.hs` - TypeScript-specific template generation

### AST Node Generators
- **`TypedASTGenerator/`** - AST node description and processing
  - `NodeDescription.hs` - Node description definitions
  - `NodeDescriptionHelper.hs` - Helper functions for node descriptions
  - `NodeProcessorDescription.hs` - Node processor descriptions

## Key Conventions

### Module Import Patterns
```haskell
-- Standard library imports (unqualified)
import Control.Monad.Trans.Class
import Data.Map

-- Third-party imports (qualified with prefix)
import Data.Aeson as Aeson
import Data.Aeson.Types

-- Internal imports (qualified with prefix)
import TreeSitterGrammarNodes qualified as TSGN
```

### Data Type Patterns
- Use record syntax for structs with 2+ fields
- Derive `Show`, `Generic` by default; add `Eq`, `Ord` as needed
- Consistent naming: `AnnoatedField` (note double 'a') used throughout codebase

### Error Handling
- **Prefer**: `MaybeT IO` for parsing operations (graceful failure)
- **Avoid**: `error` in library code; use in CLI only
- **JSON Parsing**: Use `FromJSON` with `prependFailure`/`typeMismatch`

### Type Signatures
Always provide explicit type signatures for top-level functions:
```haskell
parse_node_types :: String -> MaybeT IO [Node]
parse_node_types path = do ...
```

## Architecture Patterns

### Two-Layer Description System
1. **Property Layer** (`ProgBuilderDescription.hs`):
   - Defines high-level `Property` models with `GenerationHint`
   - Handles branch unification via `uniqueBranch` and `convergeNamedProp`
   - Manages CHOICE node alternatives

2. **Field Layer** (`FieldConversion.hs`):
   - Converts `Property` to concrete `Field` representations
   - Handles built-in vs user-defined type mapping
   - Implements suffix-based field naming (`_i`, `_0`, `_1`)

### CHOICE Node Handling
- Sophisticated branch unification algorithms
- `undefined` injection for TypeScript sum types
- Field suffixing for alternative branches

### Template-Based Generation
- Uses `Template.Template` (`TT`) and `Template.TypeScriptTemplate` (`TTS`)
- Structured emission of TypeScript classes, methods, and properties
- Consistent prologue skeleton for generated code

## Complexity Hotspots

### High Branching Complexity
1. **`ProgBuilder/ECMA/ProgBuilderForECMA.hs`** (40+ case/if/where occurrences)
   - TypeScript-specific code generation
   - CHOICE node transformation logic
   - Field evaluation and emission

2. **`TreeSitterGrammarNodes.hs`**
   - Complex grammar parsing and transformations
   - JSON parsing with extensive pattern matching

3. **`Fundamentals/Inference.hs`**
   - Inference logic with multiple branching patterns
   - Grammar node evaluation rules

### Large Modules (>500 lines)
- `ProgBuilder/ECMA/ProgBuilderForECMA.hs`
- `TreeSitterGrammarNodes.hs`
- `Fundamentals/Inference.hs`
- `ProgBuilder/ProgBuilderDescription.hs`
- `Template/TypeScriptTemplate.hs`
- `TypedASTGenerator/NodeDescription.hs`
- `TypedASTGenerator/NodeProcessorDescription.hs`
- `ProgBuilder/FieldConversion.hs`

## Development Guidelines

### Adding New GrammarNode Types
1. Add variant to `TreeSitterGrammarNodes.GrammarNode`
2. Update parser (`parseNodeFromJSON`)
3. Add inference rule in `Fundamentals.Inference`
4. Add generation logic in `ProgBuilder.ECMA.ProgBuilderForECMA`

### Property to Field Conversion
- All generation must use `fieldsFromProperties` as single source of truth
- Follow suffix-based naming conventions (`_i`, `_0`, `_1`)
- Handle built-in vs user-defined types via `isBuiltin`

### JSON Parsing Pattern
```haskell
instance FromJSON MyType where
  parseJSON (Object v) = MyType
    <$> v .: "field1"
    <*> v .: "field2"
  parseJSON invalid = prependFailure "Parsing MyType failed" (typeMismatch "Object" invalid)
```

## Common Issues

### Type Mismatches
- Ensure `evalFieldType` extracts content type, not field name
- Use `fieldsFromProperties` consistently for field naming

### String Escaping
- Use `escapeTypeScriptString` for `"` and `\` characters
- Handle JSON string literals properly

### MaybeT Failure Handling
- Remember to use `MaybeT $ return Nothing` or `mzero`
- Don't suppress type errors with `as any` or `@ts-ignore`

## Module Dependencies

```
TreeSitterNodes.hs → TreeSitterGrammarNodes.hs
                    ↓
           Fundamentals/Inference.hs
                    ↓
       ProgBuilderDescription.hs → FieldConversion.hs
                    ↓                    ↓
           ProgBuilderForECMA.hs ← Types.hs
                    ↓
           Template/TypeScriptTemplate.hs
```

## Testing Strategy

- Unit tests in corresponding `test/` directory
- Focus on inference rules and parsing logic
- Test property-to-field conversion thoroughly
- Verify TypeScript code generation output