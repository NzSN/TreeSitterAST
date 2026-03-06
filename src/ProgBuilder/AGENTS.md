# src/ProgBuilder/AGENTS.md

Code generation patterns and deep module conventions for TreeSitterAST.

## Module Overview

### Core Modules
- **`Types.hs`** - Core data types: `Field`, `SumField`, `EmptyField`, `AnnoatedField`
- **`ProgBuilderDescription.hs`** - High-level property models with `GenerationHint`
- **`FieldConversion.hs`** - Converts properties to concrete `Field` representations
- **`ECMA/ProgBuilderForECMA.hs`** - TypeScript-specific code generation

## Architecture Patterns

### Two-Layer Description System

#### 1. Property Layer (`ProgBuilderDescription.hs`)
- **`Property`** - High-level model with `GenerationHint`
- **`NamedProp`** - Named properties with type lists
- **`GenerationHint`** - Hints for code generation (e.g., `Leaf`, `Interior`)

Key algorithms:
- `uniqueBranch` - Unifies properties across alternative branches
- `convergeNamedProp` - Merges named properties with type unification
- `insertNamedProp` - Inserts properties with conflict resolution

#### 2. Field Layer (`FieldConversion.hs`)
- **`Field`** - Concrete field representation
- **`AnnoatedField`** (note spelling) - Field with original reference and optional index
- **`fieldsFromProperties`** - Single source of truth for field conversion

### CHOICE Node Handling

#### Branch Unification
```haskell
-- In ProgBuilderDescription.hs
uniqueBranch :: [Property] -> [Property]
uniqueBranch props = ...

convergeNamedProp :: NamedProp -> NamedProp -> NamedProp
convergeNamedProp (NamedProp name1 types1 hint1) (NamedProp name2 types2 hint2) =
  NamedProp name1 (nub $ types1 ++ types2) (hint1 `mappend` hint2)
```

#### TypeScript Sum Types
- CHOICE nodes generate TypeScript union types
- `undefined` injected for absent alternatives
- Field suffixing (`_i`, `_0`, `_1`) for alternative branches

### Annotated Field Model

#### `AnnoatedField` Structure
```haskell
data AnnoatedField = AnnoatedField
  { original :: Maybe Field
  , field :: Field
  , index :: Maybe Int
  }
```

#### Field Suffixing
- `_i` suffix added via `evalFieldName`
- Index-based suffixes (`_0`, `_1`) via `fieldsFromProperties`
- Consistent naming across CHOICE alternatives

## TypeScript Code Generation

### ECMA Specialization (`ECMA/ProgBuilderForECMA.hs`)
- Highest branching complexity (40+ case/if/where occurrences)
- TypeScript-specific transformations
- `undefined` injection for CHOICE nodes

#### Key Functions
- `transformChoiceFields` - Adds `undefined` to CHOICE field types
- `generateEvaluateMethod` - Generates TypeScript `evaluate()` methods
- `collapse'` - Transforms fields within CHOICE nodes to sum types

#### TypeScript Emission Pattern
```haskell
-- Template-based emission
prologue :: Text
prologue = "export class SyntaticNode {\n  evaluate(): string {\n    throw Error(...);\n  }\n}\n\n"

-- Class generation
generateClass :: GrammarNodeWithField -> Text
generateClass node = ...
```

### Built-in vs User-Defined Types

#### Type Mapping (`FieldConversion.hs`)
```haskell
isBuiltin :: Text -> Bool
isBuiltin "undefined" = True  -- Added for CHOICE node support
isBuiltin t = t `elem` ["string", "number", "boolean", ...]

typeShow :: Text -> Text
typeShow t
  | isBuiltin t = t
  | otherwise = t <> "_T"  -- Suffix for user-defined types
```

#### Property Type Strings
```haskell
propertyTypeStr :: NamedProp -> Text
propertyTypeStr (NamedProp _ types _) =
  T.intercalate " | " $ nub $ map typeShow types
```

## Module Conventions

### Import Patterns
```haskell
-- Standard imports
import Control.Monad.Trans.Class
import Data.List as L
import Data.Text.Lazy qualified as T

-- Internal imports
import TreeSitterGrammarNodes qualified as TSGN
import ProgBuilder.Types
```

### Data Type Definitions
```haskell
-- Record syntax for structs
data Field = Field
  { fieldName :: Text
  , fieldType :: Text
  , isOptional :: Bool
  }
  deriving (Show, Generic)

-- Sum types for alternatives
data GrammarNodeWithField
  = LeafNode Field
  | InteriorNode [Field]
  | ChoiceNode Text [GrammarNodeWithField]
  deriving (Show, Eq)
```

### Template System Integration
- Uses `Template.Template` (`TT`) and `Template.TypeScriptTemplate` (`TTS`)
- Structured emission of TypeScript code
- Consistent prologue and class skeletons

## Complexity Hotspots

### `ProgBuilderForECMA.hs` (Highest Complexity)
- **Lines**: ~500+ 
- **Branching**: 40+ case/if/where occurrences
- **Focus**: TypeScript code generation, CHOICE node handling

### `ProgBuilderDescription.hs`
- **Lines**: ~400+
- **Focus**: Property models, branch unification algorithms
- **Anti-pattern**: "Should never happen" comment at line 221

### `FieldConversion.hs`
- **Lines**: ~300+
- **Focus**: Property-to-field conversion, type mapping
- **Key function**: `fieldsFromProperties` (single source of truth)

## Development Guidelines

### Adding New Generation Hints
1. Add variant to `GenerationHint` in `ProgBuilderDescription.hs`
2. Update `mappend` implementation for hint combination
3. Add handling in `FieldConversion.hs` field generation
4. Update `ProgBuilderForECMA.hs` TypeScript emission

### Modifying CHOICE Node Handling
1. Update `uniqueBranch` and `convergeNamedProp` algorithms
2. Modify `transformChoiceFields` in `ProgBuilderForECMA.hs`
3. Update field suffixing logic in `fieldsFromProperties`
4. Test with sample grammar files

### Extending Type Support
1. Update `isBuiltin` in `FieldConversion.hs`
2. Add `typeShow` mapping for new types
3. Update `propertyTypeStr` for union type generation
4. Test TypeScript output compatibility

## Common Issues

### Type Mismatches
- Ensure `evalFieldType` extracts content type, not field name
- Use `fieldsFromProperties` consistently for field naming
- Check `typeShow` mapping for user-defined types

### CHOICE Node Field Suffixing
- Fields in CHOICE alternatives get `_i` suffix
- Index-based suffixes (`_0`, `_1`) for multiple properties
- `undefined` injection for TypeScript union types

### Template Emission
- Use `TT` and `TTS` for structured code generation
- Follow prologue skeleton pattern
- Ensure proper indentation in generated TypeScript

## Testing Strategy

### Unit Tests
- Test `uniqueBranch` and `convergeNamedProp` algorithms
- Verify `fieldsFromProperties` output
- Test `transformChoiceFields` behavior

### Integration Tests
- End-to-end TypeScript code generation
- CHOICE node handling with sample grammars
- Type mapping and suffixing correctness

### Property-Based Tests
- QuickCheck properties for branch unification
- Type mapping invariants
- Field suffixing consistency