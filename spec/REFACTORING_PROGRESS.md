# Refactoring Progress Summary

## Overview

This document tracks progress on refactoring the TreeSitterAST codebase to extract repetitive patterns into separate entities, improving code reuse, maintainability, and consistency.

## Current Status

**Phase 1: Analysis & Planning** - **COMPLETED**
**Phase 2: Design & Implementation** - **IN PROGRESS**

## Timeline

- **2026-03-04**: Started refactoring analysis
- **2026-03-04**: Completed comprehensive codebase exploration
- **2026-03-04**: Identified 10 major repetitive patterns for extraction
- **2026-03-04**: Begun design of extracted entities

## Exploration Results

Thorough examination of the codebase revealed the following repetitive patterns:

### 1. **Recursive Tree Traversal Patterns** (HIGH PRIORITY)
- **Location**: Multiple modules implement nearly identical recursive traversal for `GrammarNode`
- **Files**: `TreeSitterGrammarNodes.hs`, `Fundamentals/Inference.hs`, `ProgBuilder/ECMA/ProgBuilderForECMA.hs`, `ProgBuilder/ProgBuilderDescription.hs`
- **Pattern**: 15+ branch `case` statements handling the same node types (Seq, Choice, Repeat, Repeat1, Symbol, StringLiteral, etc.)
- **Duplication**: Each function has its own implementation with minor variations

### 2. **String Construction/Concatenation Patterns** (HIGH PRIORITY)
- **Location**: `ProgBuilder/ECMA/ProgBuilderForECMA.hs` (25+ occurrences)
- **Pattern**: Repeated `T.concat`, `T.pack`, `T.unpack`, `T.intercalate` for TypeScript code generation
- **Examples**: `T.concat ["this.", name, "_i = ", name, ";"]` for field assignments, error messages, expression building

### 3. **Error Handling Patterns** (MEDIUM PRIORITY)
- **Location**: Multiple files, especially `ProgBuilder/ECMA/ProgBuilderForECMA.hs`
- **Pattern**: Similar `throw new Error(...)` constructions with descriptive messages
- **Examples**: `"Cannot evaluate X node: missing Y"` patterns

### 4. **Field/Property Processing Patterns** (MEDIUM PRIORITY)
- **Location**: `ProgBuilder/ECMA/ProgBuilderForECMA.hs`
- **Pattern**: Repeated case analysis on `Field` type (Field, SumField, EmptyField)
- **Duplication**: Similar handling for `Field` and `SumField` cases, special handling for `EmptyField`

### 5. **Template Instantiation Patterns** (MEDIUM PRIORITY)
- **Location**: `ProgBuilder/ECMA/ProgBuilderForECMA.hs`, `TypedASTGenerator/NodeDescription.hs`
- **Pattern**: `TT.inst TTS.some_template $ TT.inst TTS.another_template ...` chains
- **Opportunity**: Template composition operators could reduce boilerplate

### 6. **Type Name Transformation Patterns** (LOW PRIORITY)
- **Location**: Already abstracted in `NodeDescriptionHelper.hs` but used inconsistently
- **Pattern**: `upper_the_first_char name ++ "_T"` and `name ++ "_i"` patterns

### 7. **Choice/Alternative Processing Patterns** (MEDIUM PRIORITY)
- **Location**: `Fundamentals/Inference.hs`, `ProgBuilder/ProgBuilderDescription.hs`
- **Pattern**: Processing lists of alternatives with indexing and name generation
- **Duplication**: Similar logic across inference and property extraction modules

### 8. **Map/Set Operations Patterns** (LOW PRIORITY)
- **Location**: Multiple files
- **Pattern**: Similar `Map.foldrWithKey`, `Map.mapWithKey`, `Set.union`, `Set.fromList` patterns

### 9. **Code Generation Strategy Patterns** (HIGH PRIORITY)
- **Location**: `ProgBuilder/ECMA/ProgBuilderForECMA.hs` (lines 305-405)
- **Pattern**: Similar structure in `generateEvaluateMethod` for different node types
- **Examples**: `generateSeqEvaluate`, `generateChoiceEvaluate`, `generateRepeatEvaluate`, etc.

### 10. **Constructor/Factory Method Generation Patterns** (MEDIUM PRIORITY)
- **Location**: `ProgBuilder/ECMA/ProgBuilderForECMA.hs`
- **Pattern**: Similar patterns for generating constructors and factory methods
- **Duplication**: Parameter generation, assignment generation, super() calls

## Design Proposals

### 1. **Traversal Abstraction Module**
**Status**: PLANNED
**Purpose**: Extract recursive traversal patterns
**Proposed Interface**:
```haskell
class NodeTraversal t where
  traverseNode :: (Node -> a -> a) -> a -> Node -> a
  mapNodeM :: (Node -> m Node) -> Node -> m Node
  foldNode :: (a -> Node -> a) -> a -> Node -> a
```

### 2. **String Builder Abstraction for TypeScript**
**Status**: IN DESIGN
**Purpose**: Extract TypeScript code generation patterns
**Proposed Interface**:
```haskell
-- String building DSL for TypeScript
tsFieldAssignment :: Text -> Text -> Text
tsError :: Text -> Text
tsMethodCall :: Text -> [Text] -> Text
tsStringLiteral :: Text -> Text  -- with proper escaping
```

### 3. **Error Handling Abstraction**
**Status**: PLANNED
**Purpose**: Standardize error handling patterns
**Proposed Interface**:
```haskell
data GenerationError =
    MissingField Text
  | MissingProperty Text
  | InvalidNodeType Text
  | EvaluationError Text

throwGenerationError :: GenerationError -> Text
```

### 4. **Field/Property Processing Abstraction**
**Status**: PLANNED
**Purpose**: Extract field processing patterns
**Proposed Interface**:
```haskell
processField :: (Text -> Text -> a) -> (Text -> [Text] -> a) -> a -> Field -> a
mapField :: (Text -> Text) -> Field -> Field
filterFields :: (Field -> Bool) -> [Field] -> [Field]
```

### 5. **Template Composition Enhancement**
**Status**: PLANNED
**Purpose**: Reduce template instantiation boilerplate
**Proposed Interface**:
```haskell
(.>>) :: Template a -> Template b -> Template (a -> b)
infixl 4 .>>

composeTemplates :: [Template a] -> Template [a]
```

## Immediate Implementation Priorities

### 1. **String Builder Module** (CURRENT FOCUS)
- Extract `T.concat` patterns from `ProgBuilder/ECMA/ProgBuilderForECMA.hs`
- Create helper functions for common TypeScript code generation tasks
- Maintain compatibility with existing template system

### 2. **Traversal Abstraction Module** (NEXT)
- Extract recursive patterns from `TreeSitterGrammarNodes.hs`
- Create generic traversal utilities
- Update dependent modules to use new abstraction

### 3. **Error Handling Module**
- Standardize error message construction
- Replace inline `T.concat` error constructions
- Improve error type safety

## Benefits Expected

1. **Reduced Code Duplication**: Eliminate 15+ branch case statements and repetitive string constructions
2. **Improved Maintainability**: Changes to traversal logic affect only one module
3. **Better Testability**: Isolated components are easier to test
4. **Enhanced Readability**: Higher-level abstractions make intent clearer
5. **Increased Consistency**: Common operations use the same implementation
6. **Easier Extensibility**: New node types or generation strategies can be added in one place

## Next Steps

### Short-term (Next 1-2 sessions)
1. Complete design of String Builder abstraction
2. Implement initial version and test with existing code
3. Refactor `ProgBuilder/ECMA/ProgBuilderForECMA.hs` to use new abstraction

### Medium-term (Next 3-5 sessions)
1. Design and implement Traversal abstraction
2. Update `Fundamentals/Inference.hs` and `ProgBuilder/ProgBuilderDescription.hs`
3. Implement Error Handling abstraction

### Long-term (Future work)
1. Enhance Template system with composition operators
2. Create comprehensive test suite for extracted entities
3. Document new APIs and usage patterns

## Risk Assessment

### Technical Risks
1. **Breaking Changes**: Refactoring may break existing functionality
   - **Mitigation**: Maintain backward compatibility during transition
   - **Mitigation**: Comprehensive testing before and after changes

2. **Performance Impact**: Additional abstraction layers may affect performance
   - **Mitigation**: Profile critical paths before and after refactoring
   - **Mitigation**: Use efficient data structures and algorithms

3. **Increased Complexity**: New abstractions may increase learning curve
   - **Mitigation**: Clear documentation and examples
   - **Mitigation**: Gradual adoption with deprecation warnings

### Project Risks
1. **Scope Creep**: Refactoring may uncover more patterns than anticipated
   - **Mitigation**: Prioritize patterns with highest duplication
   - **Mitigation**: Set clear completion criteria for each phase

2. **Integration Complexity**: Coordinating changes across multiple modules
   - **Mitigation**: Use feature flags or gradual rollout
   - **Mitigation**: Maintain working version at all times

## Success Criteria

1. **Code Metrics**:
   - Reduce lines of code by 20-30% through elimination of duplication
   - Increase function reuse rate (same function used in multiple places)
   - Reduce cyclomatic complexity of key functions

2. **Quality Metrics**:
   - Maintain or improve test coverage
   - No regression in generated TypeScript output
   - Improved error messages and diagnostics

3. **Maintainability Metrics**:
   - Reduced number of `case` statements with 15+ branches
   - Consistent error handling patterns
   - Clear separation of concerns

## Progress Tracking

- [x] Phase 1: Codebase exploration and pattern identification
- [ ] Phase 2: Design of extracted entities
  - [x] String Builder abstraction (in progress)
  - [ ] Traversal abstraction
  - [ ] Error handling abstraction
  - [ ] Field processing abstraction
- [ ] Phase 3: Implementation and testing
- [ ] Phase 4: Integration and validation

## Dependencies

- **Haskell Language Extensions**: Current code uses `OverloadedStrings`, `MultilineStrings` - must maintain compatibility
- **Template System**: Must work with existing `Template.Template` infrastructure
- **External Dependencies**: `aeson`, `text`, `containers`, `formatting` - no changes planned
- **Build System**: `cabal` build must continue to work

## Notes

- The codebase already shows good separation of concerns (parsing, generation, templates)
- Refactoring focuses on reducing duplication within each layer
- Most pressing: traversal patterns (appear in almost every module)
- String construction patterns are highly repetitive and error-prone

---

*Last Updated: 2026-03-04*
*Tracked by Claude Code*