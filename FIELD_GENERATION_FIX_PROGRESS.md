# Field Generation Fix - Progress Report

## Overview

This document tracks progress on fixing the mismatched data members issue in `ProgBuilderForECMA.hs`. The root cause was that data members were generated from a sorted/grouped `[Field]` list (`propFromTSGNs`), while constructor parameters, `evaluate()` references, and factory methods used the original `[Property]` list with different indexing logic.

## Changes Made (So Far)

### 1. Created `fieldsFromProperties` function
- **Location**: `src/ProgBuilder/ECMA/ProgBuilderForECMA.hs` (lines 229-241)
- **Purpose**: Convert `[Property]` to `[Field]` while preserving original order and applying correct suffixes.
- **Key logic**:
  - Iterates over properties in original order
  - Skips `StrProp` (string literals) - no field generated
  - For each non-string property, converts to `Field` using `propFromTSGN`
  - Appends suffix `_idx` where `idx` is the count of preceding non-string properties
  - Returns fields in same order as original non-string properties

### 2. Updated `build` function
- **Changes**:
  - Renamed `fields` variable to `props` (original properties)
  - Added `fields = fieldsFromProperties props`
  - Updated constructor call to `interiorConstructor fields`
  - Updated field declarations to use `collapse' fields` instead of `propFromTSGNs fields`
  - Changed variable name `props` to `fieldDecls` for clarity
- **TODO**: Still passing `props` to `generateEvaluateMethod` and `generateFactoryMethods` (needs update)

### 3. Refactored `interiorConstructor` to work with `[Field]`
- **Type change**: `[Property] -> T.Text` → `[Field] -> T.Text`
- **Simplified logic**:
  - Removed `countNonStrPropsBefore` calculations
  - Field names already include correct suffixes (`field_name`)
  - Uses `evalFieldType` for type strings
  - Handles `Field`, `SumField`, `EmptyField` cases directly

### 4. Added `isStrProp` helper
- Simple predicate to identify `StrProp` properties

## Completed Tasks

### High Priority
1. **Refactor `generateFactoryMethods` to work with `[Field]`** ✅
   - Updated to take `[Field]`, updated `generateFactoryParams` and `generateFactoryBody`
   - Removed `countNonStrPropsBefore` logic (fields already have suffixes)

2. **Refactor `generateEvaluateMethod` to work with `[Field]`** ✅
   - Updated to take `[Field]`, updated `generateSeqEvaluate`, `generateMemberCall` helpers
   - Simplified mapping logic (field list matches non-string child order)

3. **Update calls in `build` function** ✅
   - Changed `generateEvaluateMethod rule props` → `generateEvaluateMethod rule fields`
   - Changed `generateFactoryMethods ... props` → `generateFactoryMethods ... fields`

### Cleanup Tasks
4. **Remove unused functions** ✅
   - `propFromTSGNs` (line 197) – removed
   - `propFromTSGNs'` (line 210) – removed
   - `appendIDX` (line 199) – removed
   - `modifyFieldName` (line 207) – removed

5. **Consider helper consolidation** ✅
   - `propertyTypeStr` is kept for potential future use but not used currently
   - `getPropertyName` removed (no longer needed)

## Testing Completed ✅

1. **Test suite passed**: `cabal test` – all 39 tests pass
2. **Sample output regenerated**: `cabal run TreeSitterAST -- --code-gen sample/grammar.json` – generated files in `sample/` directory
3. **Verified specific class fixes**:
   - `Variable_declarator_T`: `_initializer_0_i` declarations/assignments match ✅
   - `Variable_declaration_T`: `_semicolon_2_i` declarations/assignments match ✅
   - `With_statement_T`: `body_1_i` assignment matches field declaration ✅
   - `_initializer_T`: `value_0_i` type `Expression_T` matches constructor parameter type `Expression_T` (no longer `Value_T`) ✅
   - `Switch_statement_T`: `value_0_i` type `Parenthesized_expression_T` matches constructor parameter type ✅

## Current Git Diff

*(See git diff for exact changes)*

## Summary

The field generation fix is complete. The root cause was that data members were generated from a sorted/grouped `[Field]` list (`propFromTSGNs`), while constructor parameters, `evaluate()` references, and factory methods used the original `[Property]` list with different indexing logic.

The fix introduces `fieldsFromProperties` which converts `[Property]` to `[Field]` while preserving original order and applying correct suffixes. All generation components now use the same `[Field]` list, ensuring consistent naming and typing.

**Key improvements**:
- Constructor parameter types now match field content types (e.g., `Expression_T` instead of `Value_T`)
- Consistent field naming across declarations, constructor assignments, evaluate methods, and factory methods
- Removal of unused functions and simplification of indexing logic
- All tests pass and generated code is correct