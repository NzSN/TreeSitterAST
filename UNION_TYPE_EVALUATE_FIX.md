# Union Type Evaluate() Fix for String Types

## Problem Statement

The generated TypeScript classes with union types containing `string` were incorrectly trying to call `.evaluate()` on string values. For example, in `Variable_declarator_T`:

```typescript
export class Variable_declarator_T extends SyntaticInterior {
    name_0_i: Identifier_T | string | _destructuring_pattern_T;
    _initializer_1_i: _initializer_T;

    evaluate(): string {
        return this.name_0_i.evaluate() + this._initializer_1_i.evaluate(); // ERROR!
    }
}
```

The field `name_0_i` has type `Identifier_T | string | _destructuring_pattern_T`. When it contains a `string`, calling `.evaluate()` fails because strings don't have an `evaluate()` method.

## Root Cause

The `generateEvaluateMethod` function in `ProgBuilderForECMA.hs` was generating `this.fieldName_i.evaluate()` for all fields without checking if the field type includes `string`.

The issue affected:
- `generateSeqEvaluate` - concatenating children in SEQ nodes
- `generateChoiceEvaluate` - evaluating first alternative in CHOICE nodes
- `generateRepeatEvaluate` - evaluating repeated content
- `generateRepeat1Evaluate` - evaluating 1+ repetitions
- `generateSymbolEvaluate` - evaluating symbol references

## Solution

### 1. Added Type Detection Functions

Three new helper functions were added to `generateEvaluateMethod`:

```haskell
-- Get list of types in a field
fieldTypes :: Field -> [T.Text]
fieldTypes (Field _ t) = [t]
fieldTypes (SumField _ ts) = ts
fieldTypes EmptyField = []

-- Check if a field contains string type
fieldContainsString :: Field -> Bool
fieldContainsString = L.any (== "string") . fieldTypes

-- Check if a field contains only string type (no other types)
fieldIsOnlyString :: Field -> Bool
fieldIsOnlyString field = evalFieldType field == "string"
```

### 2. Added Smart Evaluation Expression

The `evalFieldExpr` function generates the appropriate TypeScript expression:

```haskell
-- Generate expression to evaluate a field (handles string union types)
evalFieldExpr :: Field -> T.Text
evalFieldExpr field =
  let fieldName = evalFieldName field
  in if fieldContainsString field
     then if fieldIsOnlyString field
          then T.concat ["this.", fieldName]  -- Direct string access
          else T.concat ["(typeof this.", fieldName,
                         " === 'string' ? this.", fieldName,
                         " : this.", fieldName, ".evaluate())"]
     else T.concat ["this.", fieldName, ".evaluate()"]  -- Normal evaluate()
```

### 3. Updated All Evaluation Functions

All evaluation generation functions now use `evalFieldExpr` instead of directly calling `.evaluate()`:

```haskell
-- Before (incorrect):
generateMemberCall fields'' (idx, member) = ...
  then T.concat ["this.", evalFieldName (fields'' !! fieldIdx), ".evaluate()"]

-- After (correct):
generateMemberCall fields'' (idx, member) = ...
  then evalFieldExpr (fields'' !! fieldIdx)

-- Similarly for other evaluation functions:
generateChoiceEvaluate -> uses evalFieldExpr (head fields')
generateRepeatEvaluate -> uses evalFieldExpr (head fields')
generateRepeat1Evaluate -> uses evalFieldExpr (head fields')
generateSymbolEvaluate -> uses evalFieldExpr (head fields')
```

## Generated Code Examples

### Before (Error)
```typescript
evaluate(): string {
    return this.name_0_i.evaluate() + this._initializer_1_i.evaluate();
}
```

### After (Correct)
```typescript
evaluate(): string {
    return (typeof this.name_0_i === 'string' ? this.name_0_i : this.name_0_i.evaluate())
           + this._initializer_1_i.evaluate();
}
```

### Pure String Type
For fields with only `string` type (no union):
```typescript
// Field declaration: kind_0_i: string;
// Generated evaluate():
evaluate(): string {
    return this.kind_0_i + this.variable_declarator_1_i.evaluate();
}
```

## Key Implementation Details

1. **Type Detection**: The fix checks both `fieldContainsString` (for union types) and `fieldIsOnlyString` (for pure string types).

2. **Optimization**: When a field has only `string` type, it uses direct access (`this.fieldName`) instead of type checking (`typeof this.fieldName === 'string'`).

3. **Consistent Application**: All evaluation generation functions use the same `evalFieldExpr` logic, ensuring consistency across SEQ, CHOICE, REPEAT, REPEAT1, and SYMBOL nodes.

4. **Backward Compatibility**: The fix doesn't break existing functionality for non-string types - they continue to use `.evaluate()` normally.

## Files Modified

- `src/ProgBuilder/ECMA/ProgBuilderForECMA.hs` - Core implementation of the fix

## Testing

The fix can be verified by:
1. Running `cabal build` - should compile without errors
2. Generating sample output: `cabal run TreeSitterAST -- --code-gen sample/grammar.json`
3. Checking generated classes like `Variable_declarator_T` and `Using_declaration_T` for proper type checking

## Related Issues

This fix complements the earlier field generation fix (documented in `FIELD_GENERATION_FIX_PROGRESS.md`) which addressed type mismatches in constructor parameters. Together, these fixes ensure:
- Correct type annotations in field declarations and constructor parameters
- Proper type checking in `evaluate()` method implementations