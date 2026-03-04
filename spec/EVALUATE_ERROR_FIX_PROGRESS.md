# Evaluate Error Fix Progress

## Bug Description

In the `generateEvaluateMethod` function, `evaluate` was being applied to a non-existent property `'unknown'` for certain grammar nodes. The expected behavior is to throw an error within such `evaluate()` method to indicate that the node is unevaluatable.

## Investigation

1. **Locate `generateEvaluateMethod`**: Found in `src/ProgBuilder/ECMA/ProgBuilderForECMA.hs`.
2. **Identify `'unknown'` references**: Searched the codebase for literal `"unknown"` – only one occurrence in `TreeSitterGrammarNodes.hs` (unrelated error message). The placeholder `'unknown'` was not present in source code.
3. **Generated code analysis**: Examined generated TypeScript/JavaScript files (`sample/javascript_template.js` and newly generated `javascript_template.ts`). Found that `this.unknown.evaluate()` appeared in many generated classes, especially those with empty fields or missing properties.
4. **Root cause analysis**: The bug occurred when:
   - Grammar nodes had no extractable properties (e.g., `Blank`, `Empty`, `Symbol` with no fields).
   - The property extraction (`propsOfNode`) returned an empty list, leading to empty `fields` list.
   - The `generateEvaluateMethod` for `SEQ`, `CHOICE`, `REPEAT`, `REPEAT1`, `SYMBOL` nodes incorrectly assumed at least one field exists and tried to evaluate `head fields'` (which would throw a runtime Haskell error, but guarded by `not (null fields')`).
   - However, for `SEQ` nodes, the `generateMemberCall` function would compute a `fieldIdx` that could be out of bounds, leading to the fallback error message `"Internal error: field index out of bounds for SEQ node"`. In older generated code, this error message was not being used; instead a placeholder `this.unknown.evaluate()` appeared.
   - The placeholder `'unknown'` likely originated from a previous version of the generator where a default property name `'unknown'` was used when a field name was empty.

## Root Cause

The immediate cause was that the generated error messages were being concatenated as expressions (`+ throw new Error(...)`) inside the `evaluate()` method’s return statement, resulting in invalid TypeScript syntax. Moreover, the error messages themselves were not being thrown correctly (they were being concatenated as strings). The `'unknown'` property appeared due to a bug in an earlier version; the current generator already produced error throws but with syntactic issues.

## Fixes Applied

1. **Updated error messages** in `generateEvaluateMethod` to be more descriptive:
   - `"Cannot evaluate CHOICE node: missing property"`
   - `"Cannot evaluate REPEAT node: missing property"`
   - `"Cannot evaluate REPEAT1 node: missing property"`
   - `"Cannot evaluate SYMBOL node: missing property"`
   - `"Cannot evaluate SEQ node: missing field"`
   - `"Cannot evaluate empty field"` (for `EmptyField` case)

2. **Fixed syntactic issue**: Modified `generateChoiceEvaluate`, `generateRepeatEvaluate`, `generateRepeat1Evaluate`, `generateSymbolEvaluate` to detect when `childCall` contains a `throw` statement and omit the surrounding `return … ;` wrapper. This ensures the generated code is `throw new Error(...);` instead of `return throw new Error(...);`.

3. **Fixed SEQ error handling**: Updated `generateSeqEvaluate` to detect if any child call contains a `throw` and, if so, use that throw as the entire body of the `evaluate()` method (instead of concatenating with `+`). This prevents invalid concatenation of `throw` statements.

4. **Added handling for `EmptyField`**: Added a case in `evalFieldExpr` to throw an error when encountering an `EmptyField` (though `EmptyField` is filtered out in `fieldsFromProperties`, this is a safety net).

## Code Changes

### `src/ProgBuilder/ECMA/ProgBuilderForECMA.hs`

- **Line 285‑294**: Rewrote `evalFieldExpr` to handle `EmptyField` with a throw.
- **Line 337‑347**: Updated `generateChoiceEvaluate` to conditionally wrap `childCall` with `return` only when it is not a throw.
- **Line 350‑358**: Updated `generateRepeatEvaluate` similarly.
- **Line 361‑369**: Updated `generateRepeat1Evaluate` similarly.
- **Line 372‑380**: Updated `generateSymbolEvaluate` similarly.
- **Line 301‑311**: Updated `generateSeqEvaluate` to detect throws and avoid concatenation.
- **Line 325**: Changed SEQ error message from `"Internal error: field index out of bounds for SEQ node"` to `"Cannot evaluate SEQ node: missing field"`.

## Testing

1. **Rebuilt the executable** with `cabal build exe:TreeSitterAST` – successful.
2. **Ran the code generator** on `sample/grammar.json` and inspected the output.
3. **Verified that `'unknown'` no longer appears** in the generated TypeScript file.
4. **Verified that error messages are correctly placed** as `throw new Error(...);` statements, not concatenated.
5. **Checked sample classes** (`Ternary_expression_T`, `String_T`, `Statement_block_T`, etc.) – all now have proper error throws.

## Result

- The bug where `evaluate()` referenced a non‑existent property `'unknown'` is resolved.
- Unevaluatable nodes now throw descriptive errors at runtime.
- Generated TypeScript code is syntactically valid.

## Bug Fix Enhancement: False Positive Throw Detection

After the initial fix, a secondary issue was identified: the detection of `throw` statements used a simple substring check (`"throw" `T.isInfixOf` call`), which would incorrectly match string literals containing the word `"throw"` (e.g., in `Throw_statement_T` or `Statement_throw_statement_T` classes). This could cause valid string concatenation to be replaced by a throw statement.

### Root Cause
- The detection logic `"throw" `T.isInfixOf` call` matched any occurrence of the substring `"throw"`, including inside quoted string literals like `"throw"`.
- String literals are generated as `"\"throw\""` (in Haskell) which becomes `"throw"` in TypeScript, containing the substring `"throw"`.

### Fix Applied
1. **Added `isThrowStatement` helper**: Defined as `T.isPrefixOf "throw "` to check if a generated call starts with the literal `"throw "` (followed by a space).
2. **Updated all detection points**:
   - `generateChoiceEvaluate`
   - `generateRepeatEvaluate`
   - `generateRepeat1Evaluate`
   - `generateSymbolEvaluate`
   - `generateSeqEvaluate` (via `containsThrow` predicate)
3. **Why this works**:
   - Throw statements always begin with `"throw "` (e.g., `"throw new Error(...)"`).
   - String literals begin with a double quote (`"`) not `"throw "`.
   - The space after `"throw"` distinguishes the keyword from substring matches.

### Verification
- Generated code for `Throw_statement_T` correctly produces `return "throw" + ...;` (not a throw statement).
- All error throws for unevaluatable nodes remain intact.
- No false positives for string literals containing `"throw"`.

## Future Considerations

- The `head` partial function warnings could be addressed by using pattern matching.
- The error messages could be refined to include the node name or member index for better debugging.
- The `generateMemberCall` could be refactored to return a `Maybe` type to explicitly signal errors.

## Commit

These changes are ready to be committed. The fix ensures that the code generator produces robust, self‑descriptive error handling for unevaluatable grammar nodes.

---

*Tracked by Claude Code on 2026‑03‑04*