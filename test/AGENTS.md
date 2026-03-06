# test/AGENTS.md

Test suite organization and patterns for TreeSitterAST.

## Test Structure

### Centralized Test Harness
- **`Main.hs`** - Central test runner wiring all unit tests via Test.Tasty
- **Test composition**: All tests imported and composed into a single `TestTree`
- **Grouping**: Tests organized into logical groups (e.g., "Unit tests")

### Test Module Organization
```
test/
├── Main.hs                          # Test harness
├── Fundamentals/
│   └── InferenceSpec.hs            # Inference rule tests
├── Template/
│   └── TypeScriptTemplateSpec.hs   # Template generation tests
├── TreeSitterGrammarNodesSpec.hs   # Grammar node parsing tests
├── ProgBuilderUtilitiesSpec.hs     # ProgBuilder utility tests
└── StringEscapingTest.hs           # String escaping tests
```

### Test Module Naming Conventions
- Use `Spec.hs` or `Test.hs` suffixes
- Export a single test group symbol for harness import
- Follow naming: `module_name_spec` or `module_name_tests`

## Test Patterns

### Module Export Pattern
Each test module exports a single `TestTree` that the harness imports:

```haskell
-- In test module (e.g., InferenceSpec.hs)
module Fundamentals.InferenceSpec (inference_spec) where

inference_spec :: TestTree
inference_spec = testGroup "Inference Tests" [...]

-- In test/Main.hs
import Fundamentals.InferenceSpec (inference_spec)

tests :: TestTree
tests = testGroup "Tests" [unitTests]
  where unitTests = testGroup "Unit tests" 
          [template_testcase, grammar_nodes_spec, 
           prog_builder_utilities_spec, inference_spec, 
           stringEscapingTests]
```

### Test Group Structure
```haskell
-- Example from InferenceSpec.hs
inference_spec :: TestTree
inference_spec = testGroup "Inference Tests"
  [ testCase "description" $ do
      actual @?= expected
  , testProperty "property name" $ \input ->
      myFunction input `shouldSatisfy` someProperty
  ]
```

### HUnit Test Pattern
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

### QuickCheck Property Pattern
```haskell
import Test.Tasty.QuickCheck

my_prop_spec :: TestTree
my_prop_spec = testGroup "QuickCheck Properties"
  [ testProperty "propName" $ \input ->
      myFunction input `shouldSatisfy` someProperty
  ]
```

## Cabal Configuration

### Test Suite Wiring
```cabal
test-suite TreeSitterAST-test
  hs-source-dirs: test
  main-is: Main.hs
  other-modules:
    Template.TypeScriptTemplateSpec
    TreeSitterGrammarNodesSpec
    ProgBuilderUtilitiesSpec
    Fundamentals.InferenceSpec
    StringEscapingTest
  build-depends:
    base,
    tasty,
    tasty-hunit,
    tasty-quickcheck,
    ...
```

### Test Dependencies
- `tasty` - Test framework
- `tasty-hunit` - HUnit integration
- `tasty-quickcheck` - QuickCheck integration
- `QuickCheck` - Property-based testing

## Running Tests

### Via Cabal
```bash
# Run all tests with streaming output
cabal test --test-show-details=streaming

# Run tests with verbosity
cabal test -v
```

### Via Test Binary
```bash
# Find and run test binary
TEST_BIN=$(find dist-newstyle -name "TreeSitterAST-test" -type f -executable | head -1)
$TEST_BIN

# Run tests matching pattern
$TEST_BIN -p "Inference"

# List available test patterns
$TEST_BIN --list-tests

# Run specific test file
$TEST_BIN -p "Template"

# Run HUnit tests only
$TEST_BIN -p "Test.*HUnit"

# QuickCheck with specific test count
$TEST_BIN --quickcheck-tests=500
```

## Adding New Tests

### Step 1: Create Test Module
Create `test/Feature/FeatureSpec.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Feature.FeatureSpec (feature_spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import FeatureModule

feature_spec :: TestTree
feature_spec = testGroup "Feature Tests"
  [ testCase "basic functionality" $ do
      result <- someFunction "input"
      result @?= expectedResult
  , testProperty "property holds" $ \input ->
      propertyFunction input `shouldSatisfy` someCondition
  ]
```

### Step 2: Update Cabal File
Add to `other-modules` in `TreeSitterAST.cabal`:
```cabal
other-modules:
  ...
  Feature.FeatureSpec
```

### Step 3: Update Test Harness
Add import and wiring in `test/Main.hs`:
```haskell
import Feature.FeatureSpec (feature_spec)

tests :: TestTree
tests = testGroup "Tests" [unitTests]
  where unitTests = testGroup "Unit tests" 
          [..., feature_spec, ...]
```

## Test Coverage Areas

### Core Functionality Tests
1. **Grammar Parsing** (`TreeSitterGrammarNodesSpec.hs`)
   - JSON parsing correctness
   - Grammar node transformations
   - Error handling

2. **Inference Rules** (`Fundamentals/InferenceSpec.hs`)
   - Expression evaluation
   - Node type inference
   - Branch unification

3. **Template Generation** (`Template/TypeScriptTemplateSpec.hs`)
   - Template rendering
   - TypeScript code generation
   - String escaping

4. **ProgBuilder Utilities** (`ProgBuilderUtilitiesSpec.hs`)
   - Property conversion
   - Field generation
   - Type mapping

5. **String Escaping** (`StringEscapingTest.hs`)
   - TypeScript string escaping
   - JSON compatibility
   - Special character handling

## Test Data Management

### Sample Files
- Use `sample/` directory for test input files
- `node-types.json` - Node type definitions
- `grammar.json` - Grammar definitions

### Test Fixtures
- Create minimal test cases in test modules
- Use `Arbitrary` instances for QuickCheck properties
- Mock complex dependencies when necessary

## Best Practices

### Test Organization
- Group related tests in `testGroup`
- Use descriptive test case names
- Follow existing naming conventions

### Property-Based Testing
- Use QuickCheck for invariant testing
- Define `Arbitrary` instances for custom types
- Test edge cases and boundary conditions

### Test Maintenance
- Update tests when functionality changes
- Keep test data in sync with sample files
- Run tests before committing changes

### Performance Considerations
- Keep individual tests fast (<100ms)
- Use appropriate test size for QuickCheck
- Avoid I/O in unit tests when possible