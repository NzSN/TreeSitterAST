# Sentence Generation Architecture for TreeSitterAST Code Generator

## Overview

This document outlines the architecture for completing the Code Generator (`--code-gen` mode) to generate TypeScript classes that can produce valid sentences from Tree-sitter grammar definitions. The system will generate classes with functional `evaluate()` methods, factory methods, and generation strategies.

## Current State Analysis

### Existing Components
1. **Grammar Parsing** (`TreeSitterGrammarNodes.hs`): Parses `grammar.json` into `GrammarNode` structures
2. **Inference System** (`Fundamentals/Inference.hs`): Transforms grammar nodes, tracks CHOICE transition points
3. **Code Generation** (`ProgBuilder/ECMA/ProgBuilderForECMA.hs`): Generates TypeScript class skeletons
4. **Property Extraction** (`ProgBuilder/ProgBuilderDescription.hs`): Extracts properties from grammar nodes
5. **Template System** (`Template/TypeScriptTemplate.hs`): TypeScript code templates

### Current Limitations
- Generated classes have empty `evaluate()` methods (throw errors)
- No sentence generation logic
- No factory methods for construction
- No generation strategies (random, guided)
- No sentence validation

## Architecture Design

### 1. Core Data Types

#### New Types in `Fundamentals/Generation.hs`:

```haskell
-- Generation strategies
data GenerationStrategy
  = RandomStrategy { seed :: Maybe Int }
  | GuidedStrategy { constraints :: [Constraint] }
  | ExhaustiveStrategy { maxDepth :: Int }
  deriving (Show, Eq)

-- Generation context
data GenerationContext = GenerationContext
  { strategy :: GenerationStrategy
  , depth :: Int
  , maxDepth :: Int
  , symbolTable :: Map Text (GrammarNode Text)
  , choiceHistory :: [ChoiceDecision]
  } deriving (Show)

-- Choice resolution
data ChoiceDecision = ChoiceDecision
  { ruleName :: Text
  , alternativeIndex :: Int
  , alternativeName :: Text
  } deriving (Show, Eq)

-- Factory method configuration
data FactoryConfig = FactoryConfig
  { generateFactories :: Bool
  , factoryStyle :: FactoryStyle
  } deriving (Show)

data FactoryStyle
  = StaticMethods      -- static createX() methods
  | BuilderPattern     -- fluent builder pattern
  | FunctionalStyle    -- pure functions
  deriving (Show, Eq)
```

### 2. Sentence Generation Algorithms

#### 2.1 Grammar Node Evaluation
Each grammar node type needs an evaluation strategy:

```haskell
-- Evaluation rules for each GrammarNode type
evaluateNode :: GenerationContext -> GrammarNode Text -> Maybe (Text, GenerationContext)

-- SEQ: Evaluate all members and concatenate
evaluateNode ctx (Seq members) = do
  (results, ctx') <- foldM evalAccumulator ctx members
  return (T.concat results, ctx')
  where
    evalAccumulator accCtx member = do
      (result, newCtx) <- evaluateNode accCtx member
      return (result:accResults, newCtx)

-- CHOICE: Select one alternative based on strategy
evaluateNode ctx (Choice alternatives) = do
  (selectedIdx, ctx') <- selectAlternative ctx alternatives
  evaluateNode ctx' (alternatives !! selectedIdx)

-- REPEAT/REPEAT1: Generate 0-n or 1-n repetitions
evaluateNode ctx (Repeat content) = do
  count <- determineCount ctx  -- Based on strategy
  generateRepeated ctx content count

-- SYMBOL: Look up and evaluate referenced rule
evaluateNode ctx (Symbol name) = do
  node <- Map.lookup name (symbolTable ctx)
  evaluateNode ctx node

-- StringLiteral/PATTERN: Return literal value
evaluateNode _ (StringLiteral value) = Just (value, ctx)
evaluateNode _ (Pattern value) = Just (value, ctx)

-- FIELD: Generate with field annotation (for validation)
evaluateNode ctx (Field fieldName content) = do
  (result, ctx') <- evaluateNode ctx content
  return (annotateField fieldName result, ctx')
```

#### 2.2 Choice Resolution Strategies

```haskell
selectAlternative :: GenerationContext -> [GrammarNode Text] -> Maybe (Int, GenerationContext)
selectAlternative ctx alternatives =
  case strategy ctx of
    RandomStrategy seed -> randomSelection seed alternatives
    GuidedStrategy constraints -> guidedSelection constraints alternatives
    ExhaustiveStrategy maxDepth -> exhaustiveSelection ctx alternatives
```

#### 2.3 Repetition Handling

```haskell
generateRepeated :: GenerationContext -> GrammarNode Text -> Int -> Maybe (Text, GenerationContext)
generateRepeated ctx content count
  | count <= 0 = Just ("", ctx)
  | otherwise = do
      (firstResult, ctx1) <- evaluateNode ctx content
      (restResult, ctxN) <- generateRepeated ctx1 content (count - 1)
      return (firstResult <> separator <> restResult, ctxN)
  where
    separator = determineSeparator content  -- e.g., ", " for lists
```

### 3. Code Generation Enhancements

#### 3.1 Enhanced `ProgBuilderForECMA.hs`

```haskell
-- Generate evaluate() method for each class
generateEvaluateMethod :: GrammarNode Text -> [Property] -> T.Text
generateEvaluateMethod node properties =
  case node of
    Seq members -> generateSeqEvaluate members properties
    Choice alternatives -> generateChoiceEvaluate alternatives properties
    Repeat content -> generateRepeatEvaluate content properties
    Symbol name -> generateSymbolEvaluate name properties
    StringLiteral _ -> generateLiteralEvaluate
    _ -> generateDefaultEvaluate properties

-- Generate factory methods
generateFactoryMethods :: String -> [Property] -> FactoryConfig -> T.Text
generateFactoryMethods className properties config =
  case factoryStyle config of
    StaticMethods -> generateStaticFactories className properties
    BuilderPattern -> generateBuilderPattern className properties
    FunctionalStyle -> generateFunctionalFactories className properties
```

#### 3.2 New Template Functions in `TypeScriptTemplate.hs`

```haskell
-- Template for evaluate() method
evaluate_method :: Template (TArray Text -> Text)
evaluate_method = T $ "evaluate(): string { " % build % " }"

-- Template for factory methods
static_factory_method :: Template (Text -> TArray Text -> Text -> Text)
static_factory_method = T $ "static create" % text % "(" % build % "): " % text % " { " % text % " }"

-- Template for builder pattern
builder_method :: Template (Text -> Text -> Text)
builder_method = T $ text % "(" % text % "): this { " % text % " return this; }"
```

### 4. Module Responsibilities

#### 4.1 New Module: `Fundamentals/Generation.hs`
- Generation strategies and algorithms
- Choice resolution logic
- Repetition handling
- Context management

#### 4.2 Enhanced Module: `ProgBuilder/ECMA/ProgBuilderForECMA.hs`
- Generate `evaluate()` method implementations
- Generate factory methods
- Integrate generation strategies
- Handle property initialization in constructors

#### 4.3 New Module: `ProgBuilder/GenerationDriver.hs`
- Orchestrate generation process
- Apply generation strategies
- Validate generated sentences
- Provide generation statistics

#### 4.4 Enhanced Module: `Template/TypeScriptTemplate.hs`
- New templates for evaluate methods
- Factory method templates
- Builder pattern templates
- Validation code templates

### 5. Integration Points

#### 5.1 Data Flow
```
grammar.json
    ↓
TreeSitterGrammarNodes.parseGrammarFromJSON
    ↓
Grammar (Map Text (GrammarNode Text))
    ↓
Fundamentals.Inference.trans (transform CHOICE nodes)
    ↓
Enhanced ProgBuilderForECMA.descript
    ↓
TypeScript classes with evaluate() + factories
```

#### 5.2 Generation Pipeline
```
Grammar Node
    ↓
Property Extraction (ProgBuilderDescription)
    ↓
Generation Strategy Selection
    ↓
Node Evaluation (Fundamentals.Generation)
    ↓
Code Generation (ProgBuilderForECMA)
    ↓
TypeScript Output
```

### 6. Specific File Changes

#### 6.1 `src/Fundamentals/Generation.hs` (New)
```haskell
module Fundamentals.Generation where

import TreeSitterGrammarNodes qualified as TSGN
import Data.Map qualified as Map
import Data.Text.Lazy (Text)
import System.Random (RandomGen, newStdGen, randomR)

-- Core generation types and functions as described above
```

#### 6.2 `src/ProgBuilder/ECMA/ProgBuilderForECMA.hs` (Modified)
```haskell
-- Add imports
import Fundamentals.Generation (GenerationStrategy(..), evaluateNode, defaultContext)
import Template.TypeScriptTemplate (evaluate_method, static_factory_method)

-- Enhance build function
build :: String -> TSGN.Node -> GenerationStrategy -> T.Text
build name rule strategy =
  let className = T.pack $ node_type_ident name
      fields = propsOfNode rule
      evaluateMethod = generateEvaluateMethod rule fields strategy
      factoryMethods = generateFactoryMethods name fields factoryConfig
      -- ... rest of existing build function
  in T.concat [classDecl, evaluateMethod, factoryMethods]
```

#### 6.3 `src/Template/TypeScriptTemplate.hs` (Modified)
```haskell
-- Add new templates
evaluate_method :: Template (TArray Text -> Text)
evaluate_method = T $ "evaluate(): string { " % build % " }"

static_factory_method :: Template (Text -> TArray Text -> Text -> Text)
static_factory_method = T $ "static create" % text % "(" % build % "): " % text % " { " % text % " }"

builder_pattern_class :: Template (Text -> TArray Text -> Text)
builder_pattern_class = T $ "export class " % text % "Builder { " % build % " }"
```

#### 6.4 `src/ProgBuilder/GenerationDriver.hs` (New)
```haskell
module ProgBuilder.GenerationDriver where

import Fundamentals.Generation
import TreeSitterGrammarNodes qualified as TSGN

-- Driver for generating example sentences
generateExamples :: TSGN.Grammar -> GenerationStrategy -> Int -> [Text]
generateExamples grammar strategy count =
  take count $ generateFromGrammar grammar strategy

-- Validate generated sentence against grammar
validateSentence :: TSGN.Grammar -> Text -> Bool
validateSentence grammar sentence = -- Implementation using tree-sitter
```

### 7. Generation Strategies

#### 7.1 Random Generation
- Select random alternatives at CHOICE nodes
- Random repetition counts (within bounds)
- Configurable seed for reproducibility

#### 7.2 Guided Generation
- Apply constraints (e.g., "generate if statements")
- Follow specific alternative paths
- Respect depth limits

#### 7.3 Exhaustive Generation
- Generate all possible combinations (up to depth limit)
- Useful for testing and validation
- Can be limited by combinatorial explosion

### 8. Factory System Design

#### 8.1 Static Factory Methods
```typescript
export class IfStatement_T extends SyntaticInterior {
  static create(condition: Expression_T, body: Statement_T): IfStatement_T {
    const instance = new IfStatement_T();
    instance.condition_0_i = condition;
    instance.body_0_i = body;
    return instance;
  }
}
```

#### 8.2 Builder Pattern
```typescript
export class IfStatementBuilder {
  private condition?: Expression_T;
  private body?: Statement_T;

  withCondition(cond: Expression_T): this {
    this.condition = cond;
    return this;
  }

  withBody(b: Statement_T): this {
    this.body = b;
    return this;
  }

  build(): IfStatement_T {
    const instance = new IfStatement_T();
    instance.condition_0_i = this.condition!;
    instance.body_0_i = this.body!;
    return instance;
  }
}
```

### 9. Validation System

#### 9.1 Grammar Conformance
- Generated sentences should parse according to original grammar
- Use tree-sitter to validate syntax
- Report validation errors

#### 9.2 Property Constraints
- Required fields must be set
- Type constraints must be satisfied
- Cardinality constraints (REPEAT, REPEAT1)

### 10. Test Strategy

#### 10.1 Unit Tests
- Test each generation algorithm independently
- Test choice resolution strategies
- Test repetition handling

#### 10.2 Integration Tests
- End-to-end generation from grammar.json
- Validate generated TypeScript compiles
- Test factory methods work correctly

#### 10.3 Property Tests
- Generated sentences should parse correctly
- Factory methods should create valid instances
- evaluate() should produce valid syntax

### 11. Implementation Plan

#### Phase 1: Core Generation Algorithms
1. Implement `Fundamentals/Generation.hs`
2. Add basic evaluation for all node types
3. Implement random generation strategy

#### Phase 2: Code Generation Enhancements
1. Enhance `ProgBuilderForECMA.hs` to generate evaluate() methods
2. Add template functions in `TypeScriptTemplate.hs`
3. Generate basic factory methods

#### Phase 3: Advanced Features
1. Implement guided and exhaustive strategies
2. Add builder pattern generation
3. Implement validation system

#### Phase 4: Testing and Refinement
1. Add comprehensive test suite
2. Optimize generation algorithms
3. Document usage examples

### 12. Performance Considerations

#### 12.1 Memory Usage
- Limit recursion depth
- Use lazy generation where possible
- Stream generated sentences

#### 12.2 Generation Speed
- Cache evaluated sub-expressions
- Prune impossible branches early
- Parallelize independent generations

### 13. Extensibility

#### 13.1 Custom Generation Strategies
- Allow user-defined strategy plugins
- Configurable via command line
- Extensible strategy interface

#### 13.2 Language Extensions
- Support for other target languages (Python, Java, etc.)
- Custom templates per language
- Language-specific optimizations

## Conclusion

This architecture provides a comprehensive plan for completing the TreeSitterAST Code Generator to produce TypeScript classes capable of generating valid sentences from Tree-sitter grammars. The design maintains modularity, extensibility, and follows Haskell best practices.

The implementation will transform the current skeleton generator into a powerful tool for grammar-based code generation, testing, and validation.