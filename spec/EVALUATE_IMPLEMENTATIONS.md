# Evaluate() Implementations and Factory System Design

## 1. Evaluate() Method Generation

### 1.1 Base Class Implementations

#### SyntaticNode (Abstract Base Class)
```typescript
export abstract class SyntaticNode {
  abstract evaluate(): string;

  // Helper method for validation
  validate(): boolean {
    try {
      const result = this.evaluate();
      // Basic validation - result should be non-empty for non-blank nodes
      return result.length > 0 || this instanceof BlankNode;
    } catch (e) {
      return false;
    }
  }
}
```

#### SyntaticLeaf (String Literals, Patterns)
```typescript
export class SyntaticLeaf extends SyntaticNode {
  value_: string;

  constructor(value: string) {
    super();
    this.value_ = value;
  }

  evaluate(): string {
    return this.value_;
  }

  // Factory method for leaf nodes
  static create(value: string): SyntaticLeaf {
    return new SyntaticLeaf(value);
  }
}
```

#### SyntaticInterior (All Composite Nodes)
Base class with common functionality for all interior nodes.

### 1.2 Node-Specific Evaluate() Implementations

#### SEQ Nodes (Sequence)
```typescript
export class SequenceNode_T extends SyntaticInterior {
  members_0_i: SyntaticNode[];

  constructor(members: SyntaticNode[]) {
    super();
    this.members_0_i = members;
  }

  evaluate(): string {
    return this.members_0_i
      .map(member => member.evaluate())
      .join(''); // No separator by default
  }

  // Factory with variadic arguments
  static create(...members: SyntaticNode[]): SequenceNode_T {
    return new SequenceNode_T(members);
  }
}
```

#### CHOICE Nodes (Alternative Selection)
```typescript
export class ChoiceNode_T extends SyntaticInterior {
  selectedAlternative_0_i: SyntaticNode;
  alternatives_0_i: SyntaticNode[]; // All alternatives for regeneration

  constructor(selectedAlternative: SyntaticNode, alternatives: SyntaticNode[] = []) {
    super();
    this.selectedAlternative_0_i = selectedAlternative;
    this.alternatives_0_i = alternatives.length > 0 ? alternatives : [selectedAlternative];
  }

  evaluate(): string {
    return this.selectedAlternative_0_i.evaluate();
  }

  // Regenerate with different alternative
  regenerate(alternativeIndex: number): ChoiceNode_T {
    if (alternativeIndex < 0 || alternativeIndex >= this.alternatives_0_i.length) {
      throw new Error(`Invalid alternative index: ${alternativeIndex}`);
    }
    return new ChoiceNode_T(
      this.alternatives_0_i[alternativeIndex],
      this.alternatives_0_i
    );
  }

  // Factory that randomly selects an alternative
  static createRandom(alternatives: SyntaticNode[], seed?: number): ChoiceNode_T {
    const random = seed !== undefined ? new Random(seed) : Math.random();
    const index = Math.floor(random * alternatives.length);
    return new ChoiceNode_T(alternatives[index], alternatives);
  }
}
```

#### REPEAT Nodes (Zero or More)
```typescript
export class RepeatNode_T extends SyntaticInterior {
  content_0_i: SyntaticNode;
  minCount_0_i: number = 0;
  maxCount_0_i: number = 10; // Configurable limit
  separator_0_i: string = ''; // Configurable separator

  constructor(content: SyntaticNode, minCount: number = 0, maxCount: number = 10, separator: string = '') {
    super();
    this.content_0_i = content;
    this.minCount_0_i = minCount;
    this.maxCount_0_i = maxCount;
    this.separator_0_i = separator;
  }

  evaluate(): string {
    const count = this.determineCount();
    const parts: string[] = [];

    for (let i = 0; i < count; i++) {
      parts.push(this.content_0_i.evaluate());
    }

    return parts.join(this.separator_0_i);
  }

  private determineCount(): number {
    // Default implementation: random within bounds
    return Math.floor(
      Math.random() * (this.maxCount_0_i - this.minCount_0_i + 1)
    ) + this.minCount_0_i;
  }

  // Factory with configurable bounds
  static create(
    content: SyntaticNode,
    options: { min?: number; max?: number; separator?: string } = {}
  ): RepeatNode_T {
    return new RepeatNode_T(
      content,
      options.min ?? 0,
      options.max ?? 10,
      options.separator ?? ''
    );
  }
}
```

#### REPEAT1 Nodes (One or More)
```typescript
export class Repeat1Node_T extends RepeatNode_T {
  constructor(content: SyntaticNode, maxCount: number = 10, separator: string = '') {
    super(content, 1, maxCount, separator);
  }

  // Factory for REPEAT1
  static create(
    content: SyntaticNode,
    options: { max?: number; separator?: string } = {}
  ): Repeat1Node_T {
    return new Repeat1Node_T(
      content,
      options.max ?? 10,
      options.separator ?? ''
    );
  }
}
```

#### FIELD Nodes (Annotated Fields)
```typescript
export class FieldNode_T extends SyntaticInterior {
  fieldName_0_i: string;
  content_0_i: SyntaticNode;

  constructor(fieldName: string, content: SyntaticNode) {
    super();
    this.fieldName_0_i = fieldName;
    this.content_0_i = content;
  }

  evaluate(): string {
    return this.content_0_i.evaluate();
  }

  getFieldName(): string {
    return this.fieldName_0_i;
  }

  // Factory
  static create(fieldName: string, content: SyntaticNode): FieldNode_T {
    return new FieldNode_T(fieldName, content);
  }
}
```

### 1.3 Grammar-Specific Node Examples

#### If Statement Example
```typescript
export class If_statement_T extends SyntaticInterior {
  condition_0_i: Parenthesized_expression_T;
  body_0_i: Statement_T;
  elseClause_0_i?: Else_clause_T; // Optional

  constructor(
    condition: Parenthesized_expression_T,
    body: Statement_T,
    elseClause?: Else_clause_T
  ) {
    super();
    this.condition_0_i = condition;
    this.body_0_i = body;
    this.elseClause_0_i = elseClause;
  }

  evaluate(): string {
    const condition = this.condition_0_i.evaluate();
    const body = this.body_0_i.evaluate();
    const elsePart = this.elseClause_0_i?.evaluate() ?? '';

    return `if (${condition}) ${body}${elsePart}`;
  }

  // Multiple factory methods for different use cases
  static createSimple(condition: Expression_T, body: Statement_T): If_statement_T {
    const parenCondition = new Parenthesized_expression_T(condition);
    return new If_statement_T(parenCondition, body);
  }

  static createWithElse(
    condition: Expression_T,
    body: Statement_T,
    elseBody: Statement_T
  ): If_statement_T {
    const parenCondition = new Parenthesized_expression_T(condition);
    const elseClause = new Else_clause_T(elseBody);
    return new If_statement_T(parenCondition, body, elseClause);
  }
}
```

#### Function Call Example
```typescript
export class Call_expression_T extends SyntaticInterior {
  function_0_i: Expression_T;
  arguments_0_i: Arguments_T;

  constructor(func: Expression_T, args: Arguments_T) {
    super();
    this.function_0_i = func;
    this.arguments_0_i = args;
  }

  evaluate(): string {
    const func = this.function_0_i.evaluate();
    const args = this.arguments_0_i.evaluate();
    return `${func}${args}`;
  }

  // Builder pattern for complex construction
  static builder(): CallExpressionBuilder {
    return new CallExpressionBuilder();
  }
}
```

## 2. Factory System Design

### 2.1 Factory Method Patterns

#### Pattern 1: Static Factory Methods
```typescript
// Simple static factory
export class Variable_declaration_T extends SyntaticInterior {
  static create(
    name: Identifier_T,
    type?: Type_annotation_T,
    initializer?: Expression_T
  ): Variable_declaration_T {
    const declarator = Variable_declarator_T.create(name, initializer);
    return new Variable_declaration_T(declarator);
  }
}
```

#### Pattern 2: Builder Pattern
```typescript
export class Function_declarationBuilder {
  private name?: Identifier_T;
  private parameters: Parameter_T[] = [];
  private returnType?: Type_annotation_T;
  private body?: Statement_block_T;

  withName(name: Identifier_T): this {
    this.name = name;
    return this;
  }

  withParameter(param: Parameter_T): this {
    this.parameters.push(param);
    return this;
  }

  withReturnType(type: Type_annotation_T): this {
    this.returnType = type;
    return this;
  }

  withBody(body: Statement_block_T): this {
    this.body = body;
    return this;
  }

  build(): Function_declaration_T {
    if (!this.name || !this.body) {
      throw new Error('Name and body are required');
    }

    const params = new Parameters_T(this.parameters);
    return new Function_declaration_T(
      this.name,
      params,
      this.returnType,
      this.body
    );
  }
}
```

#### Pattern 3: Functional Factories
```typescript
// Pure functions that create nodes
export const createBinaryExpression = (
  left: Expression_T,
  operator: string,
  right: Expression_T
): Binary_expression_T => {
  return new Binary_expression_T(left, operator, right);
};

// Curried version for partial application
export const createBinaryExpressionWithOperator = (operator: string) =>
  (left: Expression_T, right: Expression_T): Binary_expression_T =>
    createBinaryExpression(left, operator, right);

export const createAddition = createBinaryExpressionWithOperator('+');
export const createMultiplication = createBinaryExpressionWithOperator('*');
```

### 2.2 Smart Factories with Inference

```typescript
// Factory that infers types from values
export class LiteralFactory {
  static create(value: any): SyntaticLeaf {
    if (typeof value === 'string') {
      return new String_literal_T(value);
    } else if (typeof value === 'number') {
      return new Number_literal_T(value.toString());
    } else if (typeof value === 'boolean') {
      return value ? new True_T() : new False_T();
    } else if (value === null) {
      return new Null_T();
    } else if (value === undefined) {
      return new Undefined_T();
    }
    throw new Error(`Unsupported literal type: ${typeof value}`);
  }
}

// Usage
const strLit = LiteralFactory.create("hello");
const numLit = LiteralFactory.create(42);
const boolLit = LiteralFactory.create(true);
```

### 2.3 Composite Factories

```typescript
// Factory that creates complex structures
export class ExpressionFactory {
  static createArithmeticExpression(
    left: number | Expression_T,
    operator: '+' | '-' | '*' | '/',
    right: number | Expression_T
  ): Binary_expression_T {
    const leftExpr = typeof left === 'number'
      ? LiteralFactory.create(left)
      : left;
    const rightExpr = typeof right === 'number'
      ? LiteralFactory.create(right)
      : right;

    return createBinaryExpression(leftExpr, operator, rightExpr);
  }

  static createFunctionCall(
    funcName: string,
    args: Array<number | string | boolean | Expression_T>
  ): Call_expression_T {
    const identifier = new Identifier_T(funcName);
    const argumentsList = args.map(arg => {
      if (arg instanceof SyntaticNode) {
        return arg;
      }
      return LiteralFactory.create(arg);
    });
    const argumentsNode = new Arguments_T(argumentsList);

    return new Call_expression_T(identifier, argumentsNode);
  }
}
```

## 3. Generation Strategies Integration

### 3.1 Strategy-Aware Evaluate Methods

```typescript
export class StrategyAwareNode extends SyntaticInterior {
  protected generationStrategy: GenerationStrategy = GenerationStrategy.Random;

  setGenerationStrategy(strategy: GenerationStrategy): void {
    this.generationStrategy = strategy;
    // Propagate to children
    this.propagateStrategyToChildren();
  }

  protected propagateStrategyToChildren(): void {
    // Implement in subclasses to set strategy on child nodes
  }
}

export class ChoiceNodeWithStrategy extends StrategyAwareNode {
  alternatives_0_i: SyntaticNode[];

  evaluate(): string {
    const selectedIndex = this.selectAlternativeIndex();
    return this.alternatives_0_i[selectedIndex].evaluate();
  }

  private selectAlternativeIndex(): number {
    switch (this.generationStrategy) {
      case GenerationStrategy.Random:
        return Math.floor(Math.random() * this.alternatives_0_i.length);
      case GenerationStrategy.Guided:
        return this.selectGuidedAlternative();
      case GenerationStrategy.Exhaustive:
        return this.selectExhaustiveAlternative();
      default:
        return 0;
    }
  }
}
```

### 3.2 Configurable Generation

```typescript
export interface GenerationConfig {
  strategy: GenerationStrategy;
  maxDepth: number;
  maxRepetitions: number;
  preferShortAlternatives: boolean;
  seed?: number;
}

export class ConfigurableNodeFactory {
  constructor(private config: GenerationConfig) {}

  createNode(nodeType: string, ...args: any[]): SyntaticNode {
    const node = this.createBaseNode(nodeType, ...args);

    if (node instanceof StrategyAwareNode) {
      node.setGenerationStrategy(this.config.strategy);
    }

    if (node instanceof RepeatNode_T || node instanceof Repeat1Node_T) {
      this.configureRepetitionNode(node);
    }

    return node;
  }

  private configureRepetitionNode(node: RepeatNode_T): void {
    node.maxCount_0_i = this.config.maxRepetitions;
  }
}
```

## 4. Code Generation Implementation

### 4.1 Haskell Code for Evaluate Method Generation

```haskell
-- In ProgBuilderForECMA.hs
generateEvaluateMethod :: TSGN.Node -> [Property] -> GenerationStrategy -> T.Text
generateEvaluateMethod node properties strategy =
  case node of
    TSGN.Seq members -> generateSeqEvaluate members properties
    TSGN.Choice alternatives -> generateChoiceEvaluate alternatives properties strategy
    TSGN.Repeat content -> generateRepeatEvaluate content properties strategy
    TSGN.Repeat1 content -> generateRepeat1Evaluate content properties strategy
    TSGN.Symbol name -> generateSymbolEvaluate name properties
    TSGN.StringLiteral value -> generateStringLiteralEvaluate value
    TSGN.Pattern value -> generatePatternEvaluate value
    _ -> generateDefaultEvaluate properties

generateSeqEvaluate :: [TSGN.Node] -> [Property] -> T.Text
generateSeqEvaluate members properties =
  let memberEvals = zipWith (\i _ -> T.concat ["this.member_", T.pack (show i), "_i.evaluate()"]) [0..] members
      joinedEvals = T.intercalate " + " memberEvals  -- Default: concatenate
  in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", joinedEvals, ";"]])

generateChoiceEvaluate :: [TSGN.Node] -> [Property] -> GenerationStrategy -> T.Text
generateChoiceEvaluate alternatives properties strategy =
  let altCount = length alternatives
      selectionLogic = case strategy of
        RandomStrategy -> T.concat ["const idx = Math.floor(Math.random() * ", T.pack (show altCount), ");"]
        GuidedStrategy -> "const idx = this.selectGuidedAlternative();"
        ExhaustiveStrategy -> "const idx = this.selectExhaustiveAlternative();"
      evalLogic = "return this.alternatives_0_i[idx].evaluate();"
  in TT.inst TTS.evaluate_method (TT.TArray [selectionLogic, evalLogic])
```

### 4.2 Factory Method Generation

```haskell
generateFactoryMethods :: String -> [Property] -> FactoryConfig -> T.Text
generateFactoryMethods className properties config =
  case factoryStyle config of
    StaticMethods -> generateStaticFactories className properties
    BuilderPattern -> generateBuilderPattern className properties
    FunctionalStyle -> generateFunctionalFactories className properties

generateStaticFactories :: String -> [Property] -> T.Text
generateStaticFactories className properties =
  let paramDecls = map propertyToParam properties
      paramNames = map (\i -> T.concat ["param", T.pack (show i)]) [0..length properties - 1]
      assignments = zipWith (\prop name -> T.concat ["instance.", fieldName prop, " = ", name, ";"]) properties paramNames
      factoryBody = T.concat [
        "const instance = new ", T.pack className, "();\n",
        T.intercalate "\n" assignments, "\n",
        "return instance;"
      ]
  in TT.inst TTS.static_factory_method
       (T.pack className)
       (TT.TArray paramDecls)
       (T.pack className)
       factoryBody
```

## 5. Usage Examples

### 5.1 Basic Usage
```typescript
// Create a simple expression
const expr = ExpressionFactory.createArithmeticExpression(5, '+', 3);
console.log(expr.evaluate()); // "5 + 3"

// Create an if statement
const condition = ExpressionFactory.createArithmeticExpression(10, '>', 5);
const body = new Statement_block_T([new Expression_statement_T(expr)]);
const ifStmt = If_statement_T.createSimple(condition, body);
console.log(ifStmt.evaluate()); // "if (10 > 5) { 5 + 3; }"
```

### 5.2 With Generation Strategy
```typescript
// Configure generation
const config: GenerationConfig = {
  strategy: GenerationStrategy.Guided,
  maxDepth: 5,
  maxRepetitions: 3,
  preferShortAlternatives: true
};

const factory = new ConfigurableNodeFactory(config);

// Create nodes with configured strategy
const choiceNode = factory.createNode('ChoiceNode', [alt1, alt2, alt3]);
console.log(choiceNode.evaluate()); // Uses guided selection
```

### 5.3 Builder Pattern
```typescript
// Build complex function declaration
const func = new Function_declarationBuilder()
  .withName(new Identifier_T('calculate'))
  .withParameter(new Parameter_T(new Identifier_T('x'), new Type_annotation_T('number')))
  .withParameter(new Parameter_T(new Identifier_T('y'), new Type_annotation_T('number')))
  .withReturnType(new Type_annotation_T('number'))
  .withBody(new Statement_block_T([
    new Return_statement_T(
      ExpressionFactory.createArithmeticExpression(
        new Identifier_T('x'),
        '+',
        new Identifier_T('y')
      )
    )
  ]))
  .build();

console.log(func.evaluate());
// "function calculate(x: number, y: number): number { return x + y; }"
```

## 6. Implementation Notes

### 6.1 Circular Dependencies
- Handle circular references in grammar definitions
- Use lazy evaluation or proxy objects
- Implement cycle detection

### 6.2 Performance Optimization
- Cache evaluated results for deterministic nodes
- Use memoization for expensive operations
- Implement incremental evaluation

### 6.3 Error Handling
- Validate node construction parameters
- Provide meaningful error messages
- Support recovery strategies

### 6.4 Testing
- Unit tests for each evaluate() implementation
- Property-based testing for generation strategies
- Integration tests for factory methods

This design provides a comprehensive framework for generating functional evaluate() methods and factory systems for TreeSitterAST-generated TypeScript classes.