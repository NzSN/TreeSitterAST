# src/TypedASTGenerator/AGENTS.md

Node description patterns for TreeSitterAST typed AST generation.

## Module Overview

### Core Modules
- **`NodeDescription.hs`** - Node description definitions
- **`NodeDescriptionHelper.hs`** - Helper functions for node descriptions
- **`NodeProcessorDescription.hs`** - Node processor descriptions

## Node Description System

### Node Description Types

#### Basic Node Description
```haskell
-- In NodeDescription.hs
data NodeDescription = NodeDescription
  { nodeName :: Text
  , nodeType :: NodeType
  , fields :: [FieldDescription]
  , children :: [ChildDescription]
  }
  deriving (Show, Generic)
```

#### Field Description
```haskell
data FieldDescription = FieldDescription
  { fieldName :: Text
  , fieldType :: FieldType
  , isOptional :: Bool
  , defaultValue :: Maybe Text
  }
  deriving (Show, Generic)
```

#### Child Description
```haskell
data ChildDescription = ChildDescription
  { childName :: Text
  , childType :: NodeType
  , isMultiple :: Bool
  , isOptional :: Bool
  }
  deriving (Show, Generic)
```

### Node Type Classification
```haskell
data NodeType
  = LeafNode
  | InteriorNode
  | ChoiceNode Text [NodeType]
  | ReferenceNode Text
  deriving (Show, Eq, Generic)
```

## Node Description Patterns

### Leaf Node Pattern
```haskell
leafNodeDescription :: Text -> NodeDescription
leafNodeDescription name = NodeDescription
  { nodeName = name
  , nodeType = LeafNode
  , fields = []
  , children = []
  }
```

### Interior Node Pattern
```haskell
interiorNodeDescription :: Text -> [FieldDescription] -> [ChildDescription] -> NodeDescription
interiorNodeDescription name fs cs = NodeDescription
  { nodeName = name
  , nodeType = InteriorNode
  , fields = fs
  , children = cs
  }
```

### Choice Node Pattern
```haskell
choiceNodeDescription :: Text -> [NodeType] -> NodeDescription
choiceNodeDescription name alternatives = NodeDescription
  { nodeName = name
  , nodeType = ChoiceNode name alternatives
  , fields = []
  , children = []
  }
```

## Field Description Patterns

### Required Field
```haskell
requiredField :: Text -> FieldType -> FieldDescription
requiredField name ftype = FieldDescription
  { fieldName = name
  , fieldType = ftype
  , isOptional = False
  , defaultValue = Nothing
  }
```

### Optional Field
```haskell
optionalField :: Text -> FieldType -> Maybe Text -> FieldDescription
optionalField name ftype defVal = FieldDescription
  { fieldName = name
  , fieldType = ftype
  , isOptional = True
  , defaultValue = defVal
  }
```

### Field Type Definitions
```haskell
data FieldType
  = StringType
  | NumberType
  | BooleanType
  | CustomType Text
  | UnionType [FieldType]
  deriving (Show, Eq, Generic)
```

## Node Description Helper Functions

### Field Type Conversion
```haskell
-- In NodeDescriptionHelper.hs
fieldTypeToText :: FieldType -> Text
fieldTypeToText StringType = "string"
fieldTypeToText NumberType = "number"
fieldTypeToText BooleanType = "boolean"
fieldTypeToText (CustomType t) = t
fieldTypeToText (UnionType ts) = T.intercalate " | " (map fieldTypeToText ts)
```

### Node Validation
```haskell
validateNodeDescription :: NodeDescription -> Either Text NodeDescription
validateNodeDescription nd = do
  unless (not (T.null (nodeName nd))) $ Left "Node name cannot be empty"
  unless (all (not . T.null . fieldName) (fields nd)) $ Left "Field names cannot be empty"
  unless (all (not . T.null . childName) (children nd)) $ Left "Child names cannot be empty"
  Right nd
```

### Node Transformation
```haskell
transformNodeDescription :: (NodeDescription -> NodeDescription) -> NodeDescription -> NodeDescription
transformNodeDescription f nd = f nd

-- Example: Add prefix to all field names
prefixFields :: Text -> NodeDescription -> NodeDescription
prefixFields prefix nd = nd { fields = map prefixField (fields nd) }
  where prefixField fd = fd { fieldName = prefix <> fieldName fd }
```

## Node Processor Descriptions

### Processor Description Structure
```haskell
-- In NodeProcessorDescription.hs
data NodeProcessorDescription = NodeProcessorDescription
  { processorName :: Text
  , inputType :: NodeType
  , outputType :: FieldType
  , processingLogic :: Text
  , dependencies :: [Text]
  }
  deriving (Show, Generic)
```

### Processor Patterns

#### Simple Processor
```haskell
simpleProcessor :: Text -> NodeType -> FieldType -> Text -> NodeProcessorDescription
simpleProcessor name input output logic = NodeProcessorDescription
  { processorName = name
  , inputType = input
  , outputType = output
  , processingLogic = logic
  , dependencies = []
  }
```

#### Composite Processor
```haskell
compositeProcessor :: Text -> NodeType -> FieldType -> [Text] -> Text -> NodeProcessorDescription
compositeProcessor name input output deps logic = NodeProcessorDescription
  { processorName = name
  , inputType = input
  , outputType = output
  , processingLogic = logic
  , dependencies = deps
  }
```

## Integration with Other Components

### Connection to ProgBuilder
- Node descriptions inform property generation
- Field types map to ProgBuilder `Field` representations
- Choice nodes correspond to CHOICE handling in ProgBuilder

### Connection to Template System
- Node descriptions influence template selection
- Field and child information used in template parameterization
- Processor descriptions may generate custom template logic

### Connection to Inference System
- Node types used in inference rule matching
- Field constraints inform type inference
- Choice node alternatives affect branch unification

## Complexity Considerations

### Large Modules
- **`NodeDescription.hs`** - ~500+ lines, extensive type definitions
- **`NodeProcessorDescription.hs`** - ~400+ lines, processor logic definitions
- **`NodeDescriptionHelper.hs`** - ~300+ lines, utility functions

### Design Patterns
- Use record syntax for all description types
- Provide validation functions for data integrity
- Support transformation pipelines for node processing

## Development Guidelines

### Adding New Node Types
1. Add variant to `NodeType` in `NodeDescription.hs`
2. Update pattern matching in helper functions
3. Add corresponding description constructor
4. Update validation and transformation logic
5. Test with sample node descriptions

### Extending Field Types
1. Add variant to `FieldType` in `NodeDescription.hs`
2. Update `fieldTypeToText` conversion
3. Add validation for new type
4. Update template generation if needed
5. Test TypeScript compatibility

### Creating Node Processors
1. Define processor in `NodeProcessorDescription.hs`
2. Specify input/output types and dependencies
3. Implement processing logic
4. Test processor integration
5. Document processor usage

## Testing Strategy

### Unit Tests
- Test node description validation
- Test field type conversions
- Test node transformation functions
- Test processor description validation

### Integration Tests
- Test node description to ProgBuilder conversion
- Test processor integration with inference system
- Test template generation from node descriptions

### Property-Based Tests
- QuickCheck properties for node validation
- Type conversion invariants
- Transformation function properties

## Best Practices

### Node Description Design
- Use descriptive node and field names
- Follow TypeScript naming conventions
- Document node purpose and usage
- Validate descriptions before use

### Processor Design
- Keep processors focused and single-purpose
- Document input/output contracts
- Manage processor dependencies carefully
- Test processor logic thoroughly

### Performance Considerations
- Cache frequently used node descriptions
- Use efficient data structures for large description sets
- Consider lazy evaluation for complex transformations