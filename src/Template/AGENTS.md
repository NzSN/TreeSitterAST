# src/Template/AGENTS.md

Template system documentation for TreeSitterAST code generation.

## Module Overview

### Core Modules
- **`Template.hs`** - Foundation template system
- **`TypeScriptTemplate.hs`** - TypeScript-specific template generation

## Template System Architecture

### Template Types
```haskell
-- In Template.hs
type Template = Text -> Text
type TemplateT = Text -> Text

-- TypeScript-specific templates
type TSTemplate = Template
type TSTemplateT = TemplateT
```

### Template Composition
- **`TT`** - Alias for `Template.Template`
- **`TTS`** - Alias for `Template.TypeScriptTemplate`
- Template chaining via function composition

## Template Patterns

### Basic Template Pattern
```haskell
-- Simple text substitution
simpleTemplate :: Template
simpleTemplate = T.replace "{{name}}" "value"

-- Parameterized template
paramTemplate :: Text -> Template
paramTemplate param = \text -> T.replace "{{param}}" param text
```

### TypeScript Template Patterns

#### Class Template
```haskell
classTemplate :: Text -> Text -> Text -> Text
classTemplate className fields methods =
  "export class " <> className <> " extends SyntaticNode {\n" <>
  fields <>
  "  constructor() { super(); }\n" <>
  methods <>
  "}\n"
```

#### Method Template
```haskell
methodTemplate :: Text -> Text -> Text -> Text
methodTemplate methodName params body =
  "  " <> methodName <> "(" <> params <> "): string {\n" <>
  "    " <> body <> "\n" <>
  "  }\n"
```

#### Field Template
```haskell
fieldTemplate :: Text -> Text -> Bool -> Text
fieldTemplate fieldName fieldType isOptional =
  let typeStr = if isOptional then fieldType <> " | undefined" else fieldType
  in "  " <> fieldName <> ": " <> typeStr <> ";\n"
```

## Template Usage in Code Generation

### Integration with ProgBuilder
```haskell
-- In ProgBuilderForECMA.hs
import Template.Template as TT
import Template.TypeScriptTemplate as TTS

generateTypeScript :: GrammarNodeWithField -> Text
generateTypeScript node =
  let className = getClassName node
      fields = generateFields node
      methods = generateMethods node
  in TTS.classTemplate className fields methods
```

### Template Chaining
```haskell
-- Chain multiple templates
fullTemplate :: Template
fullTemplate = 
  TT.prologueTemplate 
  . TT.importTemplate 
  . TTS.classTemplate "MyClass" fields methods
  . TT.epilogueTemplate
```

## TypeScript-Specific Templates

### Prologue Template
```haskell
prologueTemplate :: Text
prologueTemplate = 
  "export class SyntaticNode {\n" <>
  "  evaluate(): string {\n" <>
  "    throw Error(\"Interior or Leaf should implement evaluate().\");\n" <>
  "  }\n" <>
  "}\n\n" <>
  "export class SyntaticLeaf extends SyntaticNode {\n" <>
  "  value_: string;\n" <>
  "  constructor(value: string) {\n" <>
  "    super();\n" <>
  "    this.value_ = value;\n" <>
  "  }\n" <>
  "  evaluate(): string {\n" <>
  "    return this.value_;\n" <>
  "  }\n" <>
  "}\n\n"
```

### Import Template
```haskell
importTemplate :: [Text] -> Text
importTemplate imports =
  if null imports 
    then ""
    else T.intercalate "\n" (map (\i -> "import " <> i <> ";") imports) <> "\n\n"
```

### Union Type Template
```haskell
unionTypeTemplate :: [Text] -> Text
unionTypeTemplate types =
  T.intercalate " | " (nub types)
```

## Template Customization

### Adding New Template Types
1. Define template function in `Template.hs` or `TypeScriptTemplate.hs`
2. Follow existing pattern: `templateName :: Parameters -> Text`
3. Export from module for use in other components
4. Add tests in `test/Template/TypeScriptTemplateSpec.hs`

### Template Parameters
- Use `Text` type for all template content
- Support optional parameters with default values
- Document parameter requirements in function comments

### Error Handling in Templates
- Templates should not fail on invalid input
- Use safe string operations (`T.replace`, `T.intercalate`)
- Provide default values for missing parameters

## Performance Considerations

### Template Caching
- Consider caching frequently used templates
- Use `Text` builders for large template concatenation
- Avoid repeated string operations in hot paths

### Memory Usage
- Templates generate `Text` values
- Use lazy `Text` (`Data.Text.Lazy`) for large outputs
- Consider streaming output for very large generated files

## Testing Templates

### Unit Test Pattern
```haskell
-- In TypeScriptTemplateSpec.hs
template_testcase :: TestTree
template_testcase = testGroup "Template Tests"
  [ testCase "class template generation" $ do
      let result = TTS.classTemplate "MyClass" "field: string;" "evaluate() { return ''; }"
      result @?= expectedOutput
  ]
```

### Test Coverage
- Test individual template functions
- Test template composition and chaining
- Test edge cases (empty input, special characters)
- Test TypeScript syntax validity of generated code

## Best Practices

### Template Design
- Keep templates focused and single-purpose
- Use descriptive parameter names
- Document template usage and output format
- Follow existing naming conventions

### Template Composition
- Compose small templates into larger ones
- Use function composition (`.`) for template chaining
- Avoid deep nesting of template logic

### Template Maintenance
- Update templates when TypeScript syntax changes
- Keep templates in sync with ProgBuilder output
- Test templates with sample grammars regularly

## Integration Points

### With ProgBuilder
- `ProgBuilderForECMA.hs` uses templates for TypeScript emission
- Field and method generation integrated with template system
- CHOICE node handling affects template parameterization

### With TypedASTGenerator
- Node descriptions may influence template selection
- Processor descriptions may require custom templates
- Template parameters derived from node metadata

### With Test Suite
- Templates tested in `TypeScriptTemplateSpec.hs`
- Integration tests verify template output correctness
- Sample files used for end-to-end template testing