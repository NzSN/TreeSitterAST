# app/AGENTS.md

Entry point and CLI documentation for TreeSitterAST.

## Module Overview

### Core Modules
- **`Main.hs`** - CLI entry point and main executable
- **`Args.hs`** - Command-line argument parsing

## Command-Line Interface

### Modes of Operation

#### 1. AST Processor Mode (`--ast-proc`)
Generates TypeScript class declarations and node processors from `node-types.json`:

```bash
cabal run TreeSitterAST -- --ast-proc sample/node-types.json
```

**Output Files:**
- `node_declare.ts` - TypeScript class declarations for AST nodes
- `node_processor.ts` - TypeScript node processors for AST traversal

#### 2. Code Generator Mode (`--code-gen`)
Generates TypeScript classes and grammar representations from `grammar.json`:

```bash
cabal run TreeSitterAST -- --code-gen sample/grammar.json
```

**Output Files:**
- `<grammar_name>_rep.ts` - TypeScript classes for syntactic nodes
- `<grammar_name>_grammar.txt` - Text representation of parsed grammar

### Command-Line Options

#### Common Options
```bash
# Specify output directory
cabal run TreeSitterAST -- --ast-proc sample/node-types.json --output-dir generated
cabal run TreeSitterAST -- --ast-proc sample/node-types.json -o generated

# Help and version
cabal run TreeSitterAST -- --help
cabal run TreeSitterAST -- --version
```

#### Mode-Specific Options
```bash
# AST Processor mode
cabal run TreeSitterAST -- --ast-proc <node-types.json> [--output-dir <dir>]

# Code Generator mode  
cabal run TreeSitterAST -- --code-gen <grammar.json> [--output-dir <dir>]
```

## Argument Parsing

### Args.hs Structure
```haskell
-- Command modes
data Mode
  = AstProc FilePath
  | CodeGen FilePath
  deriving (Show, Eq)

-- Command-line arguments
data Args = Args
  { mode :: Mode
  , outputDir :: FilePath
  }
  deriving (Show, Eq)

-- Parser definition
argsParser :: Parser Args
argsParser = Args
  <$> modeParser
  <*> outputDirParser

modeParser :: Parser Mode
modeParser = 
  AstProc <$> astProcParser
  <|> CodeGen <$> codeGenParser

astProcParser :: Parser FilePath
astProcParser = strOption
  ( long "ast-proc"
  <> metavar "NODE_TYPES_JSON"
  <> help "Generate AST node declarations and processors from node-types.json" )

codeGenParser :: Parser FilePath
codeGenParser = strOption
  ( long "code-gen"
  <> metavar "GRAMMAR_JSON"
  <> help "Generate grammar-based code from grammar.json" )

outputDirParser :: Parser FilePath
outputDirParser = strOption
  ( short 'o'
  <> long "output-dir"
  <> metavar "DIR"
  <> value "."
  <> help "Output directory (default: current directory)" )
```

### Parser Execution
```haskell
parseArgs :: IO Args
parseArgs = execParser $ info (argsParser <**> helper)
  ( fullDesc
  <> progDesc "Generate TypeScript AST definitions from Tree-sitter grammar files"
  <> header "TreeSitterAST - TypeScript AST generator" )
```

## Main.hs Entry Point

### Program Flow
```haskell
main :: IO ()
main = do
  args <- parseArgs
  case mode args of
    AstProc inputFile -> do
      putStrLn $ "Processing AST from: " ++ inputFile
      result <- astGen inputFile (outputDir args)
      either printSuccess printError result
      
    CodeGen inputFile -> do
      putStrLn $ "Generating code from grammar: " ++ inputFile
      result <- progBuilderGen inputFile (outputDir args)
      either printSuccess printError result
```

### Mode Handlers

#### AST Processor Handler
```haskell
astGen :: FilePath -> FilePath -> IO (Either String ())
astGen input outDir = runMaybeT $ do
  nodes <- parse_node_types input
  let tsCode = generateTypeScript nodes
  liftIO $ writeFile (outDir </> "node_declare.ts") tsCode
  liftIO $ writeFile (outDir </> "node_processor.ts") (generateProcessors nodes)
  return ()
```

#### Code Generator Handler
```haskell
progBuilderGen :: FilePath -> FilePath -> IO (Either String ())
progBuilderGen input outDir = runMaybeT $ do
  grammar <- parse_grammar input
  let grammarName = getGrammarName grammar
  let tsCode = generateGrammarCode grammar
  let grammarText = renderGrammar grammar
  liftIO $ writeFile (outDir </> grammarName ++ "_rep.ts") tsCode
  liftIO $ writeFile (outDir </> grammarName ++ "_grammar.txt") grammarText
  return ()
```

## Error Handling

### Parse Errors
```haskell
handleParseError :: String -> IO ()
handleParseError err = do
  hPutStrLn stderr $ "Parse error: " ++ err
  exitWith (ExitFailure 1)
```

### File Errors
```haskell
handleFileError :: IOException -> IO ()
handleFileError e = do
  hPutStrLn stderr $ "File error: " ++ show e
  exitWith (ExitFailure 2)
```

### Success Reporting
```haskell
printSuccess :: () -> IO ()
printSuccess _ = putStrLn "Successfully generated TypeScript code."

printError :: String -> IO ()
printError err = do
  hPutStrLn stderr $ "Error: " ++ err
  exitWith (ExitFailure 1)
```

## Output File Naming

### Grammar-Based Naming
- Output files named using grammar's `name` field
- Example: JavaScript grammar → `javascript_rep.ts`, `javascript_grammar.txt`
- Extracted from `grammar.json` `"name"` field

### AST Processor Naming
- Fixed output names: `node_declare.ts`, `node_processor.ts`
- Consistent across all `node-types.json` inputs

## Cabal Configuration

### Executable Definition
```cabal
executable TreeSitterAST
  hs-source-dirs: app
  main-is: Main.hs
  other-modules: Args
  build-depends:
    base,
    optparse-applicative,
    TreeSitterAST,
    ...
  ghc-options: -Wall
```

### Build and Run
```bash
# Build executable
cabal build

# Run directly
cabal run TreeSitterAST -- --help

# Install to ~/.cabal/bin
cabal install
```

## Development Guidelines

### Adding New Modes
1. Add mode variant to `Mode` type in `Args.hs`
2. Add parser in `modeParser`
3. Add handler in `Main.hs` `main` function
4. Implement mode-specific logic
5. Update help text and documentation

### Extending Arguments
1. Add field to `Args` record
2. Add parser in `argsParser`
3. Update mode handlers to use new argument
4. Test argument parsing and validation

### Error Message Improvements
- Provide specific error messages for common failures
- Include file paths and line numbers when available
- Suggest corrective actions for user errors

## Testing CLI

### Unit Tests
- Test argument parsing with various inputs
- Test mode detection and validation
- Test error handling paths

### Integration Tests
- End-to-end CLI execution tests
- File generation verification
- Error condition testing

### Manual Testing
```bash
# Test help
cabal run TreeSitterAST -- --help

# Test version
cabal run TreeSitterAST -- --version

# Test AST processor
cabal run TreeSitterAST -- --ast-proc sample/node-types.json

# Test code generator
cabal run TreeSitterAST -- --code-gen sample/grammar.json

# Test output directory
cabal run TreeSitterAST -- --ast-proc sample/node-types.json -o test-output
```

## Performance Considerations

### Large File Handling
- Use streaming JSON parsing for large grammar files
- Implement progress indicators for long operations
- Consider memory usage for very large ASTs

### Output Streaming
- Stream output to files instead of building in memory
- Use lazy I/O for large generated files
- Consider incremental writing for very large outputs

## Best Practices

### CLI Design
- Follow GNU command-line conventions
- Provide clear help text
- Use consistent option naming
- Support both short and long options

### Error Reporting
- Provide actionable error messages
- Include context (file, line, column)
- Suggest solutions when possible

### User Experience
- Show progress for long operations
- Provide clear success messages
- Include summary of generated files