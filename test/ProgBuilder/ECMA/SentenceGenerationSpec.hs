{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ECMA.SentenceGenerationSpec (sentence_generation_spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import ProgBuilder.ECMA.ProgBuilderForECMA (descript)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TreeSitterGrammarNodes (parseGrammarFromFile)

-- | Generate template classes from grammar and return as Text
generateTemplateClasses :: FilePath -> IO (Either String T.Text)
generateTemplateClasses grammarPath = do
  result <- runMaybeT $ parseGrammarFromFile grammarPath
  case result of
    Nothing -> return $ Left $ "Failed to parse grammar: " ++ grammarPath
    Just grammar -> return $ Right $ T.pack $ descript grammar

-- | Compile TypeScript to JavaScript and execute with Node.js
-- Uses a single output directory for all compiled files
-- Returns Right stdout on success, Left error message on failure
compileAndExecuteTypeScript ::
  T.Text ->
  T.Text ->
  String ->
  IO (Either String String)
compileAndExecuteTypeScript templateCode harnessCode testName = do
  let sanitizedTestName = map (\c -> if c `elem` (" -" :: String) then '_' else c) testName
      outDir = "/tmp/ts_test_" ++ sanitizedTestName
      templateFile = outDir ++ "/template.ts"
      harnessFile = outDir ++ "/harness.ts"

  -- Create output directory
  _ <- readProcessWithExitCode "mkdir" ["-p", outDir] ""

  -- Write TypeScript files
  TIO.writeFile templateFile templateCode
  TIO.writeFile harnessFile harnessCode

  -- Compile TypeScript to JavaScript
  (compileExitCode, compileStdout, compileStderr) <-
    readProcessWithExitCode
      "tsc"
      [ "--target",
        "ES2020",
        "--module",
        "commonjs",
        "--moduleResolution",
        "node",
        "--outDir",
        outDir,
        templateFile,
        harnessFile
      ]
      ""

  case compileExitCode of
    ExitFailure code ->
      return $
        Left $
          "TypeScript compilation failed with exit code "
            ++ show code
            ++ "\nstdout: "
            ++ compileStdout
            ++ "\nstderr: "
            ++ compileStderr
            ++ "\n\nTemplate file: "
            ++ templateFile
            ++ "\nHarness file: "
            ++ harnessFile
    ExitSuccess -> do
      -- Run the compiled JavaScript with Node.js
      let jsHarnessFile = outDir ++ "/harness.js"
      (runExitCode, runStdout, runStderr) <-
        readProcessWithExitCode "node" [jsHarnessFile] ""

      case runExitCode of
        ExitFailure code ->
          return $
            Left $
              "Node.js execution failed with exit code "
                ++ show code
                ++ "\nstdout: "
                ++ runStdout
                ++ "\nstderr: "
                ++ runStderr
                ++ "\nJS file: "
                ++ jsHarnessFile
        ExitSuccess ->
          return $ Right runStdout

-- | Test that a generated sentence compiles as valid TypeScript
assertSentenceCompiles ::
  FilePath ->
  String ->
  (String -> T.Text) ->
  Assertion
assertSentenceCompiles grammarPath testName harnessGenerator = do
  templateResult <- generateTemplateClasses grammarPath
  case templateResult of
    Left err -> assertFailure err
    Right templateCode -> do
      let harnessCode = harnessGenerator "./template"
      compileResult <- compileAndExecuteTypeScript templateCode harnessCode testName
      case compileResult of
        Left err -> assertFailure err
        Right _output -> return ()

-- | Test that generated sentence compiles AND the evaluate() output compiles as standalone TypeScript
assertSentenceEvaluatesToCompilableCode ::
  FilePath ->
  String ->
  (String -> T.Text) ->
  (String -> String) ->
  Assertion
assertSentenceEvaluatesToCompilableCode grammarPath testName harnessGenerator contextProvider = do
  templateResult <- generateTemplateClasses grammarPath
  case templateResult of
    Left err -> assertFailure err
    Right templateCode -> do
      let harnessCode = harnessGenerator "./template"

      -- Compile and execute to get the evaluate() output
      execResult <- compileAndExecuteTypeScript templateCode harnessCode testName
      case execResult of
        Left err -> assertFailure err
        Right generatedCode -> do
          -- The generated code is captured from stdout
          let trimmedCode = dropTrailingNewline generatedCode
              -- Add context to make the generated code compilable
              fullCode = contextProvider trimmedCode
              sanitizedTestName = map (\c -> if c `elem` (" -" :: String) then '_' else c) testName
              standaloneFile = "/tmp/standalone_" ++ sanitizedTestName ++ ".ts"

          -- Write the generated code (with context) to a file
          TIO.writeFile standaloneFile (T.pack fullCode)

          -- Verify the generated code compiles with tsc
          (exitCode, _stdout, stderr) <-
            readProcessWithExitCode "tsc" ["--noEmit", "--target", "ES2020", standaloneFile] ""

          case exitCode of
            ExitSuccess -> return ()
            ExitFailure code ->
              assertFailure $
                "Generated code failed to compile: "
                  ++ show code
                  ++ "\nGenerated code (from evaluate()):\n"
                  ++ trimmedCode
                  ++ "\n\nFull code with context:\n"
                  ++ fullCode
                  ++ "\nstderr: "
                  ++ stderr
  where
    dropTrailingNewline s =
      if not (null s) && last s == '\n'
        then init s
        else s

sentence_generation_spec :: TestTree
sentence_generation_spec =
  testGroup
    "Sentence Generation Tests"
    [ testCase "generates valid identifier expression" $ do
        let harnessGenerator importPath =
              T.concat
                [ "import { Identifier_T, Expression_identifier_T } from '",
                  T.pack importPath,
                  "';\n",
                  "const id = new Identifier_T('x');\n",
                  "const expr = new Expression_identifier_T(id);\n",
                  "const generated: string = expr.evaluate();\n",
                  "console.log(generated);\n"
                ]
        assertSentenceCompiles
          "test/ProgBuilder/TestGrammars/typescript_expressions.json"
          "generates_valid_identifier_expression"
          harnessGenerator,
      testCase "generates valid binary expression" $ do
        let harnessGenerator importPath =
              T.concat
                [ "import { Identifier_T, Binary_expression_T, Expression_binary_expression_T } from '",
                  T.pack importPath,
                  "';\n",
                  "const left = new Identifier_T('x');\n",
                  "const right = new Identifier_T('y');\n",
                  "const binaryExpr = new Binary_expression_T(left, right);\n",
                  "const expr = new Expression_binary_expression_T(binaryExpr);\n",
                  "const generated: string = expr.evaluate();\n",
                  "console.log(generated);\n"
                ]
        assertSentenceCompiles
          "test/ProgBuilder/TestGrammars/typescript_expressions.json"
          "generates_valid_binary_expression"
          harnessGenerator,
      testCase "generates valid call expression" $ do
        let harnessGenerator importPath =
              T.concat
                [ "import { Identifier_T, Call_expression_T, Expression_call_expression_T } from '",
                  T.pack importPath,
                  "';\n",
                  "const funcName = new Identifier_T('foo');\n",
                  "const callExpr = new Call_expression_T(funcName);\n",
                  "const expr = new Expression_call_expression_T(callExpr);\n",
                  "const generated: string = expr.evaluate();\n",
                  "console.log(generated);\n"
                ]
        assertSentenceCompiles
          "test/ProgBuilder/TestGrammars/typescript_expressions.json"
          "generates_valid_call_expression"
          harnessGenerator,
      testCase "generates valid variable declaration" $ do
        let harnessGenerator importPath =
              T.concat
                [ "import { Identifier_T, Expression_identifier_T, Variable_declaration_T, Statement_variable_declaration_T } from '",
                  T.pack importPath,
                  "';\n",
                  "const varName = new Identifier_T('x');\n",
                  "const varValue = new Identifier_T('y');\n",
                  "const valueExpr = new Expression_identifier_T(varValue);\n",
                  "const varDecl = new Variable_declaration_T(varName, valueExpr);\n",
                  "const stmt = new Statement_variable_declaration_T(varDecl);\n",
                  "const generated: string = stmt.evaluate();\n",
                  "console.log(generated);\n"
                ]
        assertSentenceCompiles
          "test/ProgBuilder/TestGrammars/typescript_expressions.json"
          "generates_valid_variable_declaration"
          harnessGenerator,
      testCase "generates valid expression statement" $ do
        let harnessGenerator importPath =
              T.concat
                [ "import { Identifier_T, Call_expression_T, Expression_call_expression_T, Expression_statement_T, Statement_expression_statement_T } from '",
                  T.pack importPath,
                  "';\n",
                  "const funcName = new Identifier_T('foo');\n",
                  "const callExpr = new Call_expression_T(funcName);\n",
                  "const expr = new Expression_call_expression_T(callExpr);\n",
                  "const exprStmt = new Expression_statement_T(expr);\n",
                  "const stmt = new Statement_expression_statement_T(exprStmt);\n",
                  "const generated: string = stmt.evaluate();\n",
                  "console.log(generated);\n"
                ]
        assertSentenceCompiles
          "test/ProgBuilder/TestGrammars/typescript_expressions.json"
          "generates_valid_expression_statement"
          harnessGenerator,
      testCase "generates valid return statement" $ do
        let harnessGenerator importPath =
              T.concat
                [ "import { Identifier_T, Expression_identifier_T, Return_statement_T, Statement_return_statement_T } from '",
                  T.pack importPath,
                  "';\n",
                  "const retVal = new Identifier_T('x');\n",
                  "const expr = new Expression_identifier_T(retVal);\n",
                  "const retStmt = new Return_statement_T(expr);\n",
                  "const stmt = new Statement_return_statement_T(retStmt);\n",
                  "const generated: string = stmt.evaluate();\n",
                  "console.log(generated);\n"
                ]
        assertSentenceCompiles
          "test/ProgBuilder/TestGrammars/typescript_expressions.json"
          "generates_valid_return_statement"
          harnessGenerator,
      -- The key test: verify that the GENERATED code (from evaluate()) compiles
      testCase "generated code (from evaluate()) is valid TypeScript that compiles standalone" $ do
        let harnessGenerator importPath =
              T.concat
                [ "import { Identifier_T, Binary_expression_T, Expression_binary_expression_T, Variable_declaration_T, Statement_variable_declaration_T } from '",
                  T.pack importPath,
                  "';\n",
                  "const left = new Identifier_T('a');\n",
                  "const right = new Identifier_T('b');\n",
                  "const binaryExpr = new Binary_expression_T(left, right);\n",
                  "const expr = new Expression_binary_expression_T(binaryExpr);\n",
                  "const varName = new Identifier_T('result');\n",
                  "const varDecl = new Variable_declaration_T(varName, expr);\n",
                  "const stmt = new Statement_variable_declaration_T(varDecl);\n",
                  "const generatedCode = stmt.evaluate();\n",
                  "console.log(generatedCode);\n"
                ]
            -- Context provider: add variable declarations to make generated code compilable
            contextProvider generatedCode =
              "const a = 1;\nconst b = 2;\n" ++ generatedCode
        assertSentenceEvaluatesToCompilableCode
          "test/ProgBuilder/TestGrammars/typescript_expressions.json"
          "generated_code_is_valid_TypeScript_that_compiles_standalone"
          harnessGenerator
          contextProvider
    ]
