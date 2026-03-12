{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tests using TemplateEvalIterator to systematically cover all evaluation paths
module Utilities.TemplateEvalIteratorSpec (template_eval_iterator_spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Map qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TreeSitterGrammarNodes qualified as TSGN
import Utilities.TemplateEvalIterator

-- | Test spec for TemplateEvalIterator
template_eval_iterator_spec :: TestTree
template_eval_iterator_spec =
  testGroup
    "TemplateEvalIterator Tests"
    [ testCase "enumerates paths for simple choice grammar" $ do
        result <- iterateAllEvaluations "test/ProgBuilder/TestGrammars/simple_choice.json" $ \_output -> do
          -- Just return True to indicate success
          putStrLn _output
          return True
        case result of
          Left err -> assertFailure err
          Right _results -> putStrLn $ "Got " ++ show (length _results) ++ " results"
      -- testCase "enumerates paths for typescript expressions grammar" $ do
      --   result <- iterateAllEvaluations "test/ProgBuilder/TestGrammars/typescript_expressions.json" $ \_output -> do
      --     -- Just return True to indicate success
      --     return True
      --   case result of
      --     Left err -> assertFailure err
      --     Right _results -> putStrLn $ "Got " ++ show (length _results) ++ " results",
      -- testCase "context provider can add variable declarations" $ do
      --   -- Test that context provider is called and modifies the code
      --   let testCode = "let result = a + b;"
      --       context = defaultContextProvider
      --       output = provideContext context testCode
      --   -- The context should add some declarations
      --   assertBool "Context should modify the code" (length output > length testCode),
      -- testCase "default context provider extracts identifiers" $ do
      --   let testCode = "let x = a + b;"
      --       context = defaultContextProvider
      --       output = provideContext context testCode
      --   -- Should add variable declarations for 'x', 'a', 'b'
      --   assertBool "Should contain 'const'" ("const" `elem` words output),
      -- testCase "noOp context provider returns code unchanged" $ do
      --   let testCode = "let result = 42;"
      --       output = provideContext noOpContextProvider testCode
      --   testCode @?= output,
      -- -- Tests for nested CHOICE coverage
      -- testCase "findAllChoices finds nested CHOICEs in nested_choice.json" $ do
      --   mgrammar <- runMaybeT $ TSGN.parseGrammarFromFile "test/ProgBuilder/TestGrammars/nested_choice.json"
      --   case mgrammar of
      --     Nothing -> assertFailure "Failed to parse nested_choice.json"
      --     Just grammar -> do
      --       let rules = TSGN.grammarNodes (TSGN.orig (TSGN.convert grammar))
      --           allChoices = findAllChoicesInGrammar rules
      --           -- Count total choices across all rules
      --           totalChoiceCount = sum $ map length $ Map.elems allChoices

      --       -- nested_choice.json should have multiple nested CHOICEs
      --       assertBool "Should find multiple CHOICEs (including nested)" (totalChoiceCount >= 4),
      -- testCase "findAllChoices finds deeply nested CHOICEs in deeply_nested.json" $ do
      --   mgrammar <- runMaybeT $ TSGN.parseGrammarFromFile "test/ProgBuilder/TestGrammars/deeply_nested.json"
      --   case mgrammar of
      --     Nothing -> assertFailure "Failed to parse deeply_nested.json"
      --     Just grammar -> do
      --       let rules = TSGN.grammarNodes (TSGN.orig (TSGN.convert grammar))
      --           allChoices = findAllChoicesInGrammar rules
      --           totalChoiceCount = sum $ map length $ Map.elems allChoices

      --       -- deeply_nested.json has many nested CHOICEs
      --       assertBool "Should find many CHOICEs in deeply_nested.json" (totalChoiceCount >= 10),
      -- testCase "enumeratePaths generates paths for ALL rules" $ do
      --   mgrammar <- runMaybeT $ TSGN.parseGrammarFromFile "test/ProgBuilder/TestGrammars/nested_choice.json"
      --   case mgrammar of
      --     Nothing -> assertFailure "Failed to parse nested_choice.json"
      --     Just grammar -> do
      --       let rules = TSGN.grammarNodes (TSGN.orig (TSGN.convert grammar))
      --           paths = enumeratePaths rules
      --           ruleCount = Map.size rules

      --       -- Each path should cover all rules (Cartesian product)
      --       -- So path length should equal number of rules
      --       case paths of
      --         [] -> assertFailure "Should have at least one path"
      --         (firstPath : _) -> do
      --           -- Each path should have one EvalPath per rule
      --           assertBool "Each path should cover all rules" (length firstPath == ruleCount),
      -- testCase "enumerateTopLevelPaths finds CHOICEs within a single rule" $ do
      --   mgrammar <- runMaybeT $ TSGN.parseGrammarFromFile "test/ProgBuilder/TestGrammars/nested_choice.json"
      --   case mgrammar of
      --     Nothing -> assertFailure "Failed to parse nested_choice.json"
      --     Just grammar -> do
      --       let rules = TSGN.grammarNodes (TSGN.orig (TSGN.convert grammar))

      --       -- Test binary_expression which has a nested CHOICE for operator
      --       case Map.lookup "binary_expression" rules of
      --         Nothing -> assertFailure "binary_expression rule not found"
      --         Just node -> do
      --           let paths = enumerateTopLevelPaths "binary_expression" node
      --           case paths of
      --             [] -> assertFailure "Should have at least one path"
      --             (firstPath : _) -> putStrLn $ "Sample path: " ++ show firstPath
      --           -- binary_expression has a nested CHOICE (operator) with 3 alternatives
      --           assertBool "Should find paths for nested CHOICE in binary_expression" (length paths >= 1),
      -- testCase "ChoiceInfo contains correct location path for nested CHOICEs" $ do
      --   mgrammar <- runMaybeT $ TSGN.parseGrammarFromFile "test/ProgBuilder/TestGrammars/nested_choice.json"
      --   case mgrammar of
      --     Nothing -> assertFailure "Failed to parse nested_choice.json"
      --     Just grammar -> do
      --       let rules = TSGN.grammarNodes (TSGN.orig (TSGN.convert grammar))

      --       -- Find choices in binary_expression
      --       case Map.lookup "binary_expression" rules of
      --         Nothing -> assertFailure "binary_expression rule not found"
      --         Just node -> do
      --           let choices = findAllChoices node [] 0
      --           mapM_ (\c -> putStrLn $ "  Location: " ++ show (choiceLocation c) ++ ", alternatives: " ++ show (choiceAlternatives c)) choices

      --           -- The nested CHOICE for operator should have a non-empty location path
      --           let nestedChoice = filter (\c -> not (null (choiceLocation c))) choices
      --           assertBool "Should find at least one nested CHOICE with non-empty location" (not (null nestedChoice))
    ]
