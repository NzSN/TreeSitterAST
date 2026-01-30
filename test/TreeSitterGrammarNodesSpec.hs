{-# LANGUAGE OverloadedStrings #-}

module TreeSitterGrammarNodesSpec (grammar_nodes_spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Map (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TreeSitterGrammarNodes

grammar_nodes_spec :: TestTree
grammar_nodes_spec =
  testGroup
    "TreeSitterGrammarNodes Tests"
    [ testCase "Parse sample grammar.json file" $ do
        result <- runMaybeT $ parseGrammarFromFile "sample/grammar.json"

        case result of
          Nothing -> assertFailure "Failed to parse grammar.json"
          Just grammar -> do
            -- Basic assertions about the parsed grammar
            grammarName grammar @?= "javascript"
            grammarWord grammar @?= Just "identifier"

            -- Check that rules are not empty
            let rules = grammarNodes grammar
            assertBool "Rules should not be empty" (not $ null $ toList rules)

            -- Check that the "program" rule exists and is a Seq
            case lookup "program" (toList rules) of
              Nothing -> assertFailure "Program rule not found"
              Just rule -> case rule of
                Seq members -> do
                  assertBool "Program rule should have members" (not $ null members)
                  -- Program rule should have at least 2 members (choice and repeat)
                  length members @?= 2
                _ -> assertFailure "Program rule should be a Seq"

            -- Check that "hash_bang_line" exists and is a Pattern
            case lookup "hash_bang_line" (toList rules) of
              Nothing -> assertFailure "hash_bang_line rule not found"
              Just rule -> case rule of
                Pattern value -> value @?= "#!.*"
                _ -> assertFailure "hash_bang_line should be a Pattern"

            -- Check that "export_statement" exists
            case lookup "export_statement" (toList rules) of
              Nothing -> assertFailure "export_statement rule not found"
              Just rule -> case rule of
                Choice members -> assertBool "export_statement should have members" (not $ null members)
                _ -> assertFailure "export_statement should be a Choice",
      testCase "Parse simple rule from JSON string" $ do
        let jsonStr = "{\"type\": \"STRING\", \"value\": \"export\"}"
        case parseNodeFromJSON jsonStr of
          Nothing -> assertFailure "Failed to parse simple string rule"
          Just rule -> case rule of
            StringLiteral value -> value @?= "export"
            _ -> assertFailure "Expected StringLiteral rule",
      testCase "Parse symbol rule from JSON string" $ do
        let jsonStr = "{\"type\": \"SYMBOL\", \"name\": \"statement\"}"
        case parseNodeFromJSON jsonStr of
          Nothing -> assertFailure "Failed to parse symbol rule"
          Just rule -> case rule of
            Symbol name -> name @?= "statement"
            _ -> assertFailure "Expected Symbol rule"
    ]
