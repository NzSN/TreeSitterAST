{-# LANGUAGE OverloadedStrings #-}

module TreeSitterGrammarNodesSpec (grammar_nodes_spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Map (toList, elems)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TreeSitterGrammarNodes

countAlias :: Node -> Int
countAlias node = case node of
  Seq members -> sum (map countAlias members)
  Choice members -> sum (map countAlias members)
  Repeat content -> countAlias content
  Repeat1 content -> countAlias content
  Field _ content -> countAlias content
  Alias content _ _ -> 1 + countAlias content
  Token content -> countAlias content
  ImmediateToken content -> countAlias content
  Prec _ content -> countAlias content
  PrecLeft _ content -> countAlias content
  PrecRight _ content -> countAlias content
  PrecDynamic _ content -> countAlias content
  Reserved content _ -> countAlias content
  _ -> 0

countAliasGrammar :: Grammar -> Int
countAliasGrammar grammar =
  sum (map countAlias (elems (grammarNodes grammar))) +
  sum (map countAlias (maybe [] id (grammarExternals grammar))) +
  sum (map countAlias (maybe [] id (grammarInline grammar))) +
  sum (map countAlias (maybe [] id (grammarSupertypes grammar))) +
  sum (concatMap (map countAlias) (maybe [] elems (grammarReserved grammar)))

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
            _ -> assertFailure "Expected Symbol rule",
      testCase "resolveAlias removes alias nodes except those referencing externals" $ do
        result <- runMaybeT $ parseGrammarFromFile "sample/grammar.json"
        case result of
          Nothing -> assertFailure "Failed to parse grammar.json"
          Just grammar -> do
            let aliasCount = countAliasGrammar grammar
            assertBool "Grammar should contain some alias nodes" (aliasCount > 0)
            let resolved = resolveAlias grammar
            let aliasCountAfter = countAliasGrammar resolved
            -- Two aliases reference external symbols (_template_chars and _ternary_qmark)
            aliasCountAfter @?= 2
    ]
