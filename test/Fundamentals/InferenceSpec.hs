{-# LANGUAGE OverloadedStrings #-}
module Fundamentals.InferenceSpec (inference_spec) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import qualified TreeSitterGrammarNodes as TSGN
import Fundamentals.Inference

import Data.ByteString.Lazy.Char8 qualified as BS
import qualified Data.Map as Map

-- | Parse a grammar from JSON string.
parseGrammar :: BS.ByteString -> Maybe TSGN.Grammar
parseGrammar = TSGN.parseGrammarFromJSON

-- | Test grammar with a top-most CHOICE node.
grammarWithChoice :: BS.ByteString
grammarWithChoice = BS.pack $ unlines
  [ "{"
  , "  \"name\": \"test\","
  , "  \"rules\": {"
  , "    \"rule1\": {"
  , "      \"type\": \"CHOICE\","
  , "      \"members\": ["
  , "        {\"type\": \"SYMBOL\", \"name\": \"A\"},"
  , "        {\"type\": \"SYMBOL\", \"name\": \"B\"}"
  , "      ]"
  , "    }"
  , "  }"
  , "}"
  ]

-- | Test grammar with nested CHOICE inside SEQ.
grammarWithNestedChoice :: BS.ByteString
grammarWithNestedChoice = BS.pack $ unlines
  [ "{"
  , "  \"name\": \"test\","
  , "  \"rules\": {"
  , "    \"rule1\": {"
  , "      \"type\": \"SEQ\","
  , "      \"members\": ["
  , "        {"
  , "          \"type\": \"CHOICE\","
  , "          \"members\": ["
  , "            {\"type\": \"SYMBOL\", \"name\": \"A\"},"
  , "            {\"type\": \"SYMBOL\", \"name\": \"B\"}"
  , "          ]"
  , "        },"
  , "        {\"type\": \"SYMBOL\", \"name\": \"C\"}"
  , "      ]"
  , "    }"
  , "  }"
  , "}"
  ]

-- | Test grammar with CHOICE containing precedence nodes.
grammarWithPrecedenceChoices :: BS.ByteString
grammarWithPrecedenceChoices = BS.pack $ unlines
  [ "{"
  , "  \"name\": \"test\","
  , "  \"rules\": {"
  , "    \"rule1\": {"
  , "      \"type\": \"CHOICE\","
  , "      \"members\": ["
  , "        {\"type\": \"PREC\", \"value\": 1, \"content\": {\"type\": \"SYMBOL\", \"name\": \"A\"}},"
  , "        {\"type\": \"PREC_LEFT\", \"value\": \"foo\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"B\"}},"
  , "        {\"type\": \"PREC_RIGHT\", \"value\": 2, \"content\": {\"type\": \"SYMBOL\", \"name\": \"C\"}},"
  , "        {\"type\": \"PREC_DYNAMIC\", \"value\": \"bar\", \"content\": {\"type\": \"SYMBOL\", \"name\": \"D\"}},"
  , "        {\"type\": \"SYMBOL\", \"name\": \"E\"}"
  , "      ]"
  , "    }"
  , "  }"
  , "}"
  ]

inference_spec :: TestTree
inference_spec = testGroup "Inference Tests"
  [ testCase "splitChoiceRule empties top-most CHOICE" $ do
      let grammar = parseGrammar grammarWithChoice
      case grammar of
        Nothing -> assertFailure "Failed to parse grammar"
        Just g -> do
          let transformed = transformGrammarWithChoiceSplitting g
              nodes = TSGN.grammarNodes transformed
          -- Original rule should have empty CHOICE members
          case Map.lookup "rule1" nodes of
            Nothing -> assertFailure "rule1 not found"
            Just node -> case node of
              TSGN.Choice members -> length members @?= 0
              _ -> assertFailure "rule1 is not a CHOICE node"
          -- New rules should exist
          Map.lookup "rule1_A" nodes @?= Just (TSGN.Symbol "A")
          Map.lookup "rule1_B" nodes @?= Just (TSGN.Symbol "B")
  , testCase "nested CHOICE is not emptied" $ do
      let grammar = parseGrammar grammarWithNestedChoice
      case grammar of
        Nothing -> assertFailure "Failed to parse grammar"
        Just g -> do
          let transformed = transformGrammarWithChoiceSplitting g
              nodes = TSGN.grammarNodes transformed
          -- Original rule should still have nested CHOICE with two members
          case Map.lookup "rule1" nodes of
            Nothing -> assertFailure "rule1 not found"
            Just node -> case node of
              TSGN.Seq members -> case members of
                [choice, _sym] -> case choice of
                  TSGN.Choice choiceMembers -> length choiceMembers @?= 2
                  _ -> assertFailure "first member is not CHOICE"
                _ -> assertFailure "expected two members in SEQ"
              _ -> assertFailure "rule1 is not a SEQ node"
          -- No new rules should be created (since CHOICE is nested)
          Map.lookup "rule1_0" nodes @?= Nothing
          Map.lookup "rule1_1" nodes @?= Nothing
  , testCase "precedence identifiers are used for Prec* alternatives" $ do
      let grammar = parseGrammar grammarWithPrecedenceChoices
      case grammar of
        Nothing -> assertFailure "Failed to parse grammar"
        Just g -> do
          let transformed = transformGrammarWithChoiceSplitting g
              nodes = TSGN.grammarNodes transformed
          -- Original rule should have empty CHOICE members
          case Map.lookup "rule1" nodes of
            Nothing -> assertFailure "rule1 not found"
            Just node -> case node of
              TSGN.Choice members -> length members @?= 0
              _ -> assertFailure "rule1 is not a CHOICE node"
          -- New rules with precedence identifiers should exist
          Map.lookup "rule1_1" nodes @?= Just (TSGN.Symbol "A")
          Map.lookup "rule1_left_foo" nodes @?= Just (TSGN.Symbol "B")
          Map.lookup "rule1_right_2" nodes @?= Just (TSGN.Symbol "C")
          Map.lookup "rule1_dynamic_bar" nodes @?= Just (TSGN.Symbol "D")
          Map.lookup "rule1_E" nodes @?= Just (TSGN.Symbol "E")
  ]