{-# LANGUAGE OverloadedStrings #-}

module Fundamentals.GenerationSpec (generation_spec) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as T
import qualified Data.Map as Map

import TreeSitterGrammarNodes qualified as TSGN
import Fundamentals.Generation

-- Test grammar nodes
testSymbolTable :: TSGN.Nodes
testSymbolTable = Map.fromList
  [ ("literal", TSGN.StringLiteral "hello")
  , ("pattern", TSGN.Pattern "[a-z]+")
  , ("seq_example", TSGN.Seq [TSGN.Symbol "literal", TSGN.StringLiteral " world"])
  , ("choice_example", TSGN.Choice [TSGN.StringLiteral "option1", TSGN.StringLiteral "option2"])
  , ("repeat_example", TSGN.Repeat (TSGN.StringLiteral "x"))
  , ("repeat1_example", TSGN.Repeat1 (TSGN.StringLiteral "y"))
  , ("field_example", TSGN.Field "name" (TSGN.StringLiteral "value"))
  , ("alias_example", TSGN.Alias (TSGN.StringLiteral "content") True "alias")
  , ("token_example", TSGN.Token (TSGN.StringLiteral "token"))
  , ("blank_example", TSGN.Blank)
  , ("empty_example", TSGN.Empty)
  ]

-- Helper to run generation
runTestGeneration :: TSGN.Node -> GenerationStrategy -> IO (Maybe T.Text)
runTestGeneration node strategy = do
  initState <- defaultContext strategy testSymbolTable
  return $ runGeneration initState (evaluateNode node)

generation_spec :: TestTree
generation_spec = testGroup "Generation Tests"
  [ testCase "StringLiteral generation" $ do
      result <- runTestGeneration (TSGN.StringLiteral "test") (RandomStrategy Nothing)
      result @?= Just "test"

  , testCase "Pattern generation" $ do
      result <- runTestGeneration (TSGN.Pattern "[a-z]+") (RandomStrategy Nothing)
      result @?= Just "[a-z]+"

  , testCase "Blank generation" $ do
      result <- runTestGeneration TSGN.Blank (RandomStrategy Nothing)
      result @?= Just ""

  , testCase "Empty generation" $ do
      result <- runTestGeneration TSGN.Empty (RandomStrategy Nothing)
      result @?= Just ""

  , testCase "Symbol lookup and generation" $ do
      result <- runTestGeneration (TSGN.Symbol "literal") (RandomStrategy Nothing)
      result @?= Just "hello"

  , testCase "Symbol lookup failure" $ do
      result <- runTestGeneration (TSGN.Symbol "nonexistent") (RandomStrategy Nothing)
      result @?= Nothing

  , testCase "SEQ generation" $ do
      result <- runTestGeneration (TSGN.Seq [TSGN.StringLiteral "a", TSGN.StringLiteral "b"]) (RandomStrategy Nothing)
      result @?= Just "ab"

  , testCase "SEQ with depth limit" $ do
      -- Create a context with very low maxDepth
      let strategy = RandomStrategy Nothing
      initState <- defaultContext strategy testSymbolTable
      -- Modify the context to have maxDepth = 0
      let (gen, ctx) = initState
      let ctx' = ctx { maxDepth = 0 }
      let initState' = (gen, ctx')
      -- Try to evaluate a SEQ node (which increments depth)
      let result = runGeneration initState' (evaluateNode (TSGN.Seq [TSGN.StringLiteral "a", TSGN.StringLiteral "b"]))
      result @?= Nothing

  , testCase "FIELD generation" $ do
      result <- runTestGeneration (TSGN.Field "name" (TSGN.StringLiteral "value")) (RandomStrategy Nothing)
      result @?= Just "value"

  , testCase "ALIAS generation" $ do
      result <- runTestGeneration (TSGN.Alias (TSGN.StringLiteral "content") True "alias") (RandomStrategy Nothing)
      result @?= Just "content"

  , testCase "TOKEN generation" $ do
      result <- runTestGeneration (TSGN.Token (TSGN.StringLiteral "token")) (RandomStrategy Nothing)
      result @?= Just "token"

  , testCase "Generate sentence from rule" $ do
      result <- generateSentence (RandomStrategy Nothing) testSymbolTable "literal"
      result @?= Just "hello"

  , testCase "Generate multiple sentences" $ do
      -- Create a simple grammar
      let grammar = TSGN.Grammar
            { TSGN.grammarName = "test"
            , TSGN.grammarWord = Nothing
            , TSGN.grammarNodes = testSymbolTable
            , TSGN.grammarExternals = Nothing
            , TSGN.grammarInline = Nothing
            , TSGN.grammarSupertypes = Nothing
            , TSGN.grammarReserved = Nothing
            }
      sentences <- generateSentences grammar (RandomStrategy Nothing) 3
      -- Should generate sentences from the first 3 rules
      length sentences <= 3 @? "Should generate at most 3 sentences"

  , testGroup "Random Strategy Tests"
    [ testCase "CHOICE with random strategy" $ do
        -- Test that CHOICE nodes work with random strategy
        -- We'll run it multiple times to see if we get different results
        results <- mapM (\_ -> runTestGeneration (TSGN.Choice [TSGN.StringLiteral "a", TSGN.StringLiteral "b"]) (RandomStrategy Nothing)) ([1..10] :: [Int])
        -- All results should be valid
        all (`elem` [Just "a", Just "b"]) results @? "All results should be 'a' or 'b'"

    , testCase "REPEAT with random strategy" $ do
        result <- runTestGeneration (TSGN.Repeat (TSGN.StringLiteral "x")) (RandomStrategy Nothing)
        -- Result should be a string of 0-5 "x"s separated by spaces
        case result of
          Just text -> do
            if T.null text
              then return ()  -- Empty string is valid for REPEAT with count = 0
              else do
                let xs = filter (not . T.null) $ T.splitOn " " text
                let allX = all (== "x") xs
                length xs <= 5 @? "Should have at most 5 repetitions"
                allX @? "All elements should be 'x'"
          Nothing -> assertFailure "Should have generated a result"

    , testCase "REPEAT1 with random strategy" $ do
        result <- runTestGeneration (TSGN.Repeat1 (TSGN.StringLiteral "y")) (RandomStrategy Nothing)
        -- Result should be a string of 1-5 "y"s separated by spaces
        case result of
          Just text -> do
            let ys = filter (not . T.null) $ T.splitOn " " text
            let allY = all (== "y") ys
            length ys >= 1 && length ys <= 5 @? "Should have 1-5 repetitions"
            allY @? "All elements should be 'y'"
          Nothing -> assertFailure "Should have generated a result"
    ]

  , testGroup "Non-Random Strategy Tests"
    [ testCase "GuidedStrategy defaults to first alternative" $ do
        result <- runTestGeneration (TSGN.Choice [TSGN.StringLiteral "first", TSGN.StringLiteral "second"]) (GuidedStrategy [])
        result @?= Just "first"

    , testCase "ExhaustiveStrategy defaults to first alternative" $ do
        result <- runTestGeneration (TSGN.Choice [TSGN.StringLiteral "first", TSGN.StringLiteral "second"]) (ExhaustiveStrategy 5)
        result @?= Just "first"
    ]
  ]