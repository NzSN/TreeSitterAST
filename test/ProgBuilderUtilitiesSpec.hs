{-# LANGUAGE OverloadedStrings #-}

module ProgBuilderUtilitiesSpec (prog_builder_utilities_spec) where

import qualified Data.Text.Lazy as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import ProgBuilder.ProgBuilderDescription (convergeNamedProp, Property(..))

prog_builder_utilities_spec :: TestTree
prog_builder_utilities_spec =
  testGroup
    "ProgBuilder.Utilities Tests"
    [ testCase "merge NamedProps across branches" $ do
        let p1 = SymbolProp (T.pack "t1")
            p2 = SymbolProp (T.pack "t2")
            np1 = NamedProp (T.pack "field") [p1]   -- branch 0
            np2 = NamedProp (T.pack "field") [p2]   -- branch 1
            br = [[np1], [np2]]
            out = convergeNamedProp br
        -- Expect merged to branch 0 (minimum index)
        case out of
          [branch0, branch1] -> do
            let fieldCount0 = length [() | NamedProp n _ <- branch0, n == T.pack "field"]
                fieldCount1 = length [() | NamedProp n _ <- branch1, n == T.pack "field"]
            fieldCount0 @?= 1
            fieldCount1 @?= 0
            -- Check that merged NamedProp contains both types
            case branch0 of
              [NamedProp _ types] -> do
                let hasT1 = any (\case SymbolProp t -> t == T.pack "t1"; _ -> False) types
                    hasT2 = any (\case SymbolProp t -> t == T.pack "t2"; _ -> False) types
                assertBool "Should contain SymbolProp \"t1\"" hasT1
                assertBool "Should contain SymbolProp \"t2\"" hasT2
              _ -> assertFailure "Expected single NamedProp in branch0"
          _ -> assertFailure "Expected two branches"
    , testCase "merge nested NamedProps across branches" $ do
        let p1 = SymbolProp (T.pack "t1")
            p2 = SymbolProp (T.pack "t2")
            inner1 = NamedProp (T.pack "inner") [p1]  -- branch 0
            inner2 = NamedProp (T.pack "inner") [p2]  -- branch 1
            outer1 = NamedProp (T.pack "outer") [inner1]
            outer2 = NamedProp (T.pack "outer") [inner2]
            br = [[outer1], [outer2]]
            out = convergeNamedProp br
        case out of
          [branch0, branch1] -> do
            let outerCount0 = length [() | NamedProp n _ <- branch0, n == T.pack "outer"]
                outerCount1 = length [() | NamedProp n _ <- branch1, n == T.pack "outer"]
            outerCount0 @?= 1
            outerCount1 @?= 0
            -- Check inner types inside outer
            case branch0 of
              [NamedProp _ types] -> do
                let innerCount = length [() | NamedProp n _ <- types, n == T.pack "inner"]
                innerCount @?= 1
                -- Check that inner contains both SymbolProps
                case types of
                  [NamedProp _ innerTypes] -> do
                    let hasT1 = any (\case SymbolProp t -> t == T.pack "t1"; _ -> False) innerTypes
                        hasT2 = any (\case SymbolProp t -> t == T.pack "t2"; _ -> False) innerTypes
                    assertBool "Inner should contain SymbolProp \"t1\"" hasT1
                    assertBool "Inner should contain SymbolProp \"t2\"" hasT2
                  _ -> assertFailure "Expected single inner NamedProp"
              _ -> assertFailure "Expected single outer NamedProp in branch0"
          _ -> assertFailure "Expected two branches"
    ]
