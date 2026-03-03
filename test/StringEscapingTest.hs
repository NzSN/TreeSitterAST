module StringEscapingTest where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text.Lazy qualified as T

-- Test the escaping logic that should be in ProgBuilderForECMA.hs
-- This is a copy of the escapeTypeScriptString function for testing
escapeTypeScriptString :: T.Text -> T.Text
escapeTypeScriptString s = T.concatMap escapeChar s
  where
    escapeChar '"' = T.pack "\\\""
    escapeChar '\\' = T.pack "\\\\"
    escapeChar c = T.singleton c

stringEscapingTests :: TestTree
stringEscapingTests = testGroup "String escaping tests"
  [ testCase "Empty string" $
      escapeTypeScriptString (T.pack "") @?= T.pack ""
  , testCase "Plain text" $
      escapeTypeScriptString (T.pack "hello") @?= T.pack "hello"
  , testCase "Double quote" $
      escapeTypeScriptString (T.pack "\"") @?= T.pack "\\\""
  , testCase "Backslash" $
      escapeTypeScriptString (T.pack "\\") @?= T.pack "\\\\"
  , testCase "Mixed quotes and backslashes" $
      escapeTypeScriptString (T.pack "a\"b\\c\"d") @?= T.pack "a\\\"b\\\\c\\\"d"
  , testCase "Newline character (should not escape)" $
      -- \n is a single character, should remain as-is
      escapeTypeScriptString (T.pack "\n") @?= T.pack "\n"
  , testCase "Tab character (should not escape)" $
      escapeTypeScriptString (T.pack "\t") @?= T.pack "\t"
  , testCase "String with both quotes" $
      escapeTypeScriptString (T.pack "\"hello\"") @?= T.pack "\\\"hello\\\""
  , testCase "Backslash before quote" $
      escapeTypeScriptString (T.pack "\\\"") @?= T.pack "\\\\\\\""
  ]