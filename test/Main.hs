module Main (main) where

import Test.Tasty

import Template.TypeScriptTemplateSpec
import Fundamentals.FileSpec (file_spec)
import TreeSitterGrammarNodesSpec (grammar_nodes_spec)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [template_testcase, file_spec, grammar_nodes_spec]
