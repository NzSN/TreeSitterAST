module Main (main) where

import Test.Tasty

import Template.TypeScriptTemplateSpec
import Fundamentals.FileSpec (file_spec)
import TreeSitterGrammarNodesSpec (grammar_nodes_spec)
import ProgBuilderUtilitiesSpec (prog_builder_utilities_spec)
import Fundamentals.InferenceSpec (inference_spec)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [template_testcase, file_spec, grammar_nodes_spec, prog_builder_utilities_spec, inference_spec]
