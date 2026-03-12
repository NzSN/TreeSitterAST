module Main (main) where

import Fundamentals.InferenceSpec (inference_spec)
import ProgBuilder.ECMA.ProgBuilderForECMASpec (prog_builder_ecma_spec)
import ProgBuilder.ECMA.SentenceGenerationSpec (sentence_generation_spec)
import ProgBuilderUtilitiesSpec (prog_builder_utilities_spec)
import StringEscapingTest (stringEscapingTests)
import Template.TypeScriptTemplateSpec
import Test.Tasty
import TreeSitterGrammarNodesSpec (grammar_nodes_spec)
import Utilities.TemplateEvalIteratorSpec (template_eval_iterator_spec)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ template_testcase,
      grammar_nodes_spec,
      prog_builder_utilities_spec,
      prog_builder_ecma_spec,
      sentence_generation_spec,
      template_eval_iterator_spec,
      inference_spec,
      stringEscapingTests
    ]
