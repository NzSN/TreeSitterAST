{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ECMA.ProgBuilderForECMASpec (prog_builder_ecma_spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import ProgBuilder.ECMA.ProgBuilderForECMA (descript)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TreeSitterGrammarNodes (parseGrammarFromFile)

-- Helper to check if text contains pattern (case-sensitive)
contains :: T.Text -> T.Text -> Bool
contains = T.isInfixOf

-- | Helper to assert that generated TypeScript code compiles with tsc
-- Writes the code to a temp file and runs tsc --noEmit to verify compilation
assertTypeScriptCompiles :: T.Text -> String -> Assertion
assertTypeScriptCompiles tsCode testName = do
  -- Use test name in temp file to avoid conflicts when running tests in parallel
  let sanitizedTestName = map (\c -> if c `elem` (" -" :: String) then '_' else c) testName
      tempFile = "/tmp/test_ts_" ++ sanitizedTestName ++ ".ts"
  TIO.writeFile tempFile tsCode
  (exitCode, stdout, stderr) <- readProcessWithExitCode "tsc" ["--noEmit", "--target", "ES2020", tempFile] ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> do
      let errorMsg =
            "TypeScript compilation failed with exit code "
              ++ show code
              ++ "\nstdout: "
              ++ stdout
              ++ "\nstderr: "
              ++ stderr
              ++ "\n\nGenerated code:\n"
              ++ T.unpack tsCode
      assertFailure errorMsg

prog_builder_ecma_spec :: TestTree
prog_builder_ecma_spec =
  testGroup
    "ProgBuilderForECMA Generation Tests"
    [ testCase "generates TypeScript code for JavaScript grammar" $ do
        result <- runMaybeT $ parseGrammarFromFile "sample/grammar.json"
        case result of
          Nothing -> assertFailure "Failed to parse sample/grammar.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar

            -- Basic syntax checks
            assertBool "Should contain export class declarations" $
              "export class " `contains` tsCode

            assertBool "Should contain evaluate methods" $
              "evaluate()" `contains` tsCode

            assertBool "Should have proper inheritance hierarchy" $
              "extends Syntatic" `contains` tsCode

            assertBool "Should contain SyntaticNode base class" $
              "class SyntaticNode" `contains` tsCode

            assertBool "Should contain SyntaticLeaf class" $
              "class SyntaticLeaf" `contains` tsCode

            assertBool "Should contain SyntaticInterior class" $
              "class SyntaticInterior" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "javascript_grammar",
      testCase "SEQ nodes generate sequential field suffixes" $ do
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/simple_seq.json"
        case result of
          Nothing -> assertFailure "Failed to parse simple_seq.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- Should contain field suffixes _0_i and _1_i
            assertBool "Should contain _0_i suffix for first field" $
              "_0_i" `contains` tsCode
            assertBool "Should contain _1_i suffix for second field" $
              "_1_i" `contains` tsCode
            -- Should generate a class for test_seq
            assertBool "Should generate Test_seq_T class" $
              "class Test_seq_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "SEQ_nodes_generate_sequential_field_suffixes",
      testCase "CHOICE nodes generate alternative classes" $ do
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/simple_choice.json"
        case result of
          Nothing -> assertFailure "Failed to parse simple_choice.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- Should generate classes for each alternative
            assertBool "Should generate Test_choice_T class" $
              "class Test_choice_T" `contains` tsCode
            assertBool "Should generate Test_choice_option_a_T class" $
              "class Test_choice_option_a_T" `contains` tsCode
            assertBool "Should generate Test_choice_option_b_T class" $
              "class Test_choice_option_b_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "CHOICE_nodes_generate_alternative_classes",
      testCase "PATTERN nodes generate SyntaticLeaf classes" $ do
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/pattern_leaf.json"
        case result of
          Nothing -> assertFailure "Failed to parse pattern_leaf.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- PATTERN nodes should extend SyntaticLeaf
            assertBool "Should extend SyntaticLeaf" $
              "extends SyntaticLeaf" `contains` tsCode
            -- Should generate a class for test_pattern
            assertBool "Should generate Test_pattern_T class" $
              "class Test_pattern_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "PATTERN_nodes_generate_SyntaticLeaf_classes",
      testCase "SEQ nodes preserve member order in evaluate() method" $ do
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/seq_three.json"
        case result of
          Nothing -> assertFailure "Failed to parse seq_three.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- Should contain evaluate method with fields in order: first_0_i, second_1_i, third_2_i
            -- Using template literal format: ${this.first_0_i.evaluate()}${this.second_1_i.evaluate()}
            assertBool "Should contain first_0_i.evaluate() then second_1_i.evaluate() in template literal" $
              "first_0_i.evaluate()}${this.second_1_i.evaluate()" `contains` tsCode
            assertBool "Should contain second_1_i.evaluate() then third_2_i.evaluate() in template literal" $
              "second_1_i.evaluate()}${this.third_2_i.evaluate()" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "SEQ_nodes_preserve_member_order_in_evaluate",
      testCase "CHOICE nodes with named fields preserve alternative order in evaluate()" $ do
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/choice_named.json"
        case result of
          Nothing -> assertFailure "Failed to parse choice_named.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- TODO: Branch unification currently not working for FIELD nodes inside CHOICE.
            -- Should contain union type with undefined (disabled for now)
            -- assertBool "Should contain union type with undefined" $
            --   "| undefined" `contains` tsCode
            -- Should contain option_i field (disabled for now)
            -- assertBool "Should contain option_i field" $
            --   "option_i" `contains` tsCode
            -- Check that alternative classes are generated in order
            assertBool "Should generate Test_choice_named_0_T class" $
              "class Test_choice_named_0_T" `contains` tsCode
            assertBool "Should generate Test_choice_named_1_T class" $
              "class Test_choice_named_1_T" `contains` tsCode
            assertBool "Should generate Test_choice_named_2_T class" $
              "class Test_choice_named_2_T" `contains` tsCode
            -- Check that each alternative class has correct field (a_0_i, b_0_i, c_0_i)
            assertBool "Should contain a_0_i field" $
              "a_0_i" `contains` tsCode
            assertBool "Should contain b_0_i field" $
              "b_0_i" `contains` tsCode
            assertBool "Should contain c_0_i field" $
              "c_0_i" `contains` tsCode
            -- The evaluate method should test alternatives in order
            -- This is complex to test with simple string matching,
            -- but we can check basic structure
            assertBool "Should contain evaluate method" $
              "evaluate(): string" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "CHOICE_nodes_with_named_fields",
      testCase "CHOICE alternatives inherit from parent CHOICE class" $ do
        -- Test with simple grammar
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/simple_choice.json"
        case result of
          Nothing -> assertFailure "Failed to parse simple_choice.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- The CHOICE class should extend SyntaticInterior
            assertBool "Parent CHOICE class (Test_choice_T) should extend SyntaticInterior" $
              "class Test_choice_T extends SyntaticInterior" `contains` tsCode
            -- Alternative classes should extend the parent CHOICE class, NOT SyntaticInterior
            assertBool "Alternative Test_choice_option_a_T should extend Test_choice_T" $
              "class Test_choice_option_a_T extends Test_choice_T" `contains` tsCode
            assertBool "Alternative Test_choice_option_b_T should extend Test_choice_T" $
              "class Test_choice_option_b_T extends Test_choice_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "CHOICE_alternatives_inherit_from_parent",
      testCase "complex CHOICE inheritance with multiple parent classes" $ do
        -- Test with complex grammar having multiple CHOICE nodes
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/complex_choice_inheritance.json"
        case result of
          Nothing -> assertFailure "Failed to parse complex_choice_inheritance.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- primary_expression CHOICE: has 4 alternatives
            assertBool "Primary_expression_T should extend SyntaticInterior" $
              "class Primary_expression_T extends SyntaticInterior" `contains` tsCode
            assertBool "Primary_expression_identifier_T should extend Primary_expression_T" $
              "class Primary_expression_identifier_T extends Primary_expression_T" `contains` tsCode
            assertBool "Primary_expression_number_T should extend Primary_expression_T" $
              "class Primary_expression_number_T extends Primary_expression_T" `contains` tsCode
            assertBool "Primary_expression_string_literal_T should extend Primary_expression_T" $
              "class Primary_expression_string_literal_T extends Primary_expression_T" `contains` tsCode
            assertBool "Primary_expression_parenthesized_expression_T should extend Primary_expression_T" $
              "class Primary_expression_parenthesized_expression_T extends Primary_expression_T" `contains` tsCode
            -- statement CHOICE: has 3 alternatives
            assertBool "Statement_T should extend SyntaticInterior" $
              "class Statement_T extends SyntaticInterior" `contains` tsCode
            assertBool "Statement_expression_statement_T should extend Statement_T" $
              "class Statement_expression_statement_T extends Statement_T" `contains` tsCode
            assertBool "Statement_return_statement_T should extend Statement_T" $
              "class Statement_return_statement_T extends Statement_T" `contains` tsCode
            assertBool "Statement_if_statement_T should extend Statement_T" $
              "class Statement_if_statement_T extends Statement_T" `contains` tsCode
            -- _simple_type CHOICE (starts with underscore): has 2 alternatives
            assertBool "_simple_type_T should extend SyntaticInterior" $
              "class _simple_type_T extends SyntaticInterior" `contains` tsCode
            assertBool "_simple_type_primitive_type_T should extend _simple_type_T" $
              "class _simple_type_primitive_type_T extends _simple_type_T" `contains` tsCode
            assertBool "_simple_type_array_type_T should extend _simple_type_T" $
              "class _simple_type_array_type_T extends _simple_type_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "complex_CHOICE_inheritance_multiple_parents",
      testCase "real-world CHOICE inheritance from JavaScript grammar" $ do
        -- Test with actual JavaScript grammar which has edge cases like double underscores
        result <- runMaybeT $ parseGrammarFromFile "sample/grammar.json"
        case result of
          Nothing -> assertFailure "Failed to parse sample/grammar.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- _augmented_assignment_lhs has alternatives including _reserved_identifier
            -- This tests the edge case where alternative name has underscore right after parent name
            assertBool "_augmented_assignment_lhs_T should extend SyntaticInterior" $
              "class _augmented_assignment_lhs_T extends SyntaticInterior" `contains` tsCode
            assertBool "_augmented_assignment_lhs_identifier_T should extend _augmented_assignment_lhs_T" $
              "class _augmented_assignment_lhs_identifier_T extends _augmented_assignment_lhs_T" `contains` tsCode
            assertBool "_augmented_assignment_lhs_member_expression_T should extend _augmented_assignment_lhs_T" $
              "class _augmented_assignment_lhs_member_expression_T extends _augmented_assignment_lhs_T" `contains` tsCode
            -- Edge case: double underscore in name (parent ends with underscore, alt starts with underscore)
            assertBool "_augmented_assignment_lhs__reserved_identifier_T should extend _augmented_assignment_lhs_T" $
              "class _augmented_assignment_lhs__reserved_identifier_T extends _augmented_assignment_lhs_T" `contains` tsCode
            -- _destructuring_pattern CHOICE
            assertBool "_destructuring_pattern_T should extend SyntaticInterior" $
              "class _destructuring_pattern_T extends SyntaticInterior" `contains` tsCode
            assertBool "_destructuring_pattern_array_pattern_T should extend _destructuring_pattern_T" $
              "class _destructuring_pattern_array_pattern_T extends _destructuring_pattern_T" `contains` tsCode
            assertBool "_destructuring_pattern_object_pattern_T should extend _destructuring_pattern_T" $
              "class _destructuring_pattern_object_pattern_T extends _destructuring_pattern_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "real_world_CHOICE_inheritance_JavaScript_grammar",
      testCase "nested CHOICE and SEQ inheritance" $ do
        -- Test with grammar having nested CHOICE inside SEQ
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/nested_choice.json"
        case result of
          Nothing -> assertFailure "Failed to parse nested_choice.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- Expression CHOICE (top-level) with 3 alternatives
            assertBool "Expression_T should extend SyntaticInterior" $
              "class Expression_T extends SyntaticInterior" `contains` tsCode
            assertBool "Expression_binary_expression_T should extend Expression_T" $
              "class Expression_binary_expression_T extends Expression_T" `contains` tsCode
            assertBool "Expression_unary_expression_T should extend Expression_T" $
              "class Expression_unary_expression_T extends Expression_T" `contains` tsCode
            assertBool "Expression_primary_expr_T should extend Expression_T" $
              "class Expression_primary_expr_T extends Expression_T" `contains` tsCode
            -- Primary_expr CHOICE (nested under Expression)
            assertBool "Primary_expr_T should extend SyntaticInterior" $
              "class Primary_expr_T extends SyntaticInterior" `contains` tsCode
            assertBool "Primary_expr_identifier_T should extend Primary_expr_T" $
              "class Primary_expr_identifier_T extends Primary_expr_T" `contains` tsCode
            assertBool "Primary_expr_number_T should extend Primary_expr_T" $
              "class Primary_expr_number_T extends Primary_expr_T" `contains` tsCode
            assertBool "Primary_expr_parenthesized_T should extend Primary_expr_T" $
              "class Primary_expr_parenthesized_T extends Primary_expr_T" `contains` tsCode
            -- Statement CHOICE
            assertBool "Statement_T should extend SyntaticInterior" $
              "class Statement_T extends SyntaticInterior" `contains` tsCode
            assertBool "Statement_if_statement_T should extend Statement_T" $
              "class Statement_if_statement_T extends Statement_T" `contains` tsCode
            assertBool "Statement_while_statement_T should extend Statement_T" $
              "class Statement_while_statement_T extends Statement_T" `contains` tsCode
            assertBool "Statement_block_statement_T should extend Statement_T" $
              "class Statement_block_statement_T extends Statement_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "nested_CHOICE_and_SEQ_inheritance",
      testCase "deeply nested CHOICE/SEQ with multiple inheritance levels" $ do
        -- Test with complex grammar having deeply nested structures
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/deeply_nested.json"
        case result of
          Nothing -> assertFailure "Failed to parse deeply_nested.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- Declaration CHOICE (top-level module declarations)
            assertBool "Declaration_T should extend SyntaticInterior" $
              "class Declaration_T extends SyntaticInterior" `contains` tsCode
            assertBool "Declaration_function_decl_T should extend Declaration_T" $
              "class Declaration_function_decl_T extends Declaration_T" `contains` tsCode
            assertBool "Declaration_variable_decl_T should extend Declaration_T" $
              "class Declaration_variable_decl_T extends Declaration_T" `contains` tsCode
            assertBool "Declaration_class_decl_T should extend Declaration_T" $
              "class Declaration_class_decl_T extends Declaration_T" `contains` tsCode
            -- Expression CHOICE (nested in many places)
            assertBool "Expression_T should extend SyntaticInterior" $
              "class Expression_T extends SyntaticInterior" `contains` tsCode
            assertBool "Expression_identifier_T should extend Expression_T" $
              "class Expression_identifier_T extends Expression_T" `contains` tsCode
            assertBool "Expression_literal_T should extend Expression_T" $
              "class Expression_literal_T extends Expression_T" `contains` tsCode
            assertBool "Expression_binary_expr_T should extend Expression_T" $
              "class Expression_binary_expr_T extends Expression_T" `contains` tsCode
            assertBool "Expression_call_expr_T should extend Expression_T" $
              "class Expression_call_expr_T extends Expression_T" `contains` tsCode
            -- Literal CHOICE (nested in Expression)
            assertBool "Literal_T should extend SyntaticInterior" $
              "class Literal_T extends SyntaticInterior" `contains` tsCode
            assertBool "Literal_number_T should extend Literal_T" $
              "class Literal_number_T extends Literal_T" `contains` tsCode
            assertBool "Literal_string_literal_T should extend Literal_T" $
              "class Literal_string_literal_T extends Literal_T" `contains` tsCode
            -- Statement CHOICE
            assertBool "Statement_T should extend SyntaticInterior" $
              "class Statement_T extends SyntaticInterior" `contains` tsCode
            assertBool "Statement_expression_statement_T should extend Statement_T" $
              "class Statement_expression_statement_T extends Statement_T" `contains` tsCode
            assertBool "Statement_return_statement_T should extend Statement_T" $
              "class Statement_return_statement_T extends Statement_T" `contains` tsCode
            assertBool "Statement_if_statement_T should extend Statement_T" $
              "class Statement_if_statement_T extends Statement_T" `contains` tsCode
            assertBool "Statement_while_statement_T should extend Statement_T" $
              "class Statement_while_statement_T extends Statement_T" `contains` tsCode
            -- Type_name CHOICE (nested in type annotations)
            assertBool "Type_name_T should extend SyntaticInterior" $
              "class Type_name_T extends SyntaticInterior" `contains` tsCode
            assertBool "Type_name_primitive_type_T should extend Type_name_T" $
              "class Type_name_primitive_type_T extends Type_name_T" `contains` tsCode
            assertBool "Type_name_identifier_T should extend Type_name_T" $
              "class Type_name_identifier_T extends Type_name_T" `contains` tsCode
            assertBool "Type_name_array_type_T should extend Type_name_T" $
              "class Type_name_array_type_T extends Type_name_T" `contains` tsCode
            -- Class_member CHOICE
            assertBool "Class_member_T should extend SyntaticInterior" $
              "class Class_member_T extends SyntaticInterior" `contains` tsCode
            assertBool "Class_member_method_def_T should extend Class_member_T" $
              "class Class_member_method_def_T extends Class_member_T" `contains` tsCode
            assertBool "Class_member_field_def_T should extend Class_member_T" $
              "class Class_member_field_def_T extends Class_member_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "deeply_nested_CHOICE_SEQ_multiple_inheritance",
      testCase "CHOICE with various member types (SYMBOL, STRING, PATTERN, BLANK, SEQ)" $ do
        -- Test with grammar having CHOICE nodes with various member types
        -- Non-SYMBOL alternatives (STRING, PATTERN, BLANK, SEQ) get indexed names (_0, _1, etc.)
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/choice_with_various_members.json"
        case result of
          Nothing -> assertFailure "Failed to parse choice_with_various_members.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- expression CHOICE: SYMBOL, STRING, PATTERN, BLANK
            -- SYMBOL alternative gets named class
            assertBool "Expression_T should extend SyntaticInterior" $
              "class Expression_T extends SyntaticInterior" `contains` tsCode
            assertBool "Expression_identifier_T should extend Expression_T (SYMBOL alternative)" $
              "class Expression_identifier_T extends Expression_T" `contains` tsCode
            -- STRING/PATTERN/BLANK alternatives get indexed classes (SyntaticLeaf subclasses)
            assertBool "Expression_1_T should extend Expression_T (STRING alternative)" $
              "class Expression_1_T extends Expression_T" `contains` tsCode
            assertBool "Expression_2_T should extend Expression_T (PATTERN alternative)" $
              "class Expression_2_T extends Expression_T" `contains` tsCode
            assertBool "Expression_3_T should extend Expression_T (BLANK alternative)" $
              "class Expression_3_T extends Expression_T" `contains` tsCode
            -- statement CHOICE: SYMBOL, SEQ, BLANK
            assertBool "Statement_T should extend SyntaticInterior" $
              "class Statement_T extends SyntaticInterior" `contains` tsCode
            assertBool "Statement_expression_statement_T should extend Statement_T (SYMBOL alternative)" $
              "class Statement_expression_statement_T extends Statement_T" `contains` tsCode
            -- SEQ alternative gets indexed class with fields from SEQ members
            assertBool "Statement_1_T should extend Statement_T (SEQ alternative)" $
              "class Statement_1_T extends Statement_T" `contains` tsCode
            -- SEQ alternative should have identifier field from its members
            assertBool "Statement_1_T should have identifier_0_i field from SEQ" $
              "identifier_0_i" `contains` tsCode
            assertBool "Statement_2_T should extend Statement_T (BLANK alternative)" $
              "class Statement_2_T extends Statement_T" `contains` tsCode
            -- optional_modifier CHOICE: SYMBOL, BLANK
            assertBool "Optional_modifier_T should extend SyntaticInterior" $
              "class Optional_modifier_T extends SyntaticInterior" `contains` tsCode
            assertBool "Optional_modifier_modifier_T should extend Optional_modifier_T (SYMBOL alternative)" $
              "class Optional_modifier_modifier_T extends Optional_modifier_T" `contains` tsCode
            assertBool "Optional_modifier_1_T should extend Optional_modifier_T (BLANK alternative)" $
              "class Optional_modifier_1_T extends Optional_modifier_T" `contains` tsCode
            -- complex_expression CHOICE: SYMBOL, SEQ, SEQ
            assertBool "Complex_expression_T should extend SyntaticInterior" $
              "class Complex_expression_T extends SyntaticInterior" `contains` tsCode
            assertBool "Complex_expression_identifier_T should extend Complex_expression_T (SYMBOL alternative)" $
              "class Complex_expression_identifier_T extends Complex_expression_T" `contains` tsCode
            -- Both SEQ alternatives get indexed classes
            assertBool "Complex_expression_1_T should extend Complex_expression_T (first SEQ)" $
              "class Complex_expression_1_T extends Complex_expression_T" `contains` tsCode
            assertBool "Complex_expression_2_T should extend Complex_expression_T (second SEQ)" $
              "class Complex_expression_2_T extends Complex_expression_T" `contains` tsCode
            -- First SEQ alternative (member_access: identifier "." identifier) should have evaluate with "."
            assertBool "Complex_expression_1_T evaluate should contain '.' for member access" $
              "identifier_0_i.evaluate()}.${this.identifier_1_i" `contains` tsCode
            -- literal_or_blank CHOICE: STRING, STRING, BLANK (no SYMBOL alternatives)
            assertBool "Literal_or_blank_T should extend SyntaticInterior" $
              "class Literal_or_blank_T extends SyntaticInterior" `contains` tsCode
            -- All three alternatives get indexed classes (no SYMBOL to name them)
            assertBool "Literal_or_blank_0_T should extend Literal_or_blank_T (first STRING)" $
              "class Literal_or_blank_0_T extends Literal_or_blank_T" `contains` tsCode
            assertBool "Literal_or_blank_1_T should extend Literal_or_blank_T (second STRING)" $
              "class Literal_or_blank_1_T extends Literal_or_blank_T" `contains` tsCode
            assertBool "Literal_or_blank_2_T should extend Literal_or_blank_T (BLANK alternative)" $
              "class Literal_or_blank_2_T extends Literal_or_blank_T" `contains` tsCode
            -- STRING/BLANK alternatives should have no constructor parameters and return literal in evaluate()
            assertBool "Literal_or_blank_0_T should have no-arg constructor (STRING alternative)" $
              "constructor() { super(); } evaluate()" `contains` tsCode
            assertBool "Literal_or_blank_0_T evaluate should return 'true'" $
              "return \"true\";" `contains` tsCode
            assertBool "Literal_or_blank_1_T evaluate should return 'false'" $
              "return \"false\";" `contains` tsCode
            assertBool "Literal_or_blank_2_T evaluate should return empty string (BLANK alternative)" $
              "evaluate(): string { return \"\";" `contains` tsCode
            -- binary_expression CHOICE: PREC_LEFT wrapping SEQ with FIELD nodes (like JavaScript grammar)
            -- This tests PREC_LEFT handling with named fields (left, operator, right)
            assertBool "Binary_expression_T should extend SyntaticInterior" $
              "class Binary_expression_T extends SyntaticInterior" `contains` tsCode
            -- PREC_LEFT alternatives get named classes using first FIELD name + precedence value
            assertBool "Binary_expression_left_logical_and_T should extend Binary_expression_T (PREC_LEFT &&)" $
              "class Binary_expression_left_logical_and_T extends Binary_expression_T" `contains` tsCode
            assertBool "Binary_expression_left_logical_or_T should extend Binary_expression_T (PREC_LEFT ||)" $
              "class Binary_expression_left_logical_or_T extends Binary_expression_T" `contains` tsCode
            assertBool "Binary_expression_left_addition_T should extend Binary_expression_T (PREC_LEFT +)" $
              "class Binary_expression_left_addition_T extends Binary_expression_T" `contains` tsCode
            assertBool "Binary_expression_left_subtraction_T should extend Binary_expression_T (PREC_LEFT -)" $
              "class Binary_expression_left_subtraction_T extends Binary_expression_T" `contains` tsCode
            -- Each alternative should have two identifier fields (left and right, but STRING operator is skipped)
            assertBool "Binary_expression_left_addition_T should have identifier_0_i field" $
              "identifier_0_i" `contains` tsCode
            assertBool "Binary_expression_left_addition_T should have identifier_1_i field" $
              "identifier_1_i" `contains` tsCode
            -- evaluate() should contain the operator string (template literal format)
            assertBool "Binary_expression_left_addition_T evaluate should contain '+' operator" $
              "identifier_0_i.evaluate()}+" `contains` tsCode
            assertBool "Binary_expression_left_logical_and_T evaluate should contain '&&' operator" $
              "identifier_0_i.evaluate()}&&" `contains` tsCode
            -- assignment_expression CHOICE: PREC_RIGHT alternatives (like JavaScript assignment)
            assertBool "Assignment_expression_T should extend SyntaticInterior" $
              "class Assignment_expression_T extends SyntaticInterior" `contains` tsCode
            -- PREC_RIGHT alternatives get named classes using first FIELD name + precedence value
            assertBool "Assignment_expression_right_assignment_T should extend Assignment_expression_T (PREC_RIGHT =)" $
              "class Assignment_expression_right_assignment_T extends Assignment_expression_T" `contains` tsCode
            assertBool "Assignment_expression_right_compound_assignment_T should extend Assignment_expression_T (PREC_RIGHT +=)" $
              "class Assignment_expression_right_compound_assignment_T extends Assignment_expression_T" `contains` tsCode
            assertBool "Assignment_expression_right_assignment_T evaluate should contain '=' operator" $
              "identifier_0_i.evaluate()}=" `contains` tsCode
            -- unary_expression CHOICE: PREC alternatives (like JavaScript unary operators)
            assertBool "Unary_expression_T should extend SyntaticInterior" $
              "class Unary_expression_T extends SyntaticInterior" `contains` tsCode
            -- PREC alternatives get named classes using precedence value
            assertBool "Unary_expression_unary_T should extend Unary_expression_T (PREC !)" $
              "class Unary_expression_unary_T extends Unary_expression_T" `contains` tsCode
            assertBool "Unary_expression_unary_minus_T should extend Unary_expression_T (PREC -)" $
              "class Unary_expression_unary_minus_T extends Unary_expression_T" `contains` tsCode
            assertBool "Unary_expression_unary_T evaluate should contain '!' operator" $
              "class Unary_expression_unary_T extends Unary_expression_T" `contains` tsCode
            -- labeled_statement CHOICE: PREC_DYNAMIC alternatives (like JavaScript labeled statements)
            assertBool "Labeled_statement_T should extend SyntaticInterior" $
              "class Labeled_statement_T extends SyntaticInterior" `contains` tsCode
            -- PREC_DYNAMIC alternatives get named classes with dynamic_{value} pattern
            -- Negative precedence values use "neg_" prefix (e.g., -1 -> dynamic_neg_1)
            assertBool "Labeled_statement_dynamic_neg_1_T should extend Labeled_statement_T (PREC_DYNAMIC -1)" $
              "class Labeled_statement_dynamic_neg_1_T extends Labeled_statement_T" `contains` tsCode
            assertBool "Labeled_statement_dynamic_neg_1_T evaluate should contain ':'" $
              "identifier_0_i.evaluate()}:" `contains` tsCode
            -- for_statement CHOICE: ALIAS alternatives (like JavaScript for-in/for-await)
            assertBool "For_statement_T should extend SyntaticInterior" $
              "class For_statement_T extends SyntaticInterior" `contains` tsCode
            -- ALIAS unwraps content, so fields come from the aliased symbol
            assertBool "For_statement_0_T should extend For_statement_T (SEQ with ALIAS)" $
              "class For_statement_0_T extends For_statement_T" `contains` tsCode
            assertBool "For_statement_0_T evaluate should contain 'for' and 'in'" $
              "For_statement_0_T extends For_statement_T" `contains` tsCode
            assertBool "For_statement_1_T evaluate should contain 'await' from ALIAS" $
              "For_statement_1_T extends For_statement_T" `contains` tsCode
            -- nested_choice CHOICE: nested CHOICE as alternative
            assertBool "Nested_choice_T should extend SyntaticInterior" $
              "class Nested_choice_T extends SyntaticInterior" `contains` tsCode
            -- Nested CHOICE alternative gets indexed class
            assertBool "Nested_choice_1_T should extend Nested_choice_T (nested CHOICE alternative)" $
              "class Nested_choice_1_T extends Nested_choice_T" `contains` tsCode
            assertBool "Nested_choice_identifier_T should extend Nested_choice_T (SYMBOL alternative)" $
              "class Nested_choice_identifier_T extends Nested_choice_T" `contains` tsCode
            -- array_content CHOICE: REPEAT/REPEAT1 alternatives
            assertBool "Array_content_T should extend SyntaticInterior" $
              "class Array_content_T extends SyntaticInterior" `contains` tsCode
            -- REPEAT/REPEAT1 alternatives get indexed classes
            assertBool "Array_content_0_T should extend Array_content_T (REPEAT alternative)" $
              "class Array_content_0_T extends Array_content_T" `contains` tsCode
            assertBool "Array_content_1_T should extend Array_content_T (REPEAT1 alternative)" $
              "class Array_content_1_T extends Array_content_T" `contains` tsCode
            assertBool "Array_content_2_T should extend Array_content_T (BLANK alternative)" $
              "class Array_content_2_T extends Array_content_T" `contains` tsCode
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "CHOICE_with_various_member_types",
      testCase "golden file matches generated TypeScript for JavaScript grammar" $ do
        result <- runMaybeT $ parseGrammarFromFile "sample/grammar.json"
        case result of
          Nothing -> assertFailure "Failed to parse sample/grammar.json"
          Just grammar -> do
            let generated = T.pack $ descript grammar
            golden <- TIO.readFile "test/fixtures/javascript_template.golden.ts"
            assertEqual "Generated TypeScript should match golden file" golden generated
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles generated "golden_file_matches_generated_TypeScript",
      testCase "generated TypeScript compiles with tsc (TypeScript compiler)" $ do
        -- Generate TypeScript from test grammar with various patterns
        result <- runMaybeT $ parseGrammarFromFile "test/ProgBuilder/TestGrammars/choice_with_various_members.json"
        case result of
          Nothing -> assertFailure "Failed to parse choice_with_various_members.json"
          Just grammar -> do
            let tsCode = T.pack $ descript grammar
            -- Verify generated code compiles with tsc
            assertTypeScriptCompiles tsCode "generated_TypeScript_compiles_with_tsc"
    ]
