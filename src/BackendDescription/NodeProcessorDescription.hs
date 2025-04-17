{-# LANGUAGE OverloadedStrings #-}
module BackendDescription.NodeProcessorDescription where

import qualified Template.Template as TT
import qualified Template.TypeScriptTemplate as TTS
import qualified TreeSitterNodes as TN
import Data.Text.Lazy (unpack, pack, Text)
import Utility (upper_the_first_char)
import Template.TypeScriptTemplate (switch_statements)

descript :: [TN.Node] -> String
descript nodes =
  let processor_desc = node_proc $ named_nodes nodes
  in prologue ++ " " ++ processor_desc
  where
    named_nodes :: [TN.Node] -> [TN.Node]
    named_nodes [] = []
    named_nodes ((TN.Leaf (TN.NodeInfo _ False)):xs) =  named_nodes xs
    named_nodes (n@(TN.Leaf (TN.NodeInfo _ True)):xs) = n : named_nodes xs
    named_nodes (x:xs) = x : named_nodes xs

    prologue :: String
    prologue =
      (unpack $ TT.inst TTS.import_statement
        (TT.TArray ["Show"]) "../source")
      ++
      (unpack $ TT.inst TTS.import_statement
        (TT.TArray (map  named_nodes nodes)) ./node_declare")
      ++
      ("export type OutputTarget = string;")

    -- Assume that all nodes are named
    node_proc :: [TN.Node] -> String
    node_proc nodes' = build_node_processor nodes'

    build_node_processor :: [TN.Node] -> String
    build_node_processor nodes' =
      unpack $
        (TT.inst TTS.class_declare)
        (pack "NodeProcessor<T extends Show>")
        Nothing
        (Just (TT.TArray $ basic_prop ++
                           (map processor_prop_declare nodes')))
        (Just (TT.TArray [trans_method_declare nodes']))
      where
        basic_prop :: [Text]
        basic_prop = [
          "prologue? : ((Ns: TS_Node) => [OutputTarget, T][]) | undefined;",
          "epilogue? : ((Ns: TS_Node) => [OutputTarget, T][]) | undefined;",
          "post? : ((output_content: T) => T) | undefined;"
          ]

        processor_prop_declare :: TN.Node -> Text
        processor_prop_declare (TN.Leaf (TN.NodeInfo t _)) =
          TT.inst TTS.prop_declare
                (pack $ t ++ "_proc?")
                (prop_type_declare t)
        processor_prop_declare (TN.Interior (TN.NodeInfo t _) _ _ _) =
          TT.inst TTS.prop_declare
                (pack $ t ++ "_proc?")
                (prop_type_declare t)
        prop_type_declare :: String -> Text
        prop_type_declare t =
          pack $ "((N: " ++ (upper_the_first_char t) ++ ") => [OutputTarget, T][]) | undefined"

        trans_method_declare :: [TN.Node] -> Text
        trans_method_declare nodes'' =
          TT.inst TTS.method_declare "proc"
                (TT.TArray [ "node:TS_Node"])
                (Just "[OutputTarget, T][] | null")
                (TT.TArray [
                    -- Statements
                    TT.inst TTS.variable_decl "results" "null",
                    switch_statements "node.type" case_expressions,
                    "return results"
                           ])
          where
            case_expressions :: TT.TArray Text
            case_expressions = TT.TArray $ map case_expressions' nodes''

            case_expressions' :: TN.Node -> Text
            case_expressions' (TN.Leaf (TN.NodeInfo t _)) =
              TTS.case_expression (pack t) (TTS.node_processor_proc_template (pack t))
            case_expressions' (TN.Interior (TN.NodeInfo t _) _ _ _) =
              TTS.case_expression (pack t) (TTS.node_processor_proc_template (pack t))
