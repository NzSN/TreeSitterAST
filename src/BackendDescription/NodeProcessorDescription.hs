{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module BackendDescription.NodeProcessorDescription where

import qualified Template.Template as TT
import qualified Template.TypeScriptTemplate as TTS
import qualified TreeSitterNodes as TN
import Data.Text.Lazy (unpack, pack, Text)
import Template.TypeScriptTemplate (switch_statements)
import qualified BackendDescription.NodeDescription as BN
import qualified BackendDescription.NodeDescriptionHelper as BNH

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
        (TT.TArray $
         "TS_Node" :
          (flip map (named_nodes nodes) $
            \case { (TN.Leaf (TN.NodeInfo nt _)) -> pack $ BNH.node_type_ident nt;
                    (TN.Interior (TN.NodeInfo nt _) _ _ _) -> pack $ BNH.node_type_ident nt }))
        "./node_declare")
      ++
      ("export type OutputTarget = string;")

    -- Assume that all nodes are named
    node_proc :: [TN.Node] -> String
    node_proc nodes' = build_node_processor nodes'

    build_node_processor :: [TN.Node] -> String
    build_node_processor nodes' =
        (unpack $
          TT.inst TTS.export_qualifier $
          (TT.inst TTS.interface_declare)
          (pack "NodeProcessor<T extends Show>")
          Nothing
          (Just (TT.TArray $ basic_prop ++
                           (map processor_prop_declare nodes')))
          (Just (TT.TArray [])))
        ++
        (unpack $ trans_function_declare nodes')

      where
        basic_prop :: [Text]
        basic_prop = [
          "prologue? : ((Ns: TS_Node[]) => [OutputTarget, T][]) | undefined;",
          "epilogue? : ((Ns: TS_Node[]) => [OutputTarget, T][]) | undefined;",
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
          pack $ "((N: " ++ (BN.node_type_ident t) ++ ") => [OutputTarget, T][]) | undefined"

        trans_function_declare :: [TN.Node] -> Text
        trans_function_declare nodes'' =
          TT.inst TTS.export_qualifier $
          TT.inst TTS.function_declare "proc<T extends Show>"
                (TT.TArray [ "processor: NodeProcessor<T>", "node:TS_Node"])
                (Just "[OutputTarget, T][] | null")
                (TT.TArray [
                    -- Statements
                    TT.inst TTS.variable_decl "results : [OutputTarget, T][] | null" "null",
                    switch_statements "node.type" case_expressions,
                    "return results"
                           ])
          where
            case_expressions :: TT.TArray Text
            case_expressions = TT.TArray $ map case_expressions' nodes''

            case_expressions' :: TN.Node -> Text
            case_expressions' (TN.Leaf (TN.NodeInfo t _)) =
              TTS.case_expression
                (pack t)
                (TTS.node_processor_proc_template (proc_ident t) (proc_ident t) (pack $ BN.node_type_ident t))
            case_expressions' (TN.Interior (TN.NodeInfo t _) _ _ _) =
              TTS.case_expression
                (pack t)
                (TTS.node_processor_proc_template (proc_ident t) (proc_ident t) (pack $ BN.node_type_ident t))

            proc_ident t = pack $ t ++ "_proc"
