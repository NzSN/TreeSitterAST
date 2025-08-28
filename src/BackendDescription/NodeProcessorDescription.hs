{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module BackendDescription.NodeProcessorDescription where

import qualified Template.Template as T_T
import qualified Template.TypeScriptTemplate as T_TS
import qualified TreeSitterNodes as Ns
import Data.Text.Lazy (unpack, pack, Text)
import Template.TypeScriptTemplate (switch_statements)
import qualified BackendDescription.NodeDescription as BN
import qualified BackendDescription.NodeDescriptionHelper as BNH

descript :: [Ns.Node] -> String
descript nodes =
  let processor_desc = node_proc $ named_nodes nodes
  in prologue ++ " " ++ processor_desc
  where
    named_nodes :: [Ns.Node] -> [Ns.Node]
    named_nodes [] = []
    named_nodes ((Ns.Leaf (Ns.NodeInfo _ False)):xs) =  named_nodes xs
    named_nodes (n@(Ns.Leaf (Ns.NodeInfo _ True)):xs) = n : named_nodes xs
    named_nodes (x:xs) = x : named_nodes xs

    prologue :: String
    prologue =
      (unpack $ T_T.inst T_TS.import_statement
        (T_T.TArray ["Show"]) "../source")
      ++
      (unpack $ T_T.inst T_TS.import_statement
        (T_T.TArray $
         "TS_Node" :
          (flip map (named_nodes nodes) $
            \case { Ns.Leaf (Ns.NodeInfo nt _) -> pack $ BNH.node_type_ident nt;
                    Ns.Interior (Ns.NodeInfo nt _) _ _ _ -> pack $ BNH.node_type_ident nt }))
        "./node_declare")
      ++
      ("export type OutputTarget = string;")

    -- Assume that all nodes are named
    node_proc :: [Ns.Node] -> String
    node_proc nodes' = build_node_processor nodes'

    build_node_processor :: [Ns.Node] -> String
    build_node_processor nodes' =
        (unpack $
          T_T.inst T_TS.export_qualifier $
          (T_T.inst T_TS.interface_declare)
          (pack "NodeProcessor<T extends Show>")
          Nothing
          (Just (T_T.TArray $ basic_prop ++
                           (map processor_prop_declare nodes')))
          (Just (T_T.TArray [])))
        ++
        (unpack $ trans_function_declare nodes')

      where
        basic_prop :: [Text]
        basic_prop = [
          "prologue? : ((Ns: TS_Node[]) => [OutputTarget, T][]) | undefined;",
          "epilogue? : ((Ns: TS_Node[]) => [OutputTarget, T][]) | undefined;",
          "post? : ((output_content: T) => T) | undefined;"
          ]

        processor_prop_declare :: Ns.Node -> Text
        processor_prop_declare (Ns.Leaf (Ns.NodeInfo t _)) =
          T_T.inst T_TS.prop_declare
                (pack $ t ++ "_proc?")
                (prop_type_declare t)
        processor_prop_declare (Ns.Interior (Ns.NodeInfo t _) _ _ _) =
          T_T.inst T_TS.prop_declare
                (pack $ t ++ "_proc?")
                (prop_type_declare t)
        prop_type_declare :: String -> Text
        prop_type_declare t =
          pack $ "((N: " ++ (BN.node_type_ident t) ++ ") => [OutputTarget, T][]) | undefined"

        trans_function_declare :: [Ns.Node] -> Text
        trans_function_declare nodes'' =
          T_T.inst T_TS.export_qualifier $
          T_T.inst T_TS.function_declare "proc<T extends Show>"
                (T_T.TArray [ "processor: NodeProcessor<T>", "node:TS_Node"])
                (Just "[OutputTarget, T][] | null")
                (T_T.TArray [
                    -- Statements
                    T_T.inst T_TS.variable_decl "results : [OutputTarget, T][] | null" "null",
                    switch_statements "node.type" case_expressions,
                    "return results"
                           ])
          where
            case_expressions :: T_T.TArray Text
            case_expressions = T_T.TArray $ map case_expressions' nodes''

            case_expressions' :: Ns.Node -> Text
            case_expressions' (Ns.Leaf (Ns.NodeInfo t _)) =
              T_TS.case_expression
                (pack t)
                (T_TS.node_processor_proc_template (proc_ident t) (proc_ident t) (pack $ BN.node_type_ident t))
            case_expressions' (Ns.Interior (Ns.NodeInfo t _) _ _ _) =
              T_TS.case_expression
                (pack t)
                (T_TS.node_processor_proc_template (proc_ident t) (proc_ident t) (pack $ BN.node_type_ident t))

            proc_ident t = pack $ t ++ "_proc"
