{-# LANGUAGE OverloadedStrings #-}
module BackendDescription.NodeDescription (descript, node_type_ident) where

import qualified TreeSitterNodes as TN
import qualified Template.Template as TT
import qualified Template.TypeScriptTemplate as TTS
import Data.Text.Lazy (pack, Text, unpack)
import Data.Maybe (isNothing,fromJust)
import qualified Data.Map as Map
import qualified Data.List as List
import BackendDescription.NodeDescriptionHelper

descript :: [TN.Node] -> String
descript nodes =
  let node_desc = foldl' (\s n -> s ++ " " ++ node_proc n) "" nodes
  in prologue ++ node_desc
  where
    prologue :: String
    prologue =
      (unpack $
        TT.inst TTS.import_statement
          (TT.TArray ["strict as assert"]) "assert")
      ++
      (unpack $
        TT.inst TTS.import_statement
          (TT.TArray ["Node"]) "web-tree-sitter")
      ++
      (unpack $
        TT.inst TTS.import_statement
          (TT.TArray ["Searcher"])
          "../../parser/ast_helper")
      ++
      "\n\
      \export class TS_Node { \n\
        \private node_: Node; \n\
        \private start_index_: number; \n\
        \private end_index_: number;\n\
        \\n\
        \constructor(node: Node) {\n\
        \    this.node_ = node;\n\
        \    this.start_index_ = node.startIndex;\n\
        \    this.end_index_ = node.endIndex;\n\
        \}\n\
        \\n\
        \public getText() {\n\
        \    return this.node_.text;\n\
        \}\n\
        \public get start_index(): number {\n\
        \    return this.start_index_;\n\
        \}\n\
        \public get end_index(): number {\n\
        \    return this.end_index_;\n\
        \}\n\
        \\n\
        \public get type(): string {\n\
        \    return this.node_.type;\n\
        \}\n\
      \}"

    node_proc :: TN.Node -> String
    node_proc node@(TN.Leaf n_info) =
      if TN.named n_info
      then unpack $ leaf_node node
      else ""
    node_proc node = unpack $ interior_node node

    leaf_node :: TN.Node -> Text
    leaf_node (TN.Leaf n_info) = TT.inst TTS.export_qualifier $
      (TT.inst TTS.class_declare)
        (pack $ node_type_ident $ TN.node_type n_info)     -- Class Identifier
        (Just "TS_Node")                                        -- Base Class
        (Just (TT.TArray $ prop_declarations (TN.Leaf n_info))) -- Properties
        (Just (TT.TArray $ constructor (TN.Leaf n_info) : []))  -- Constructor
    leaf_node _ = undefined

    interior_node :: TN.Node -> Text
    -- Leaf node is impossible here
    interior_node node@(TN.Interior n_info _ Nothing _) =
      TT.inst TTS.export_qualifier $
      TT.inst TTS.class_declare
        (pack $ node_type_ident $ TN.node_type n_info)
        (Just "TS_Node")
        (Just (TT.TArray $ prop_declarations node))
        (Just (TT.TArray $
              -- Constructor
              constructor node :
              -- Extra Methods
              []))

    interior_node node@(TN.Interior n_info _ (Just _) _) =
      TT.inst TTS.export_qualifier $
      TT.inst TTS.class_declare
        (pack $ node_type_ident $ TN.node_type n_info)
        (Just "TS_Node")
        (Just (TT.TArray $ prop_declarations node))
        (Just (TT.TArray $
               -- Constructor
               constructor node :
               -- Extra Methods
               []))
    interior_node _ = undefined

    constructor :: TN.Node -> Text
    constructor node@(TN.Leaf _) =
      (TT.inst TTS.const_declare)
      -- Parameters
      (TT.TArray [TT.inst TTS.parameter_declare "node" "Node"])
      -- Body Statements
      (TT.TArray $ prologue_proc node)
    -- There should be only child node of a node wtih subtypes fields is non-Nothing
    constructor node@(TN.Interior _ Nothing (Just subtypes) Nothing) =
      (TT.inst TTS.const_declare)
      (TT.TArray [TT.inst TTS.parameter_declare "node" "Node"])
      (TT.TArray $
            prologue_proc node ++
            -- Prop initializations
            map prop_initializer_from_node subtypes)

    constructor node@(TN.Interior _ _ Nothing _) =
      (TT.inst TTS.const_declare)
      (TT.TArray [TT.inst TTS.parameter_declare "node" "Node"])
      (TT.TArray $
            prologue_proc node ++
            -- Field Prop Initializations
            field_prop_init_stmts node ++
            -- Children Prop Initializations
            child_node_init_stmts node)

    constructor _ = undefined

    child_node_init_stmts :: TN.Node -> [Text]
    child_node_init_stmts (TN.Leaf _) = undefined
    child_node_init_stmts (TN.Interior _ _ Nothing Nothing) = []
    child_node_init_stmts (TN.Interior _ _ Nothing (Just (TN.Children _ m ts))) =
      if m
      then map array_prop_initializer_from_node ts
      else map prop_initializer_from_node ts
    child_node_init_stmts (TN.Interior _ _ (Just ts) Nothing) =
      map prop_initializer_from_node ts
    child_node_init_stmts _ = undefined

    prop_declarations :: TN.Node -> [Text]
    prop_declarations (TN.Leaf _) = []
    prop_declarations (TN.Interior _ Nothing (Just n_infos) Nothing) =
      map (type_declare False) n_infos
    prop_declarations (TN.Interior _ fields Nothing children) =
      let field_declares = if isNothing fields
                            then []
                            else field_prop_declare (fromJust fields)
          child_declares = if isNothing children
                            then []
                            else child_prop_declare (fromJust children)
      in field_declares ++ child_declares
      where
        field_prop_declare :: Map.Map String TN.Children -> [Text]
        field_prop_declare field_map =
          Map.foldrWithKey g [] (flip Map.filter field_map $
                                 \x -> length (TN.types x) > 0 &&
                                       (flip (flip foldl' False) (TN.types x) $ \a y -> (TN.named y) || a) == True)
          where
            g :: String -> TN.Children -> [Text] -> [Text]
            g k v a = a ++ [
              TT.inst TTS.public_qualifier $ TT.inst TTS.parameter_declare (pack $ node_name_ident k) $ field_type v]

            field_type :: TN.Children -> Text
            -- Which is impossible
            field_type (TN.Children _ True ts)  =
              let r = flip (flip foldl' "") ts $
                    \acc n_info -> acc ++ (field_type' n_info) ++ " | "
                  w = words r
              in  pack $ "(" ++ (unwords $ take (length w - 1) $ w) ++ ")[] = [];"

            field_type (TN.Children _ False ts) =
              let r = flip (flip foldl' "") ts $
                    \acc n_info -> acc ++ (field_type' n_info) ++ " | "
              in  pack $ r ++ "undefined;"

            field_type' :: TN.NodeInfo -> String
            field_type' (TN.NodeInfo nt True) = node_type_ident nt
            field_type' (TN.NodeInfo _  False) = ""

        child_prop_declare :: TN.Children -> [Text]
        child_prop_declare (TN.Children _ m ts) =
          map (type_declare m) ts
    prop_declarations _ = undefined

    type_declare :: Bool -> TN.NodeInfo -> Text
    type_declare is_multipled (TN.NodeInfo nt named) =
      if named
      then TT.inst TTS.public_qualifier $
            TT.inst TTS.parameter_declare
            (pack $ node_name_ident nt)
            (pack $
              if is_multipled
              then node_type_ident nt ++ "[] | undefined;"
              else node_type_ident nt ++ " | undefined;")
        -- Currently, non-named nodes is useless just ignore it
      else ""

    prop_initializer_from_node :: TN.NodeInfo -> Text
    prop_initializer_from_node (TN.NodeInfo _ False) = pack $ ""
    prop_initializer_from_node n_info'@(TN.NodeInfo _ True) =
      TTS.prop_initialize node_type' prop_ident t_ident
      where
        prop_ident = pack $ node_name_ident $ TN.node_type n_info'
        node_type' = pack $ TN.node_type n_info'
        t_ident = pack $ node_type_ident (unpack node_type')

    array_prop_initializer_from_node :: TN.NodeInfo -> Text
    array_prop_initializer_from_node n_info' =
      let node_type' = pack $ TN.node_type n_info'
      in TTS.prop_initialize_array
         (pack $ node_name_ident $ unpack node_type')
         node_type'
         (pack $ node_type_ident $ unpack node_type')

    field_prop_init_stmts :: TN.Node -> [Text]
    field_prop_init_stmts (TN.Leaf _) = []
    field_prop_init_stmts (TN.Interior _ Nothing _ _) = []
    field_prop_init_stmts (TN.Interior _ (Just fields) _ _) =
      Map.foldrWithKey g [] fields
      where
        g :: String -> TN.Children -> [Text] -> [Text]
        g k (TN.Children _ False ts) a =
          a ++ [TTS.field_initialize
                (pack k)
                (map (pack . TN.node_type) (named_only_ts ts))]

        g k (TN.Children _ True  ts) a =
          a ++ [TTS.array_field_initialize
                (pack k)
                (map (pack . TN.node_type) (named_only_ts ts))]

        named_only_ts :: [TN.NodeInfo] -> [TN.NodeInfo]
        named_only_ts ts = flip filter ts $ \n_info -> (TN.named n_info == True)

    prologue_proc :: TN.Node -> [Text]
    prologue_proc node =
      [TT.inst TTS.function_call
       "super"
       (Just $ TT.TArray [TT.inst TTS.var_ref "node"]),
       TT.inst TTS.node_type_assertion $ pack $ TN.node_type $ TN.info node]
