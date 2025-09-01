{-# LANGUAGE OverloadedStrings, LambdaCase, MultilineStrings #-}
module BackendDescription.NodeDescription (descript, node_type_ident) where

import qualified TreeSitterNodes as Ns
import qualified Template.Template as T_T
import qualified Template.TypeScriptTemplate as T_TS
import Data.Text.Lazy (pack, Text, unpack)
import qualified Data.Map as Map
import BackendDescription.NodeDescriptionHelper

descript :: [Ns.Node] -> String
descript nodes =
  let node_desc = foldl' (\s n -> s ++ " " ++ node_proc n) "" nodes
  in prologue ++ node_desc ++ unpack (factory_function_declare nodes)
  where
    supertypes :: [String]
    supertypes =
      flip map (flip filter nodes $ \case {
                   Ns.Leaf _ -> False;
                     Ns.Interior _ _ Nothing __ -> False;
                     Ns.Interior _ _ (Just _) __ -> True;})
      $ \case {
        -- Which should impossible
        Ns.Leaf _ -> undefined;
        Ns.Interior _ _ Nothing _ -> undefined;
        Ns.Interior (Ns.NodeInfo nt _) _ (Just _) _ -> nt;
      }

    prologue :: String
    prologue = unpack
      (T_T.inst T_TS.import_statement
          (T_T.TArray ["strict as assert"]) "assert")
      ++
      unpack
        (T_T.inst T_TS.import_statement
          (T_T.TArray ["Node"]) "web-tree-sitter")
      ++
      unpack
        (T_T.inst T_TS.import_statement
          (T_T.TArray ["Searcher"])
          "../../parser/ast_helper")
      ++
      """
      export type TSNodeType = String;
      export class TS_Node {
        private node_: Node;
        private start_index_: number;
        private end_index_: number;
        public is_supertype_setup = false;

        constructor(node: Node) {
            this.node_ = node;
            this.start_index_ = node.startIndex;
            this.end_index_ = node.endIndex;
        }
        public getNode() {
          return this.node_;
        }
        public getText() {
            return this.node_.text;
        }
        public get start_index(): number {
            return this.start_index_;
        }
        public get end_index(): number {
            return this.end_index_;
        }

        public get type(): string {
            return this.node_.type;
        }
      }
      """

    node_proc :: Ns.Node -> String
    node_proc node@(Ns.Leaf n_info) =
      if Ns.named n_info
      then unpack $ leaf_node node
      else ""
    node_proc node = unpack $ interior_node node

    leaf_node :: Ns.Node -> Text
    leaf_node (Ns.Leaf n_info) = T_T.inst T_TS.export_qualifier $
      T_T.inst T_TS.class_declare
        (pack $ node_type_ident $ Ns.node_type n_info)     -- Class Identifier
        (Just "TS_Node")                                        -- Base Class
        (Just (T_T.TArray $
               pack ("public static node_type = \"" ++ Ns.node_type n_info ++ "\";") :
               prop_declarations (Ns.Leaf n_info))) -- Properties
        (Just (T_T.TArray [constructor (Ns.Leaf n_info)]))  -- Constructor
    leaf_node _ = undefined

    interior_node :: Ns.Node -> Text
    -- Leaf node is impossible here
    interior_node node@(Ns.Interior n_info _ Nothing _) =
      T_T.inst T_TS.export_qualifier $
      T_T.inst T_TS.class_declare
        (pack $ node_type_ident $ Ns.node_type n_info)
        (Just "TS_Node")
        (Just (T_T.TArray $
               pack ("public static node_type = \"" ++ Ns.node_type n_info ++ "\";") :
               prop_declarations node))
        (Just (T_T.TArray [constructor node]))

    interior_node node@(Ns.Interior n_info _ (Just _) _) =
      T_T.inst T_TS.export_qualifier $
      T_T.inst T_TS.class_declare
        (pack $ node_type_ident $ Ns.node_type n_info)
        (Just "TS_Node")
        (Just (T_T.TArray $
               pack ("public static node_type = \"" ++ Ns.node_type n_info ++ "\";") :
               prop_declarations node))
        (Just (T_T.TArray [constructor node]))
    interior_node _ = undefined

    constructor :: Ns.Node -> Text
    constructor node@(Ns.Leaf _) =
      T_T.inst T_TS.const_declare
      -- Parameters
      (T_T.TArray [T_T.inst T_TS.parameter_declare "node" "Node"])
      -- Body Statements
      (T_T.TArray $ prologue_proc node)
    -- There should be only child node of a node wtih subtypes fields is non-Nothing
    constructor node@(Ns.Interior _ Nothing (Just subtypes) Nothing) =
      T_T.inst T_TS.const_declare
      (T_T.TArray [T_T.inst T_TS.parameter_declare "node" "Node"])
      (T_T.TArray $
            subtype_prologue_proc node ++
            -- Prop initializations
            map subtype_prop_initializer_from_node subtypes)

    constructor node@(Ns.Interior _ _ Nothing _) =
      T_T.inst T_TS.const_declare
      (T_T.TArray [T_T.inst T_TS.parameter_declare "node" "Node"])
      (T_T.TArray $
            prologue_proc node ++
            -- Field Prop Initializations
            field_prop_init_stmts node ++
            -- Children Prop Initializations
            child_node_init_stmts node)

    constructor _ = undefined

    child_node_init_stmts :: Ns.Node -> [Text]
    child_node_init_stmts (Ns.Leaf _) = undefined
    child_node_init_stmts (Ns.Interior _ _ Nothing Nothing) = []
    child_node_init_stmts (Ns.Interior _ _ Nothing (Just (Ns.Children _ m ts))) =
      if m
      then map array_prop_initializer_from_node ts
      else map prop_initializer_from_node ts
    child_node_init_stmts (Ns.Interior _ _ (Just ts) Nothing) =
      map prop_initializer_from_node ts
    child_node_init_stmts _ = undefined

    prop_declarations :: Ns.Node -> [Text]
    prop_declarations (Ns.Leaf _) = []
    prop_declarations (Ns.Interior _ Nothing (Just n_infos) Nothing) =
      map (type_declare False) n_infos
    prop_declarations (Ns.Interior _ fields Nothing children) =
      let field_declares = maybe [] field_prop_declare fields
          child_declares = maybe [] child_prop_declare children
      in field_declares ++ child_declares
      where
        field_prop_declare :: Map.Map String Ns.Children -> [Text]
        field_prop_declare field_map =
          Map.foldrWithKey g [] (flip Map.filter field_map $
                                 \x -> not (null (Ns.types x)) &&
                                       foldl' (\a y -> Ns.named y || a) False (Ns.types x))
          where
            g :: String -> Ns.Children -> [Text] -> [Text]
            g k v a = a ++ [
              T_T.inst T_TS.public_qualifier $ T_T.inst T_TS.parameter_declare (pack $ node_name_ident k) $ field_type v]

            field_type :: Ns.Children -> Text
            -- Which is impossible
            field_type (Ns.Children _ True ts)  =
              let r = flip (`foldl'` "") ts $
                    \acc n_info -> acc ++ field_type' n_info ++ " | "
                  w = words r
              in  pack $ "(" ++ unwords (take (length w - 1) w) ++ ")[] = [];"

            field_type (Ns.Children _ False ts) =
              let r = flip (`foldl'` "") ts $
                    \acc n_info -> acc ++ field_type' n_info ++ " | "
              in  pack $ r ++ "undefined;"

            field_type' :: Ns.NodeInfo -> String
            field_type' (Ns.NodeInfo nt True) = node_type_ident nt
            field_type' (Ns.NodeInfo _  False) = ""

        child_prop_declare :: Ns.Children -> [Text]
        child_prop_declare (Ns.Children _ m ts) =
          map (type_declare m) ts
    prop_declarations _ = undefined

    type_declare :: Bool -> Ns.NodeInfo -> Text
    type_declare is_multipled (Ns.NodeInfo nt named) =
      if named
      then T_T.inst T_TS.public_qualifier $
            T_T.inst T_TS.parameter_declare
            (pack $ node_name_ident nt)
            (pack $
              if is_multipled
              then node_type_ident nt ++ "[] | undefined;"
              else node_type_ident nt ++ " | undefined;")
        -- Currently, non-named nodes is useless just ignore it
      else ""

    subtype_prop_initializer_from_node :: Ns.NodeInfo -> Text
    subtype_prop_initializer_from_node (Ns.NodeInfo _ False) = pack ""
    subtype_prop_initializer_from_node (Ns.NodeInfo t True) =
      T_TS.subtype_prop_initialize (map pack supertypes) $ pack t

    prop_initializer_from_node :: Ns.NodeInfo -> Text
    prop_initializer_from_node (Ns.NodeInfo _ False) = pack ""
    prop_initializer_from_node n_info'@(Ns.NodeInfo _ True) =
      T_TS.prop_initialize (map pack supertypes) node_type' prop_ident t_ident
      where
        prop_ident = pack $ node_name_ident $ Ns.node_type n_info'
        node_type' = pack $ Ns.node_type n_info'
        t_ident = pack $ node_type_ident (unpack node_type')

    array_prop_initializer_from_node :: Ns.NodeInfo -> Text
    array_prop_initializer_from_node n_info' =
      let node_type' = pack $ Ns.node_type n_info'
      in T_TS.prop_initialize_array
         (map pack supertypes)
         (pack $ node_name_ident $ unpack node_type')
         node_type'
         (pack $ node_type_ident $ unpack node_type')

    field_prop_init_stmts :: Ns.Node -> [Text]
    field_prop_init_stmts (Ns.Leaf _) = []
    field_prop_init_stmts (Ns.Interior _ Nothing _ _) = []
    field_prop_init_stmts (Ns.Interior _ (Just fields) _ _) =
      Map.foldrWithKey g [] fields
      where
        g :: String -> Ns.Children -> [Text] -> [Text]
        g k (Ns.Children _ False ts) a =
          a ++ [T_TS.field_initialize
                (map pack supertypes)
                (pack k)
                (map (pack . Ns.node_type) (named_only_ts ts))]

        g k (Ns.Children _ True  ts) a =
          a ++ [T_TS.array_field_initialize
                (map pack supertypes)
                (pack k)
                (map (pack . Ns.node_type) (named_only_ts ts))]

        named_only_ts :: [Ns.NodeInfo] -> [Ns.NodeInfo]
        named_only_ts ts = flip filter ts $ \n_info -> Ns.named n_info

    subtype_prologue_proc :: Ns.Node -> [Text]
    subtype_prologue_proc _ =
      [T_T.inst T_TS.function_call
       "super"
       (Just $ T_T.TArray [T_T.inst T_TS.var_ref "node"])]

    prologue_proc :: Ns.Node -> [Text]
    prologue_proc node =
      [T_T.inst T_TS.function_call
       "super"
       (Just $ T_T.TArray [T_T.inst T_TS.var_ref "node"]),
       T_T.inst T_TS.node_type_assertion $ pack $ Ns.node_type $ Ns.info node]

    factory_function_declare :: [Ns.Node] -> Text
    factory_function_declare nodes'' =
      T_T.inst T_TS.export_qualifier $
      T_T.inst T_TS.function_declare "tsNodeFactory"
      (T_T.TArray [ "node: Node" ])
      (Just "TS_Node")
      (T_T.TArray [
            T_TS.switch_statements "node.type" case_expressions,
            "throw new Error(\"Unsupported node type:\" + node.type);"
          ])
      where
        case_expressions :: T_T.TArray Text
        case_expressions = T_T.TArray $ map case_expressions' nodes''

        case_expressions' :: Ns.Node -> Text
        case_expressions' (Ns.Leaf (Ns.NodeInfo _ False)) = ""
        case_expressions' (Ns.Leaf (Ns.NodeInfo t _)) =
          T_TS.case_expression
            (pack t)
            (T_TS.node_build_template $ pack t)
        case_expressions' (Ns.Interior (Ns.NodeInfo _ False) _ _ _) = ""
        case_expressions' (Ns.Interior (Ns.NodeInfo t _) _ _ _) =
          T_TS.case_expression
          (pack t)
          (T_TS.node_build_template $ pack t)
