{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ProgBuilderDescription (descript, ruleToClass) where

import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Map.Strict qualified as M
import Data.Text.Lazy (Text, pack, unpack)
import Template.Template qualified as TT
import Template.TypeScriptTemplate qualified as TTS
import TreeSitterGrammarNodes qualified as TSGN
import TypedASTGenerator.NodeDescriptionHelper

descript :: TSGN.Grammar -> String
descript grammar =
  let rules = TSGN.grammarRules grammar
      classDefs = Map.foldrWithKey (\name rule acc -> acc ++ ruleToClass name rule) "" rules
   in imports ++ prologue ++ classDefs

imports :: String
imports =
  unpack
    (TT.inst TTS.import_statement
      (TT.TArray ["strict as assert"]) "assert")
  ++
  unpack
    (TT.inst TTS.import_statement
      (TT.TArray ["Node"]) "web-tree-sitter")
  ++
  unpack
    (TT.inst TTS.import_statement
      (TT.TArray ["Searcher"])
      "../../parser/ast_helper")

prologue :: String
prologue =
  """
  enum NodeType {
      Leaf,
      Interior
  }
  export class SyntaticNode {
      type_: NodeType;
      constructor(n_type: NodeType) { this.type_ = n_type; }
      evaluate(): string {
          throw Error("Interior or Leaf should implement evaluate().");
      }
  }

  export class SyntaticLeaf extends SyntaticNode {
      value_: string;
      constructor(value: string) {
          super(NodeType.Leaf)
          this.value_ = value;
      }
      evaluate(): string {
          return this.value_;
      }
  }

  export class SyntaticInterior extends SyntaticNode {
      evaluate(): string {
          throw Error("Instance of Interior should implment evaluate()");
      }
  }
  """

ruleToClass :: String -> TSGN.Rule -> String
ruleToClass name rule =
  let className = pack $ node_type_ident name
      fields = mergeFields $ extractFields rule
      isLeaf = isTokenRule rule
      (baseClass, constructorDef, props) = if isLeaf
        then ( "SyntaticLeaf"
             , leafConstructor
             , Nothing
             )
        else ( "SyntaticInterior"
             , interiorConstructor fields
             , Just $ TT.TArray $ map fieldToPropDecl fields
             )
      methods = Just $ TT.TArray [constructorDef]
   in unpack $
        TT.inst TTS.export_qualifier $
          TT.inst TTS.class_declare className (Just baseClass) props methods
  where
    leafConstructor :: Text
    leafConstructor =
      TT.inst TTS.const_declare
        (TT.TArray [TT.inst TTS.parameter_declare "value" "string"])
        (TT.TArray [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "value"])])

    interiorConstructor :: [(String, TSGN.Rule, Bool)] -> Text
    interiorConstructor fields' =
      TT.inst TTS.const_declare
        (TT.TArray [TT.inst TTS.parameter_declare "node" "Node"])
        (TT.TArray $ interiorPrologueStmts ++ map fieldToConstructorStmt fields')

    interiorPrologueStmts :: [Text]
    interiorPrologueStmts =
      [ TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "NodeType.Interior"]),
        TT.inst TTS.node_type_assertion $ pack $ name
      ]

extractFields :: TSGN.Rule -> [(String, TSGN.Rule, Bool)] -- (fieldName, content, isArray)
extractFields (TSGN.Seq members) = concatMap extractFields members
extractFields (TSGN.Choice members) = concatMap extractFields members
extractFields (TSGN.Repeat content) = map (\(n, c, _) -> (n, c, True)) $ extractFields content
extractFields (TSGN.Repeat1 content) = map (\(n, c, _) -> (n, c, True)) $ extractFields content
extractFields (TSGN.Field fieldName content) = [(fieldName, content, False)]
extractFields (TSGN.Prec _ content) = extractFields content
extractFields (TSGN.PrecLeft _ content) = extractFields content
extractFields (TSGN.PrecRight _ content) = extractFields content
extractFields (TSGN.PrecDynamic _ content) = extractFields content
extractFields (TSGN.Token content) = extractFields content
extractFields (TSGN.ImmediateToken content) = extractFields content
extractFields (TSGN.Alias content _ _) = extractFields content
extractFields (TSGN.Reserved content _) = extractFields content
extractFields _ = []

mergeFields :: [(String, TSGN.Rule, Bool)] -> [(String, TSGN.Rule, Bool)]
mergeFields fields =
  let merge (c1, a1) (c2, a2) = (if c1 == c2 then c1 else TSGN.Choice [c1, c2], a1 || a2) -- simple merge
      m = M.fromListWith merge (map (\(n, c, a) -> (n, (c, a))) fields)
   in map (\(n, (c, a)) -> (n, c, a)) (M.toList m)

isTokenRule :: TSGN.Rule -> Bool
isTokenRule (TSGN.StringLit _) = True
isTokenRule (TSGN.Pattern _) = True
isTokenRule TSGN.Blank = True
isTokenRule (TSGN.Token _) = True
isTokenRule (TSGN.ImmediateToken _) = True
isTokenRule (TSGN.Alias content _ _) = isTokenRule content
isTokenRule (TSGN.Prec _ content) = isTokenRule content
isTokenRule (TSGN.PrecLeft _ content) = isTokenRule content
isTokenRule (TSGN.PrecRight _ content) = isTokenRule content
isTokenRule (TSGN.PrecDynamic _ content) = isTokenRule content
isTokenRule (TSGN.Reserved content _) = isTokenRule content
-- Symbol could be token or non-token; assume non-token (interior)
isTokenRule _ = False

fieldToPropDecl :: (String, TSGN.Rule, Bool) -> Text
fieldToPropDecl (fieldName, content, isArray) =
  let propName = pack $ node_name_ident fieldName
      base = baseType content
      typeStr = pack $ if isArray then base ++ "[] | undefined;" else base ++ " | undefined;"
   in TT.inst TTS.public_qualifier $ TT.inst TTS.parameter_declare propName typeStr

baseType :: TSGN.Rule -> String
baseType (TSGN.Symbol symName) = node_type_ident symName
baseType (TSGN.Choice members) =
  let inner = intercalate " | " (map baseType members)
   in "(" ++ inner ++ ")"
baseType (TSGN.Seq _) = "any"
baseType (TSGN.Repeat content) = baseType content ++ "[]"
baseType (TSGN.Repeat1 content) = baseType content ++ "[]"
baseType (TSGN.Field _ content) = baseType content
baseType (TSGN.Prec _ content) = baseType content
baseType (TSGN.PrecLeft _ content) = baseType content
baseType (TSGN.PrecRight _ content) = baseType content
baseType (TSGN.PrecDynamic _ content) = baseType content
baseType (TSGN.Token content) = baseType content
baseType (TSGN.ImmediateToken content) = baseType content
baseType (TSGN.Alias content _ _) = baseType content
baseType (TSGN.Reserved content _) = baseType content
baseType _ = "any"

fieldToConstructorStmt :: (String, TSGN.Rule, Bool) -> Text
fieldToConstructorStmt (fieldName, content, isArray) =
  case content of
    TSGN.Symbol symName ->
      if isArray
        then TTS.prop_initialize_array (pack $ node_name_ident fieldName) (pack symName) (pack $ node_type_ident symName)
        else TTS.prop_initialize (pack symName) (pack $ node_name_ident fieldName) (pack $ node_type_ident symName)
    _ -> pack "" -- TODO: handle other types
