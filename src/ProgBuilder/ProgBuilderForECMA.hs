{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ProgBuilderForECMA where

import Data.List as L
import Data.Map qualified as Map
import Data.Text.Lazy (Text, pack, unpack)
import Debug.Trace (trace)
import ProgBuilder.ProgBuilderDescription
  ( Property (..),
    PropertyVar (..),
    propsOfNode,
  )
import Template.Template qualified as TT
import Template.TypeScriptTemplate qualified as TTS
import TreeSitterGrammarNodes (isTokenRule)
import TreeSitterGrammarNodes qualified as TSGN
import TypedASTGenerator.NodeDescriptionHelper

descript :: TSGN.Grammar -> String
descript grammar =
  let rules = TSGN.grammarNodes grammar
      builder_def = Map.foldrWithKey (\name rule acc -> acc ++ build name rule) "" rules
   in imports ++ prologue ++ builder_def

imports :: String
imports =
  unpack
    ( TT.inst
        TTS.import_statement
        (TT.TArray ["strict as assert"])
        "assert"
    )

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

build :: String -> TSGN.Node -> String
build name rule =
  let className = pack $ node_type_ident name
      fields = propsOfNode rule
      isLeaf = isTokenRule rule
      (baseClass, constructorDef, props) =
        if isLeaf
          then
            ( "SyntaticLeaf",
              leafConstructor,
              Nothing
            )
          else
            ( "SyntaticInterior",
              interiorConstructor (trace (show fields) fields),
              Just $ TT.TArray $ map fieldToPropDecl fields
            )
      methods = Just $ TT.TArray [constructorDef]
   in unpack $
        TT.inst TTS.export_qualifier $
          TT.inst TTS.class_declare className (Just baseClass) props methods
  where
    leafConstructor :: Text
    leafConstructor =
      TT.inst
        TTS.const_declare
        (TT.TArray [TT.inst TTS.parameter_declare "value" "string"])
        (TT.TArray [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "value"])])

    interiorConstructor :: [Property] -> Text
    interiorConstructor fields' =
      TT.inst
        TTS.const_declare
        (TT.TArray [])
        (TT.TArray $ interiorPrologueStmts ++ map fieldToConstructorStmt fields')

    interiorPrologueStmts :: [Text]
    interiorPrologueStmts =
      [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "NodeType.Interior"])]

fieldToPropDecl :: Property -> Text
fieldToPropDecl x
  | (Property literal@(PropertyLiteral _)) <- x =
      let prop_literal = propLiteral "__i" literal
          typeStr = pack $ snd prop_literal ++ " | undefined;"
       in TT.inst TTS.public_qualifier $ TT.inst TTS.parameter_declare (fst prop_literal) typeStr
  | (Repeat (Property literal@(PropertyLiteral _))) <- x =
      let prop_literal = propLiteral "__array_i" literal
          typeStr = pack $ snd prop_literal ++ "[] = [];"
       in TT.inst TTS.public_qualifier $ TT.inst TTS.parameter_declare (fst prop_literal) typeStr
  | (Repeat (Repeat prop)) <- x = fieldToPropDecl prop
  | (Repeat (Choice _)) <- x = ""
  | (Choice _) <- x = ""
  where
    propLiteral :: String -> PropertyVar -> (Text, String)
    propLiteral qualifier (PropertyLiteral n) = (++ qualifier) . prop_name

baseType :: TSGN.Node -> String
baseType (TSGN.Symbol symName) = node_type_ident symName
baseType (TSGN.Choice members) =
  let inner = L.intercalate " | " (map baseType members)
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

fieldToConstructorStmt :: Property -> Text
fieldToConstructorStmt x
  | (Property (PropertyLiteral _)) <- x =
      case p_type of
        TSGN.Symbol _ -> "{ /* Non-Array-field Initialization */ }\n"
        _ -> pack ""
  | (Repeat (Property (PropertyLiteral _))) <- x =
      case p_type of
        TSGN.Symbol _ -> "{ /* Array-field Initialization */ }\n"
        _ -> pack ""
  | (Repeat (Repeat prop)) <- x = fieldToConstructorStmt prop
  | (Repeat (Choice _)) <- x = pack ""
  | (Choice _) <- x = pack ""
