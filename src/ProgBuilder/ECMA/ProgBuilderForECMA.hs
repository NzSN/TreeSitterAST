{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ECMA.ProgBuilderForECMA where

import Data.List as L
import Data.Map qualified as Map
import Data.Text.Lazy (Text, pack, unpack)
import Debug.Trace (trace)
import ProgBuilder.ProgBuilderDescription
  ( Interior (..),
    Leaf (..),
    fromTSGN,
  )
import Template.Template qualified as TT
import Template.TypeScriptTemplate qualified as TTS
import TreeSitterGrammarNodes (isTokenRule)
import TreeSitterGrammarNodes qualified as TSGN
import TypedASTGenerator.NodeDescriptionHelper
import Utility (upper_the_first_char)

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
      fields = fromTSGN rule
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

    interiorConstructor :: [Interior] -> Text
    interiorConstructor fields' =
      TT.inst
        TTS.const_declare
        (TT.TArray [])
        ( TT.TArray $
            interiorPrologueStmts
              ++ map
                fieldToConstructorStmt
                ( flip filter fields' $
                    \case
                      (Leaf (InteriorLiteral _)) -> False
                      _ -> True
                )
        )

    interiorPrologueStmts :: [Text]
    interiorPrologueStmts =
      [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "NodeType.Interior"])]

type FieldIdentText = Text

type FieldTypeStr = String

fieldToPropDecl :: Interior -> Text
fieldToPropDecl x
  | (Leaf ref@(InteriorRef _)) <- x =
      let prop_literal = propLiteral "__i" ref
          typeStr = pack $ snd prop_literal ++ " | undefined;"
       in TT.inst TTS.public_qualifier $ TT.inst TTS.parameter_declare (fst prop_literal) typeStr
  | (Repeat (Leaf literal@(InteriorLiteral _))) <- x =
      let prop_literal = propLiteral "__array_i" literal
          typeStr = pack $ snd prop_literal ++ "[] = [];"
       in TT.inst TTS.public_qualifier $ TT.inst TTS.parameter_declare (fst prop_literal) typeStr
  | (Repeat (Repeat prop)) <- x = fieldToPropDecl prop
  | (Repeat (Choice _)) <- x = ""
  | (Choice _) <- x = ""
  where
    propLiteral :: String -> Leaf -> (FieldIdentText, FieldTypeStr)
    propLiteral _ (InteriorLiteral _) = error "Literal should not declared as field"
    propLiteral qualifier (InteriorRef n) = (pack (upper_the_first_char n ++ qualifier), n)

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

fieldToConstructorStmt :: Interior -> Text
fieldToConstructorStmt x
  | (Leaf (InteriorLiteral _)) <- x =
      error "Construct field for Literal Interior is not expected"
  | (Leaf (InteriorRef _)) <- x =
      "{ /* Initialization Ref field */ }"
  | (Repeat (Leaf (InteriorLiteral _))) <- x =
      "{ /* Array-field Initialization */ }\n"
  | (Repeat (Repeat prop)) <- x = fieldToConstructorStmt prop
  | (Repeat (Choice _)) <- x = pack ""
  | (Choice _) <- x = pack ""
