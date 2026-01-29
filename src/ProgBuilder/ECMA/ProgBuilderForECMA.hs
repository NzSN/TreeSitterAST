{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ECMA.ProgBuilderForECMA where

import Control.Monad.Trans.State

import Data.List as L
import Data.Map qualified as Map
import Data.Text.Lazy (Text, pack, unpack)
import ProgBuilder.ProgBuilderDescription
  ( Property(..),
    propsOfNode,
  )
import Template.Template qualified as TT
import Template.TypeScriptTemplate qualified as TTS
import TreeSitterGrammarNodes (isLeaf)
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
    (TT.inst
      TTS.import_statement
      (TT.TArray ["strict as assert"])
      "assert")

prologue :: String
prologue =
  """
  export class SyntaticNode {
      evaluate(): string {
          throw Error("Interior or Leaf should implement evaluate().");
      }
  }

  export class SyntaticLeaf extends SyntaticNode {
      value_: string;
      constructor(value: string) {
          this.value_ = value;
      }
      evaluate(): string {
          super();
          return this.value_;
      }
  }

  export class SyntaticInterior extends SyntaticNode {
      evaluate(): string {
          throw Error("Instance of Interior should implment evaluate()");
      }
  }\n
  """

build :: String -> TSGN.Node -> String
build name rule =
  let className = pack $ node_type_ident name
      fields = propsOfNode rule
      (baseClass, constructorDef, props) =
        if isLeaf rule
          then
            ( "SyntaticLeaf",
              leafConstructor,
              Nothing)
          else
            ("SyntaticInterior",
              interiorConstructor fields,
              Just $ TT.TArray $ map pack $ collapse' $ evalState (propFromTSGNs fields) 0)
      methods = Just $ TT.TArray [constructorDef]
   in (++ "\n") $ unpack $
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
    interiorConstructor _ =
      TT.inst
        TTS.const_declare
        (TT.TArray [])
        (TT.TArray
            interiorPrologueStmts
              -- ++ [fieldToConstructorStmt props]
        )

    interiorPrologueStmts :: [Text]
    interiorPrologueStmts =
      [TT.inst TTS.function_call "super" Nothing]

data Field =
  Field { field_name :: String, field_type :: String } |
  EmptyField
  deriving (Show)

eval :: Field -> String
eval f
  | (Field f_name f_type) <- f = f_name ++ " : " ++ f_type
  | EmptyField <- f = ""

evalFieldName :: Field -> String
evalFieldName f
  | (Field f_name _) <- f = f_name ++ "__i"
  | EmptyField <- f = ""

evalFieldType :: Field -> String
evalFieldType f
  | (Field _ f_type) <- f = upper_the_first_char f_type ++ "_T"
  | EmptyField <- f = ""

propFromTSGNs :: [Property] -> State Int [Field]
propFromTSGNs (x:xs) = do
  ident_id <- get
  let (field, next) = runState (propFromTSGN x) ident_id
  return $ field : evalState (propFromTSGNs xs) next
propFromTSGNs [] = return []


propFromTSGN :: Property -> State Int Field
propFromTSGN x
  | (SymbolProp p_type) <- x = return $ Field p_type p_type
  | (StrProp _) <- x = return EmptyField
  | (NamedProp p_name p_types) <- x =
      let prop_literal = p_name ++ "__i"
          -- Igonore the inner field type hence
          -- collapse type of fields only.
          eval_type_str = get >>= \s -> return $ collapseFieldType (evalState (propFromTSGNs p_types) s)
      in do
        ident <- get
        let (typestr,next) = runState eval_type_str ident
        put next
        return $ Field prop_literal typestr

collapseFieldType :: [Field] -> String
collapseFieldType fs = foldl type_plus "" $ map evalFieldType fs
  where type_plus acc t = acc ++ t ++ " | "

collapse' :: [Field] -> [String]
collapse' (EmptyField:_) = []
collapse' (x:xs) = (eval x ++ ";\n") : collapse' xs
collapse' [] = []

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

-- fieldToConstructorStmt :: [Property] -> Text
-- fieldToConstructorStmt x
--   | (Leaf (InteriorLiteral _)) <- x =
--       error "Construct field for Literal Interior is not expected"
--   | (Leaf (InteriorRef _)) <- x =
--       "{ /* Initialization Ref field */ }"
--   | (Repeat (Leaf (InteriorLiteral _))) <- x =
--       "{ /* Array-field Initialization */ }\n"
--   | (Repeat (Repeat prop)) <- x = fieldToConstructorStmt prop
--   | (Repeat (Choice _)) <- x = pack ""
--   | (Choice _) <- x = pack ""
