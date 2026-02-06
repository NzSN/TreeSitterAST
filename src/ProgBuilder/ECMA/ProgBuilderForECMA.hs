{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

module ProgBuilder.ECMA.ProgBuilderForECMA where

import Control.Monad.Trans.State

import Data.List as L
import Data.Map qualified as Map
import qualified Data.Text.Lazy as T
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
      builder_def = Map.foldrWithKey (\name rule acc -> T.concat [acc, build name rule]) "" rules
   in T.unpack $ T.concat [imports, prologue, builder_def]

imports :: T.Text
imports =
    TT.inst
      TTS.import_statement
      (TT.TArray ["strict as assert"])
      "assert"

prologue :: T.Text
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
          super();
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
  }\n
  """

build :: String -> TSGN.Node -> T.Text
build name rule =
  let className = T.pack $ node_type_ident name
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
              Just $ TT.TArray $ collapse' $ evalState (propFromTSGNs fields) 0)
      methods = Just $ TT.TArray [constructorDef]
   in T.concat [TT.inst TTS.export_qualifier $
        TT.inst TTS.class_declare className (Just baseClass) props methods, "\n"]
  where
    leafConstructor :: T.Text
    leafConstructor =
      TT.inst
        TTS.const_declare
        (TT.TArray [TT.inst TTS.parameter_declare "value" "string"])
        (TT.TArray [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "value"])])

    interiorConstructor :: [Property] -> T.Text
    interiorConstructor _ =
      TT.inst
        TTS.const_declare
        (TT.TArray [])
        (TT.TArray
            interiorPrologueStmts
              -- ++ [fieldToConstructorStmt props]
        )

    interiorPrologueStmts :: [T.Text]
    interiorPrologueStmts =
      [TT.inst TTS.function_call "super" Nothing]

data Field =
  Field { field_name :: T.Text, field_type :: T.Text } |
  SumField { field_name :: T.Text, field_types :: [T.Text] } |
  EmptyField
  deriving (Show)

eval :: Field -> T.Text
eval f
  | field@(Field _ _) <- f = evaluate field
  | field@(SumField _ _) <- f = evaluate field
  | EmptyField <- f = ""
  where evaluate field = T.concat [evalFieldName field, " : " , evalFieldType field]

mergeFieldType :: Field -> Field -> Field
mergeFieldType f0 f1 = case (f0, f1) of
    (Field name0 ty0, Field name1 ty1) | name0 == name1 ->
        SumField name0 $ dedupTypes [ty0, ty1]
    (Field name0 ty0, SumField name1 tys1) | name0 == name1 ->
        SumField name0 $ dedupTypes (ty0 : tys1)
    (SumField name0 tys0, Field name1 ty1) | name0 == name1 ->
        SumField name0 $ dedupTypes (tys0 ++ [ty1])
    (SumField name0 tys0, SumField name1 tys1) | name0 == name1 ->
        SumField name0 $ dedupTypes (tys0 ++ tys1)
    _ -> EmptyField
  where
    dedupTypes :: [T.Text] -> [T.Text]
    dedupTypes = L.nub

evalFieldName :: Field -> T.Text
evalFieldName f
  | (Field f_name _) <- f = T.concat [f_name, "__i"]
  | (SumField f_name _) <- f = T.concat [f_name, "__i"]
  | EmptyField <- f = ""


evalFieldType :: Field -> T.Text
evalFieldType f
  | (Field _ f_type) <- f =
      T.pack $ upper_the_first_char (T.unpack f_type) ++ "_T"
   | field@(SumField _ _) <- f = collapseSumType field
  | EmptyField <- f = ""
  where
    collapseSumType :: Field -> T.Text
    collapseSumType (SumField _ []) = T.pack "undefined"
    collapseSumType (SumField _ types) =
      T.pack $ L.intercalate " | " $ map ((++ "_T") . upper_the_first_char . T.unpack) types
    -- Unreachable
    collapseSumType _ = undefined


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
      -- Igonore the inner field type hence
      -- collapse type of fields only.
      let eval_type_str = get >>=
            \s -> return $
                    collapseFieldType (evalState (propFromTSGNs p_types) s)
      in do
        ident <- get
        let (typestr,next) = runState eval_type_str ident
        put next
        return $ SumField p_name typestr

  where
    collapseFieldType :: [Field] -> [T.Text]
    collapseFieldType = map field_type . filter
      (\case { EmptyField -> False; _ -> True })

mergeDuplicates :: [Field] -> [Field]
mergeDuplicates fields = Map.elems $ L.foldl' insertField Map.empty (filter (not . isEmpty) fields)
  where
    isEmpty :: Field -> Bool
    isEmpty EmptyField = True
    isEmpty _ = False

    insertField :: Map.Map T.Text Field -> Field -> Map.Map T.Text Field
    insertField acc field = case field of
      EmptyField -> acc
      _ -> Map.alter (combine field) (fieldName field) acc

    fieldName :: Field -> T.Text
    fieldName (Field name _) = name
    fieldName (SumField name _) = name
    fieldName EmptyField = ""  -- Should not happen due to case above

    combine :: Field -> Maybe Field -> Maybe Field
    combine newField Nothing = Just newField
    combine newField (Just oldField) =
      case mergeFieldType oldField newField of
        EmptyField -> Nothing  -- Should not happen for same field names
        merged -> Just merged

collapse' :: [Field] -> [T.Text]
collapse' fields = map (\f -> T.concat [eval f, ";\n"]) (mergeDuplicates fields)
