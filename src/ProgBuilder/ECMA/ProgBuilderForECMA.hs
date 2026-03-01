{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ECMA.ProgBuilderForECMA where

import Data.List as L
import Data.Map qualified as Map
import Data.Text.Lazy qualified as T
import ProgBuilder.ProgBuilderDescription
  ( Property (..),
    propsOfNode, propType,
  )
import Template.Template qualified as TT
import Template.TypeScriptTemplate qualified as TTS
import TreeSitterGrammarNodes (isLeaf)
import TreeSitterGrammarNodes qualified as TSGN
import Fundamentals.Inference (transformGrammarWithChoiceSplitting)
import TypedASTGenerator.NodeDescriptionHelper
import Utility (upper_the_first_char)

descript :: TSGN.Grammar -> String
descript grammar =
  let processed = TSGN.convert grammar
      origGrammar = TSGN.orig processed
      transformedGrammar = transformGrammarWithChoiceSplitting origGrammar
      rules = TSGN.grammarNodes transformedGrammar
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
              Nothing
            )
          else
            ( "SyntaticInterior",
              interiorConstructor fields,
              Just $ TT.TArray $ collapse' $ propFromTSGNs fields
            )
      methods = Just $ TT.TArray [constructorDef]
   in T.concat
        [ TT.inst TTS.export_qualifier $
            TT.inst TTS.class_declare className (Just baseClass) props methods,
          "\n"
        ]
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
        ( TT.TArray
            interiorPrologueStmts
            -- ++ [fieldToConstructorStmt props]
        )

    interiorPrologueStmts :: [T.Text]
    interiorPrologueStmts =
      [TT.inst TTS.function_call "super" Nothing]

-- | Represent a property field of a Javascript class.
data Field
  = Field {field_name :: T.Text, field_type :: T.Text}
  | SumField {field_name :: T.Text, field_types :: [T.Text]}
  | EmptyField
  deriving (Show, Eq, Ord)

eval :: Field -> T.Text
eval f
  | field@(Field _ _) <- f = evaluate field
  | field@(SumField _ _) <- f = evaluate field
  | EmptyField <- f = ""
  where
    evaluate field = T.concat [evalFieldName field, " : ", evalFieldType field]

evalFieldName :: Field -> T.Text
evalFieldName f
  | (Field f_name _) <- f = T.concat [f_name, "_i"]
  | (SumField f_name _) <- f = T.concat [f_name, "_i"]
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
      T.pack $ L.intercalate " | " $
        nub $ map typeShow types
    -- Unreachable
    collapseSumType _ = undefined

    typeShow :: T.Text -> String
    typeShow x = if isBuiltin x
                  then T.unpack x
                  else (++ "_T") . upper_the_first_char . T.unpack $ x


collapse' :: [Field] -> [T.Text]
collapse' = map (\f -> T.concat [eval f, ";\n"])

propFromTSGNs :: [Property] -> [Field]
propFromTSGNs = foldl (appendIDX 0) [] . (L.group . L.sort . propFromTSGNs')
  where
    appendIDX :: Int -> [Field] -> [Field] -> [Field]
    appendIDX i r (Field f_name f_type : xs) =
      Field (modifyFieldName i f_name) f_type : appendIDX (i + 1) [] xs ++ r
    appendIDX i r (SumField f_name f_types : xs) =
      SumField (modifyFieldName i f_name) f_types : appendIDX (i + 1) [] xs ++ r
    appendIDX i r (EmptyField : xs) = appendIDX i [] xs ++ r
    appendIDX _ _ [] = []

    modifyFieldName :: Int -> T.Text -> T.Text
    modifyFieldName i t = T.concat [t, T.pack . ("_" ++) . show $ i]

propFromTSGNs' :: [Property] -> [Field]
propFromTSGNs' = map propFromTSGN

propFromTSGN :: Property -> Field
propFromTSGN x
  | (SymbolProp p_type) <- x = Field p_type p_type
  | (StrProp _) <- x = EmptyField
  | (NamedProp p_name p_types) <- x = do
      SumField p_name $ map (asTypeStr . propType) p_types
  where
    asTypeStr :: Maybe T.Text -> T.Text
    asTypeStr x'
      | Nothing <- x' = "string"
      | Just s  <- x' = s

-- | ECMA Knowledge
isBuiltin :: T.Text -> Bool
isBuiltin x
  | x == "string" = True
  | otherwise = False
