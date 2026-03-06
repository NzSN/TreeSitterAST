{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module ProgBuilder.FieldConversion
  ( fieldsFromProperties,
    evalFieldName,
    evalFieldType,
    propFromTSGN,
    propertyTypeStr,
    isStrProp,
    isBuiltin,
    typeShow,
    toGrammarNodeWithField,
    extractFields,
  )
where

import Control.Monad.State
import Data.List as L (intercalate, nub)
import Data.Maybe (catMaybes, isJust)
import Data.Text.Lazy qualified as T
import ProgBuilder.ProgBuilderDescription (GrammarNodeWithProp, Property (..), propType, propsOfNode)
import ProgBuilder.Types qualified as Typ
import TreeSitterGrammarNodes (GrammarNode, Node)
import TreeSitterGrammarNodes qualified as TSGN
import Utility (upper_the_first_char)

-- | ECMA Knowledge
isBuiltin :: T.Text -> Bool
isBuiltin x
  | x `elem` ["string", "number", "boolean", "undefined"] = True
  | otherwise = False

-- | Convert a type name to TypeScript type string.
-- Built-in types are kept as-is, others get "_T" suffix and capitalized first letter.
typeShow :: T.Text -> String
typeShow x =
  if isBuiltin x
    then T.unpack x
    else (++ "_T") . upper_the_first_char . T.unpack $ x

-- | Convert Maybe Text to Text, defaulting to "string" for Nothing.
asTypeStr :: Maybe T.Text -> T.Text
asTypeStr Nothing = "string"
asTypeStr (Just s) = s

-- | Get TypeScript type string for a Property
propertyTypeStr :: Property -> T.Text
propertyTypeStr prop = case prop of
  SymbolProp name _ ->
    T.pack $ upper_the_first_char (T.unpack name) ++ "_T"
  NamedProp _ p_types _ ->
    let types = map (asTypeStr . propType) p_types
        typeStrs = nub $ map typeShow types
     in if null typeStrs
          then "undefined"
          else T.pack $ L.intercalate " | " typeStrs
  StrProp _ _ -> "string"

-- | Convert a Property to a Field (without suffix indexing)
propFromTSGN :: Property -> Typ.Field
propFromTSGN x
  | (SymbolProp p_type _) <- x =
      let tsType = T.pack $ upper_the_first_char (T.unpack p_type) ++ "_T"
       in Typ.Field p_type tsType
  | (StrProp _ _) <- x = Typ.EmptyField
  | (NamedProp p_name p_types _) <- x = Typ.SumField p_name $ map (asTypeStr . propType) p_types

-- | Check if a property is a string literal (StrProp)
isStrProp :: Property -> Bool
isStrProp (StrProp _ _) = True
isStrProp _ = False

-- | Convert list of properties to fields with sequential suffixes.
-- String literals are skipped (no field). Other properties get _0, _1 suffixes.
fieldsFromProperties :: [Property] -> [Typ.Field]
fieldsFromProperties props = go 0 props
  where
    go _ [] = []
    go idx (prop : rest)
      | isStrProp prop = go idx rest -- skip string literals, no field
      | otherwise =
          let baseField = propFromTSGN prop
              suffixedField = case baseField of
                Typ.Field name ftype -> Typ.Field (T.concat [name, T.pack $ "_" ++ show idx]) ftype
                Typ.SumField name ftypes -> Typ.SumField (T.concat [name, T.pack $ "_" ++ show idx]) ftypes
                Typ.EmptyField -> Typ.EmptyField -- shouldn't happen since StrProp already filtered
           in suffixedField : go (idx + 1) rest

-- | Get the instance variable name for a field (adds _i suffix)
evalFieldName :: Typ.Field -> T.Text
evalFieldName f
  | (Typ.Field f_name _) <- f = T.concat [f_name, "_i"]
  | (Typ.SumField f_name _) <- f = T.concat [f_name, "_i"]
  | Typ.EmptyField <- f = ""

-- | Collapse sum type field to TypeScript union type string
collapseSumType :: Typ.Field -> T.Text
collapseSumType (Typ.SumField _ []) = T.pack "undefined"
collapseSumType (Typ.SumField _ types) =
  T.pack $
    L.intercalate " | " $
      nub $
        map T.unpack types
-- Unreachable
collapseSumType _ = undefined

-- | Get TypeScript type string for a field
evalFieldType :: Typ.Field -> T.Text
evalFieldType f
  | (Typ.Field _ f_type) <- f = f_type
  | field@(Typ.SumField _ _) <- f = collapseSumType field
  | Typ.EmptyField <- f = ""

toGrammarNodeWithField :: GrammarNodeWithProp -> Typ.GrammarNodeWithField
toGrammarNodeWithField node = evalState (traverse propertyToAnnoatedField node) 0
  where
    propertyToAnnoatedField :: Property -> State Int Typ.AnnoatedField
    propertyToAnnoatedField prop = do
      idx <- get
      if isStrProp prop
        then return $ Typ.AnnoatedField (Just (str_value prop)) Typ.EmptyField Nothing
        else do
          put (idx + 1)
          let field = propFromTSGN prop
              original = case prop of
                SymbolProp t _ -> Just t
                NamedProp n _ _ -> Just n
                _ -> Nothing -- shouldn't happen (StrProp already filtered)
          return $ Typ.AnnoatedField original field (Just idx)

-- | Extract fields from an annotated grammar tree.
-- Collects all non-empty fields from leaf AnnoatedField nodes,
-- adding positional suffixes based on their stored indices.
-- Note: Field nodes (TSGN.Field) are not handled specially;
-- only leaf annotations are considered.
extractFields :: Typ.GrammarNodeWithField -> [Typ.Field]
extractFields node = catMaybes $ foldMap extractFieldFromAnnoated node
  where
    extractFieldFromAnnoated :: Typ.AnnoatedField -> [Maybe Typ.Field]
    extractFieldFromAnnoated (Typ.AnnoatedField _ field idx) =
      case (field, idx) of
        (Typ.EmptyField, _) -> [Nothing]
        (_, Nothing) -> [Nothing] -- shouldn't happen
        (_, Just i) -> [Just $ addSuffixToField field i]

    addSuffixToField :: Typ.Field -> Int -> Typ.Field
    addSuffixToField (Typ.Field name ftype) idx =
      Typ.Field (T.concat [name, T.pack $ "_" ++ show idx]) ftype
    addSuffixToField (Typ.SumField name ftypes) idx =
      Typ.SumField (T.concat [name, T.pack $ "_" ++ show idx]) ftypes
    addSuffixToField Typ.EmptyField _ = Typ.EmptyField
