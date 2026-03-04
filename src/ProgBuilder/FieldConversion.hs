{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module ProgBuilder.FieldConversion
  ( fieldsFromProperties
  , evalFieldName
  , evalFieldType
  , propFromTSGN
  , propertyTypeStr
  , isStrProp
  , isBuiltin
  , typeShow
  ) where

import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.List as L (nub, intercalate, any)
import ProgBuilder.ProgBuilderDescription (Property(..), propType)
import ProgBuilder.Types (Field(..))
import Utility (upper_the_first_char)

-- | ECMA Knowledge
isBuiltin :: T.Text -> Bool
isBuiltin x
  | x == "string" = True
  | otherwise = False

-- | Convert a type name to TypeScript type string.
-- Built-in types are kept as-is, others get "_T" suffix and capitalized first letter.
typeShow :: T.Text -> String
typeShow x = if isBuiltin x
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
propFromTSGN :: Property -> Field
propFromTSGN x
  | (SymbolProp p_type _) <- x = Field p_type p_type
  | (StrProp _ _) <- x = EmptyField
  | (NamedProp p_name p_types _) <- x = SumField p_name $ map (asTypeStr . propType) p_types

-- | Check if a property is a string literal (StrProp)
isStrProp :: Property -> Bool
isStrProp (StrProp _ _) = True
isStrProp _ = False

-- | Convert list of properties to fields with sequential suffixes.
-- String literals are skipped (no field). Other properties get _0, _1 suffixes.
fieldsFromProperties :: [Property] -> [Field]
fieldsFromProperties props = go 0 props
  where
    go _ [] = []
    go idx (prop : rest)
      | isStrProp prop = go idx rest  -- skip string literals, no field
      | otherwise =
          let baseField = propFromTSGN prop
              suffixedField = case baseField of
                Field name ftype -> Field (T.concat [name, T.pack $ "_" ++ show idx]) ftype
                SumField name ftypes -> SumField (T.concat [name, T.pack $ "_" ++ show idx]) ftypes
                EmptyField -> EmptyField  -- shouldn't happen since StrProp already filtered
          in suffixedField : go (idx + 1) rest

-- | Get the instance variable name for a field (adds _i suffix)
evalFieldName :: Field -> T.Text
evalFieldName f
  | (Field f_name _) <- f = T.concat [f_name, "_i"]
  | (SumField f_name _) <- f = T.concat [f_name, "_i"]
  | EmptyField <- f = ""

-- | Collapse sum type field to TypeScript union type string
collapseSumType :: Field -> T.Text
collapseSumType (SumField _ []) = T.pack "undefined"
collapseSumType (SumField _ types) =
  T.pack $ L.intercalate " | " $
    nub $ map typeShow types
-- Unreachable
collapseSumType _ = undefined

-- | Get TypeScript type string for a field
evalFieldType :: Field -> T.Text
evalFieldType f
  | (Field _ f_type) <- f =
      T.pack $ upper_the_first_char (T.unpack f_type) ++ "_T"
  | field@(SumField _ _) <- f = collapseSumType field
  | EmptyField <- f = ""