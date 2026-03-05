{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.Types
  ( Field(..),
    GrammarNodeWithField,
    AnnoatedField(..),
  ) where

import qualified Data.Text.Lazy as T
import TreeSitterGrammarNodes (GrammarNode)

-- | Represent a property field of a Javascript class.
data Field
  = Field {field_name :: T.Text, field_type :: T.Text}
  | SumField {field_name :: T.Text, field_types :: [T.Text]}
  | EmptyField
  deriving (Show, Eq, Ord)

data AnnoatedField = AnnoatedField { original_value :: Maybe T.Text, annoated_field :: Field, field_index :: Maybe Int } deriving (Show)
type GrammarNodeWithField = GrammarNode AnnoatedField
