{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.Types
  ( Field(..)
  ) where

import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)

-- | Represent a property field of a Javascript class.
data Field
  = Field {field_name :: T.Text, field_type :: T.Text}
  | SumField {field_name :: T.Text, field_types :: [T.Text]}
  | EmptyField
  deriving (Show, Eq, Ord)