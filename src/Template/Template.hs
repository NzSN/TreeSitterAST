{-# LANGUAGE TemplateHaskell #-}
module Template.Template (
  Template(..),
  TArray(..),
  inst) where

import Formatting (Format, format)
import Formatting.Buildable (Buildable(..))
import Data.Text.Lazy (Text)

newtype Template args = T { eval :: Format Text args }
inst :: Template a -> a
inst t = format $ eval t

newtype TArray a = TArray { params :: [a] } deriving (Show,Foldable)
instance Buildable a => Buildable (TArray a) where
  build tarray = mconcat $ map build $ params tarray
