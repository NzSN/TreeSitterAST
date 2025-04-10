module Template.Template (
  Template(..),
  TArray(..),
  inst) where

import Formatting (Format, formatToString)
import Formatting.Buildable (Buildable(..))

data Template args = T { eval :: Format String args }

inst :: Template a -> a
inst t = formatToString $ eval t

data TArray a = TArray { params :: [a] } deriving (Show,Foldable)
instance Buildable a => Buildable (TArray a) where
  build tarray = mconcat $ map build $ params tarray
