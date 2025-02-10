module Backends (Backend(..)) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

class Backend a where
  name :: a -> String
  gen_ast :: a -> [Aeson.Object] -> BSL.ByteString
