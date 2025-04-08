{-# LANGUAGE DeriveAnyClass #-}

module CPP_Backend where

import qualified Data.Aeson as Aeson
import qualified Backends
import qualified Data.ByteString.Lazy as BSL

data CPP_Backend = CPP_Backend { a :: String }

instance Backends.Backend CPP_Backend where
  name :: CPP_Backend -> String
  name _ = "CPP"

  gen_ast :: CPP_Backend -> [Aeson.Object] -> BSL.ByteString
  gen_ast backend json = generate_cpp_output backend json

generate_cpp_output :: CPP_Backend -> [Aeson.Object] -> BSL.ByteString
generate_cpp_output backend json = undefined
