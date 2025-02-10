module TreeSitterNodes
(BasicInfo(..),
 ChildInfo(..),
 Field(..),
 Node(..),
 parse_node_types,
) where

import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Aeson.Key
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BSL_Char8

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

data BasicInfo =
  BasicInfo { node_type :: String,
              named     :: Bool }
  deriving (Show)

data ChildInfo =
  ChildInfo { required  :: Bool,
              multipled :: Bool,
              types     :: [BasicInfo] }
  | NoChild
  deriving (Show)

data Field =
  Field { field_name :: String,
          field_info :: ChildInfo }
  deriving (Show)

data Node =
  Leaf { info :: BasicInfo } |
  Internal { info :: BasicInfo,
             fields :: [Field],
             children :: ChildInfo }
  deriving (Show)

parse_node_types :: String -> MaybeT IO [Node]
parse_node_types path = do
  content <- lift $ readFile path

  let json = Aeson.decode (BSL_Char8.pack content) :: Maybe [Aeson.Object]

  case json of
    Nothing  -> MaybeT $ return Nothing
    Just obj -> return $ fromJust $ parseAsNodeTypes obj

  where
    parseAsNodeTypes :: [Aeson.Object] -> Maybe [Node]
    parseAsNodeTypes [] = Just []
    parseAsNodeTypes (x:xs) =
      let current =
            flip parseMaybe x $ \obj -> do
                node_type <- obj .: (fromString "type")
                named     <- obj .: (fromString "named")

                return $ Leaf (BasicInfo node_type named)

          rest = parseAsNodeTypes xs
      in if (isNothing current) || (isNothing rest)
         then Nothing
         else return $ [fromJust current] ++ (fromJust rest)
