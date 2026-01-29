{-# LANGUAGE OverloadedStrings #-}

module TreeSitterNodes
  ( NodeInfo (..),
    Children (..),
    Node (..),
    parse_node_types,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 qualified as BSL_Char8
import Data.Map
import GHC.Generics

data NodeInfo
  = NodeInfo
  { node_type :: String,
    named :: Bool
  }
  deriving (Show, Generic)

instance ToJSON NodeInfo

instance FromJSON NodeInfo where
  parseJSON (Object v) =
    NodeInfo
      <$> v .: "type"
      <*> v .: "named"
  parseJSON invalid =
    prependFailure
      "Parsing NodeInfo failed"
      (typeMismatch "Object" invalid)

data Children
  = Children
  { required :: Bool,
    multiple :: Bool,
    types :: [NodeInfo]
  }
  deriving (Show, Generic)

instance ToJSON Children

instance FromJSON Children

data Node
  = Leaf {info :: NodeInfo}
  | Interior
      { info :: NodeInfo,
        fields :: Maybe (Map String Children),
        subtypes :: Maybe [NodeInfo],
        children :: Maybe Children
      }
  deriving (Show, Generic)

instance ToJSON Node

instance FromJSON Node

parse_node_types :: String -> MaybeT IO [Node]
parse_node_types path = do
  content <- lift $ readFile path

  let json = Aeson.decode (BSL_Char8.pack content) :: Maybe [Aeson.Object]

  case json of
    Nothing -> MaybeT $ return Nothing
    Just obj -> case parseAsNodeTypes obj of
      Nothing -> MaybeT $ return Nothing
      Just ns -> return ns
  where
    parseAsNodeTypes :: [Aeson.Object] -> Maybe [Node]
    parseAsNodeTypes [] = Just []
    parseAsNodeTypes (x : xs) =
      let current =
            flip parseMaybe x $ \obj -> do
              node_type <- obj .: "type"
              named <- obj .: "named"
              children <- obj .:? "children"
              fields <- obj .:? "fields"
              subtypes <- obj .:? "subtypes"

              case (children, fields, subtypes) of
                (Nothing, Nothing, Nothing) -> return $ Leaf (NodeInfo node_type named)
                (Nothing, Nothing, Just st) -> return $ Interior (NodeInfo node_type named) Nothing st Nothing
                (Just cs, Just fs, Just st) -> return $ Interior (NodeInfo node_type named) fs st cs
                (Just cs, Just fs, Nothing) -> return $ Interior (NodeInfo node_type named) fs Nothing cs
                (Just cs, Nothing, Just st) -> return $ Interior (NodeInfo node_type named) Nothing st cs
                (Just cs, Nothing, Nothing) -> return $ Interior (NodeInfo node_type named) Nothing Nothing cs
                (Nothing, Just fs, Just st) -> return $ Interior (NodeInfo node_type named) fs st Nothing
                (Nothing, Just fs, Nothing) -> return $ Interior (NodeInfo node_type named) fs Nothing Nothing
          rest = parseAsNodeTypes xs
       in case (current, rest) of
            (Nothing, _) -> Nothing
            (_, Nothing) -> Nothing
            (Just current_, Just rest_) -> return $ current_ : rest_
