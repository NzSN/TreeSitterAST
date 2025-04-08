{-# LANGUAGE OverloadedStrings #-}

module TreeSitterNodes
(NodeInfo(..),
 Children(..),
 Node(..),
 parse_node_types,
) where

import GHC.Generics

import Data.Map
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BSL_Char8

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

data NodeInfo =
  NodeInfo { node_type :: String,
             named     :: Bool }
  deriving (Show, Generic)
instance ToJSON NodeInfo
instance FromJSON NodeInfo where
  parseJSON (Object v) =
    NodeInfo
    <$> v .: "type"
    <*> v .: "named"
  parseJSON invalid = prependFailure
    "Parsing NodeInfo failed"
    (typeMismatch "Object" invalid)

data Children =
  Children { required  :: Bool,
              multiple  :: Bool,
              types     :: [NodeInfo] }
  deriving (Show, Generic)
instance ToJSON Children
instance FromJSON Children
data Node =
  Leaf { info :: NodeInfo } |
  Interior { info :: NodeInfo,
             fields :: Map String Children,
             children :: Maybe Children }
  deriving (Show, Generic)
instance ToJSON Node
instance FromJSON Node

parse_node_types :: String -> MaybeT IO [Node]
parse_node_types path = do
  content <- lift $ readFile path

  let json = Aeson.decode (BSL_Char8.pack content) :: Maybe [Aeson.Object]

  case json of
    Nothing  -> MaybeT $ return Nothing
    Just obj -> case parseAsNodeTypes obj of
      Nothing -> MaybeT $ return Nothing
      Just ns -> return ns
  where
    parseAsNodeTypes :: [Aeson.Object] -> Maybe [Node]
    parseAsNodeTypes [] = Just []
    parseAsNodeTypes (x:xs) =
      let current =
            flip parseMaybe x $ \obj -> do
                node_type <- obj .: "type"
                named     <- obj .: "named"
                children  <- obj .:? "children"
                fields    <- obj .:? "fields"

                case (children,fields) of
                  (Nothing, Nothing)  -> return $ Leaf (NodeInfo node_type named)
                  (Just cs, Just fs)  -> return $ Interior (NodeInfo node_type named) fs cs
                  (Just cs, Nothing) -> return $ Interior (NodeInfo node_type named) empty cs
                  (Nothing, Just fs) -> return $ Interior (NodeInfo node_type named) fs Nothing
          rest = parseAsNodeTypes xs
      in if (isNothing current) || (isNothing rest)
         then Nothing
         else return $ [fromJust current] ++ (fromJust rest)
