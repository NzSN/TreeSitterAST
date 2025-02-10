{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies #-}

module MyLib (
  decode_node_types,
  NodeDesc(..),
  ChildrenNodes(..),
  NodeTypes(..)) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BSL_Char8
import qualified Data.Aeson as Aeson
import Data.Aeson.Types


data NodeDesc = ND {
  node_type :: String,
  named     :: Bool }

data ChildrenNodes = NC {
  multipled :: Bool,
  required  :: Bool,
  types     :: [NodeDesc] } | NC_Empty

data NodeTypes = NT {
  info      :: NodeDesc,
  children  :: ChildrenNodes }


decode_node_types :: String -> MaybeT IO [NodeTypes]
decode_node_types path = do
  content <- lift $ readFile path

  let json = Aeson.decode (BSL_Char8.pack content) :: Maybe [Aeson.Object]

  case json of
    Nothing  -> MaybeT $ return Nothing
    Just obj -> return $ fromJust $ parseAsNodeTypes obj

  where
    parseAsNodeTypes :: [Aeson.Object] -> Maybe [NodeTypes]
    parseAsNodeTypes [] = Just []
    parseAsNodeTypes (x:xs) =
      let current =
            flip parseMaybe x $ \obj -> do
                node_type <- obj .: ("type" :: Key)
                named     <- obj .: "named"

                Just $ NT (ND node_type True) NC_Empty
          rest = parseAsNodeTypes xs
      in if (isNothing current) || (isNothing rest)
         then Nothing
         else [fromJust current] ++ (fromJust rest)
