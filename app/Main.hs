module Main where

import System.Environment (getArgs)
import Control.Monad.Trans.Maybe

import qualified TreeSitterNodes as TS

main :: IO ()
main = do
  (path:_) <- getArgs

  content <- runMaybeT $ TS.parse_node_types path

  putStrLn $ show content
