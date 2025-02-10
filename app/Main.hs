module Main where

import System.Environment (getArgs)
import Control.Monad.Trans.Maybe

import qualified MyLib (decode_node_types)

main :: IO ()
main = do
  (path:_) <- getArgs

  content <- runMaybeT $ MyLib.decode_node_types path

  putStrLn $ show content
