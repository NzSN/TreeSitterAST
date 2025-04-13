module Main where

import Data.Maybe
import System.Environment (getArgs)
import Control.Monad.Trans.Maybe

import qualified TreeSitterNodes as TS
import BackendDescription.NodeDescription as BN

main :: IO ()
main = do
  (path:_) <- getArgs
  content <- runMaybeT $ TS.parse_node_types path

  if isNothing content
    then putStrLn "Fail to parse"
    else putStrLn $ BN.descript $ fromJust content
