module Main where

import Data.Maybe
import System.Environment (getArgs)
import Control.Monad.Trans.Maybe

import System.IO

import qualified TreeSitterNodes as TS
import qualified BackendDescription.NodeDescription as BN
import qualified BackendDescription.NodeProcessorDescription as BP

main :: IO ()
main = do
  (path:_) <- getArgs
  content <- runMaybeT $ TS.parse_node_types path

  if isNothing content
    then putStrLn "Fail to parse node-types.json"
    else do
      withFile "node_processor.ts" WriteMode $
        \h -> hPutStr h $ BP.descript $ fromJust content
      withFile "node_declare.ts" WriteMode $
        \h -> hPutStr h $ BN.descript $ fromJust content
