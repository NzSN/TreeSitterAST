module Main where

import Args (Mode(..), parseArgs)
import Control.Monad.Trans.Maybe
import qualified TreeSitterNodes as TS
import qualified TypedASTGenerator.NodeDescription as BN
import qualified TypedASTGenerator.NodeProcessorDescription as BP

main :: IO ()
main = do
  (mode, path) <- parseArgs
  ns <- runMaybeT $ TS.parse_node_types path

  maybe reportError (transform mode) ns
  where
    reportError = error "Fail to parse node_typs.json or grammar.json"

    transform :: Mode -> [TS.Node] -> IO ()
    transform mode = case mode of
      AstProc -> astGen
      CodeGen -> progBuilderGen

astGen :: [TS.Node] -> IO ()
astGen ns =
  writeFile "node_declare.ts" (BN.descript ns) >>
  writeFile "node_processor.ts" (BP.descript ns)

progBuilderGen :: [TS.Node] -> IO ()
progBuilderGen _ =
  putStrLn "Code generation not yet implemented"
