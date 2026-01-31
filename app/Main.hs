{-# LANGUAGE LambdaCase #-}

module Main where

import Args (Mode (..), parseArgs)
import Control.Monad.Trans.Maybe
import ProgBuilder.ECMA.ProgBuilderForECMA qualified as PB
import TreeSitterGrammarNodes qualified as TSGN
import TreeSitterNodes qualified as TS
import TypedASTGenerator.NodeDescription qualified as BN
import TypedASTGenerator.NodeProcessorDescription qualified as BP

main :: IO ()
main =
  parseArgs >>= uncurry transform
  where
    transform :: Mode -> String -> IO ()
    transform mode = case mode of
      AstProc -> astGen
      CodeGen -> progBuilderGen

    astGen :: String -> IO ()
    astGen path =
      runMaybeT (TS.parse_node_types path)
        >>= \case
          Nothing -> reportError
          Just ns -> generate ns
      where
        generate :: [TS.Node] -> IO ()
        generate ns =
          writeFile "node_declare.ts" (BN.descript ns)
            >> writeFile "node_processor.ts" (BP.descript ns)

    progBuilderGen :: String -> IO ()
    progBuilderGen path =
      runMaybeT (TSGN.parseGrammarFromFile path)
        >>= \case
          Nothing -> reportError
          Just grammar ->
            writeFile "grammar_nodes.txt" (show grammar)
              >> writeFile "grammar_classes.ts" (PB.descript grammar)

    reportError = error "Fail to parse node_typs.json or grammar.json"
