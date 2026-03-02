{-# LANGUAGE LambdaCase #-}

module Main where

import Args (Mode (..), parseArgs)
import Control.Monad.Trans.Maybe
import Data.Text.Lazy (unpack)
import ProgBuilder.ECMA.ProgBuilderForECMA qualified as PB
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import TreeSitterGrammarNodes qualified as TSGN
import TreeSitterNodes qualified as TS
import TypedASTGenerator.NodeDescription qualified as BN
import TypedASTGenerator.NodeProcessorDescription qualified as BP

main :: IO ()
main =
  parseArgs >>= \(mode, input, outDir) -> transform mode input outDir
  where
    transform :: Mode -> String -> FilePath -> IO ()
    transform mode input outDir = case mode of
      AstProc -> astGen input outDir
      CodeGen -> progBuilderGen input outDir

    astGen :: FilePath -> FilePath -> IO ()
    astGen path outDir =
      createDirectoryIfMissing True outDir
        >> runMaybeT (TS.parse_node_types path)
        >>= \case
          Nothing -> reportError
          Just ns -> generate ns outDir
      where
        generate :: [TS.Node] -> FilePath -> IO ()
        generate ns outDir' =
          let declarePath = outDir' </> "node_declare.ts"
              processorPath = outDir' </> "node_processor.ts"
           in writeFile declarePath (BN.descript ns)
                >> writeFile processorPath (BP.descript ns)

    progBuilderGen :: FilePath -> FilePath -> IO ()
    progBuilderGen path outDir =
      createDirectoryIfMissing True outDir
        >> runMaybeT (TSGN.parseGrammarFromFile path)
        >>= \case
          Nothing -> reportError
          Just grammar ->
            let name = unpack (TSGN.grammarName grammar)
                nodesPath = outDir </> (name ++ "_nodes.txt")
                classesPath = outDir </> (name ++ "_template.ts")
             in writeFile nodesPath (show grammar)
                  >> writeFile classesPath (PB.descript grammar)

    reportError = error "Fail to parse node_typs.json or grammar.json"
