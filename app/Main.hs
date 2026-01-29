module Main where

import Data.Maybe
import Control.Monad.Trans.Maybe
import Options.Applicative

import qualified TreeSitterNodes as TS
import qualified BackendDescription.NodeDescription as BN
import qualified BackendDescription.NodeProcessorDescription as BP

data Mode = AstProc | CodeGen deriving (Show)

modeParser :: Parser Mode
modeParser = astProcFlag <|> codeGenFlag
  where
    astProcFlag = flag' AstProc (long "ast-proc" <> help "Generate both node_processor.ts and node_declare.ts")
    codeGenFlag = flag' CodeGen (long "code-gen" <> help "Placeholder for future code generation")

argsParser :: Parser (Mode, FilePath)
argsParser = (,) <$> modeParser <*> strArgument (metavar "FILE" <> help "Path to node-types.json file")

parserInfo :: ParserInfo (Mode, FilePath)
parserInfo = info (argsParser <**> helper)
  ( fullDesc
  <> progDesc "Generate TypeScript files from Tree-sitter node-types.json"
  <> header "TreeSitterAST - Tree-sitter AST code generator" )

parseArgs :: IO (Mode, FilePath)
parseArgs = execParser parserInfo

main :: IO ()
main = do
  (mode, path) <- parseArgs
  content <- runMaybeT $ TS.parse_node_types path

  if isNothing content
    then error "Fail to parse node-types.json"
    else case mode of
      AstProc -> do
        writeFile "node_processor.ts" (BP.descript $ fromJust content)
        writeFile "node_declare.ts" (BN.descript $ fromJust content)
      CodeGen -> putStrLn "Code generation not yet implemented"
