module Args
  ( Mode (..),
    parseArgs,
  )
where

import Options.Applicative

data Mode = AstProc | CodeGen deriving (Show)

modeParser :: Parser Mode
modeParser = astProcFlag <|> codeGenFlag
  where
    astProcFlag =
      flag'
        AstProc
        ( long "ast-proc"
            <> help "Generate both node_processor.ts and node_declare.ts"
        )
    codeGenFlag =
      flag'
        CodeGen
        ( long "code-gen"
            <> help "Placeholder for future code generation"
        )

argsParser :: Parser (Mode, FilePath)
argsParser = (,) <$> modeParser <*> strArgument (metavar "FILE" <> help "Path to node-types.json file")

parserInfo :: ParserInfo (Mode, FilePath)
parserInfo =
  info
    (argsParser <**> helper)
    ( fullDesc
        <> progDesc "Generate TypeScript files from Tree-sitter node-types.json"
        <> header "TreeSitterAST - Tree-sitter AST code generator"
    )

parseArgs :: IO (Mode, FilePath)
parseArgs = execParser parserInfo
