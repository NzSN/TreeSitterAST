import qualified TreeSitterGrammarNodes as TSGN
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map (keys)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        (f:_) -> f
        _ -> "sample/grammar.json"
  content <- BS.readFile file
  case TSGN.parseGrammarFromJSON content of
    Nothing -> putStrLn "Failed to parse"
    Just grammar -> do
      putStrLn $ "Grammar name: " ++ show (TSGN.grammarName grammar)
      putStrLn $ "Number of rules: " ++ show (length $ keys $ TSGN.grammarNodes grammar)
      putStrLn $ "Rule keys: " ++ show (keys $ TSGN.grammarNodes grammar)
      putStrLn $ "Externals: " ++ show (TSGN.grammarExternals grammar)
      putStrLn $ "Inline: " ++ show (TSGN.grammarInline grammar)
      putStrLn $ "Supertypes: " ++ show (TSGN.grammarSupertypes grammar)
      putStrLn $ "Reserved keys: " ++ show (fmap (keys) (TSGN.grammarReserved grammar))