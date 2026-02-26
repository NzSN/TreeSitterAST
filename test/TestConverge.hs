import qualified Data.Text.Lazy as T
import ProgBuilder.ProgBuilderDescription (convergeNamedProp, Property(..))

main :: IO ()
main = do
  let p1 = SymbolProp (T.pack "t1")
      p2 = SymbolProp (T.pack "t2")
      np1 = NamedProp (T.pack "field") [p1]   -- branch 0
      np2 = NamedProp (T.pack "field") [p2]   -- branch 1
      br = [[np1], [np2]]
      out = convergeNamedProp br
  putStrLn "Test merging NamedProps across branches:"
  putStrLn $ "Input: " ++ show br
  putStrLn $ "Output: " ++ show out
  case out of
    [branch0, branch1] -> do
      putStrLn $ "\nBranch 0: " ++ show branch0
      putStrLn $ "Branch 1: " ++ show branch1
      case branch0 of
        [NamedProp _ types] -> do
          putStrLn $ "Merged types: " ++ show types
          putStrLn $ "Expected: [SymbolProp \"t1\", SymbolProp \"t2\"] (order may vary)"
        _ -> putStrLn "No NamedProp in branch0"
    _ -> putStrLn "Unexpected output structure"
