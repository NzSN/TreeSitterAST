import qualified Data.Text.Lazy as T
import ProgBuilder.ProgBuilderDescription

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

  -- Check if merged
  case out of
    [branch0, branch1] -> do
      putStrLn $ "\nBranch 0: " ++ show branch0
      putStrLn $ "Branch 1: " ++ show branch1
      let fieldCount0 = length [() | NamedProp n _ <- branch0, n == T.pack "field"]
          fieldCount1 = length [() | NamedProp n _ <- branch1, n == T.pack "field"]
      putStrLn $ "\nField count in branch 0: " ++ show fieldCount0
      putStrLn $ "Field count in branch 1: " ++ show fieldCount1
      if fieldCount0 == 1 && fieldCount1 == 0
        then putStrLn "✓ Merged to first branch (minimum index)"
        else if fieldCount0 == 0 && fieldCount1 == 1
          then putStrLn "✗ Merged to second branch (should be first)"
          else putStrLn "✗ Not merged or unexpected distribution"
    _ -> putStrLn "✗ Unexpected output structure"