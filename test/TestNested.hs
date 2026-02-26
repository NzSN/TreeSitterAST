import qualified Data.Text.Lazy as T
import ProgBuilder.ProgBuilderDescription (convergeNamedProp, Property(..))

main :: IO ()
main = do
  let p1 = SymbolProp (T.pack "t1")
      p2 = SymbolProp (T.pack "t2")
      inner1 = NamedProp (T.pack "inner") [p1]
      inner2 = NamedProp (T.pack "inner") [p2]
      outer1 = NamedProp (T.pack "outer") [inner1]
      outer2 = NamedProp (T.pack "outer") [inner2]
      br = [[outer1], [outer2]]
      out = convergeNamedProp br
  putStrLn "Test nested NamedProps across branches:"
  putStrLn $ "Input: " ++ show br
  putStrLn $ "Output: " ++ show out
  case out of
    [branch0, branch1] -> do
      putStrLn $ "\nBranch 0: " ++ show branch0
      putStrLn $ "Branch 1: " ++ show branch1
      case branch0 of
        [NamedProp _ types] -> do
          putStrLn $ "Outer types: " ++ show types
          case types of
            [NamedProp _ innerTypes] -> do
              putStrLn $ "Inner types: " ++ show innerTypes
              putStrLn $ "Expected inner types: [SymbolProp \"t1\", SymbolProp \"t2\"]"
            _ -> putStrLn "Inner not a single NamedProp"
        _ -> putStrLn "No outer NamedProp"
    _ -> putStrLn "Unexpected output structure"
