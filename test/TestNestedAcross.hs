import qualified Data.Text.Lazy as T
import ProgBuilder.ProgBuilderDescription

main :: IO ()
main = do
  let p1 = SymbolProp (T.pack "t1")
      p2 = SymbolProp (T.pack "t2")
      -- Nested NamedProps in different branches
      inner1 = NamedProp (T.pack "inner") [p1]  -- branch 0
      inner2 = NamedProp (T.pack "inner") [p2]  -- branch 1
      outer1 = NamedProp (T.pack "outer") [inner1]
      outer2 = NamedProp (T.pack "outer") [inner2]
      br = [[outer1], [outer2]]
      out = convergeNamedProp br
  putStrLn "Test nested NamedProps across branches:"
  putStrLn $ "Input: " ++ show br
  putStrLn $ "Output: " ++ show out

  -- Check if outer merged across branches
  case out of
    [branch0, branch1] -> do
      putStrLn $ "\nBranch 0: " ++ show branch0
      putStrLn $ "Branch 1: " ++ show branch1
      let outerCount0 = length [() | NamedProp n _ <- branch0, n == T.pack "outer"]
          outerCount1 = length [() | NamedProp n _ <- branch1, n == T.pack "outer"]
      putStrLn $ "\nOuter count in branch 0: " ++ show outerCount0
      putStrLn $ "Outer count in branch 1: " ++ show outerCount1
      if outerCount0 == 1 && outerCount1 == 0
        then putStrLn "✓ Outer merged to first branch"
        else putStrLn "✗ Outer not merged as expected"
      -- Check inner types inside outer
      case branch0 of
        [NamedProp _ types] -> do
          putStrLn $ "Outer types: " ++ show types
          let innerCount = length [() | NamedProp n _ <- types, n == T.pack "inner"]
          putStrLn $ "Inner count inside outer: " ++ show innerCount
          if innerCount == 1
            then putStrLn "✓ Inner merged within outer (should be merged within branch)"
            else putStrLn "✗ Inner not merged"
        _ -> putStrLn "✗ Outer structure unexpected"
    _ -> putStrLn "✗ Unexpected output structure"