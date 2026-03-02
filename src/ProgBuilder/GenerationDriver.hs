{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Generation driver for orchestrating code generation process.
-- Provides high-level control over generation, statistics collection,
-- and coordination between different generation phases.
module ProgBuilder.GenerationDriver where

import TreeSitterGrammarNodes qualified as TSGN
import Fundamentals.Generation (GenerationStrategy(..), generateSentence)
import Data.Map qualified as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import System.IO (hPutStrLn, stderr)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad (forM_, when)

-- | Statistics collected during generation.
data GenerationStats = GenerationStats
  { totalRules :: Int
  , successfulGenerations :: Int
  , failedGenerations :: Int
  , totalTime :: Double  -- in seconds
  , averageTimePerRule :: Double
  } deriving (Show)

-- | Generation result for a single rule.
data RuleGenerationResult = RuleGenerationResult
  { ruleName :: Text
  , success :: Bool
  , generatedText :: Maybe Text
  , generationTime :: Double  -- in seconds
  } deriving (Show)

-- | Configuration for generation driver.
data GenerationConfig = GenerationConfig
  { strategy :: GenerationStrategy
  , verbose :: Bool
  , maxAttemptsPerRule :: Int
  , collectStats :: Bool
  } deriving (Show)

-- | Default generation configuration.
defaultConfig :: GenerationConfig
defaultConfig = GenerationConfig
  { strategy = RandomStrategy Nothing
  , verbose = False
  , maxAttemptsPerRule = 3
  , collectStats = True
  }

-- | Generate sentences for all rules in grammar.
generateAllRules :: TSGN.Grammar -> GenerationConfig -> IO ([RuleGenerationResult], Maybe GenerationStats)
generateAllRules grammar config = do
  let symbolTable = TSGN.grammarNodes grammar
  let ruleNames = map T.pack $ Map.keys symbolTable

  startTime <- getCurrentTime
  results <- mapM (generateForRule symbolTable config) ruleNames
  endTime <- getCurrentTime

  let totalTimeSec = realToFrac $ diffUTCTime endTime startTime
  let stats = if config.collectStats
        then Just $ calculateStats results totalTimeSec
        else Nothing

  return (results, stats)
  where
    generateForRule :: TSGN.Nodes -> GenerationConfig -> Text -> IO RuleGenerationResult
    generateForRule symbolTable config' ruleName' = do
      start <- getCurrentTime
      result <- generateSentence config'.strategy symbolTable ruleName'
      end <- getCurrentTime
      let timeSec = realToFrac $ diffUTCTime end start

      when (config'.verbose) $ do
        case result of
          Just text -> hPutStrLn stderr $ "Generated for " ++ T.unpack ruleName' ++ ": " ++ T.unpack (T.take 50 text) ++ "..."
          Nothing -> hPutStrLn stderr $ "Failed to generate for " ++ T.unpack ruleName'

      return $ RuleGenerationResult
        { ruleName = ruleName'
        , success = case result of Just _ -> True; Nothing -> False
        , generatedText = result
        , generationTime = timeSec
        }

    calculateStats :: [RuleGenerationResult] -> Double -> GenerationStats
    calculateStats results' totalTime' =
      let total = length results'
          successful = length $ filter (.success) results'
          failed = total - successful
          avgTime = if total > 0 then totalTime' / fromIntegral total else 0
      in GenerationStats
          { totalRules = total
          , successfulGenerations = successful
          , failedGenerations = failed
          , totalTime = totalTime'
          , averageTimePerRule = avgTime
          }

-- | Generate and print statistics.
generateWithStats :: TSGN.Grammar -> GenerationConfig -> IO ()
generateWithStats grammar config = do
  (results, mstats) <- generateAllRules grammar config
  case mstats of
    Just stats -> do
      putStrLn "=== Generation Statistics ==="
      putStrLn $ "Total rules: " ++ show stats.totalRules
      putStrLn $ "Successful: " ++ show stats.successfulGenerations
      putStrLn $ "Failed: " ++ show stats.failedGenerations
      putStrLn $ "Total time: " ++ show stats.totalTime ++ "s"
      putStrLn $ "Average time per rule: " ++ show stats.averageTimePerRule ++ "s"

      when (config.verbose) $ do
        putStrLn "\n=== Failed Rules ==="
        forM_ (filter (not . .success) results) $ \result -> do
          putStrLn $ "  " ++ T.unpack result.ruleName
    Nothing -> return ()

-- | Test different generation strategies.
testStrategies :: TSGN.Grammar -> [GenerationStrategy] -> IO ()
testStrategies grammar strategies = do
  putStrLn "Testing generation strategies..."
  forM_ strategies $ \strategy' -> do
    putStrLn $ "\nStrategy: " ++ show strategy'
    let config = defaultConfig { strategy = strategy', verbose = False }
    (results, mstats) <- generateAllRules grammar config
    case mstats of
      Just stats -> do
        putStrLn $ "  Success rate: " ++ show (stats.successfulGenerations) ++ "/" ++ show (stats.totalRules)
        putStrLn $ "  Total time: " ++ show stats.totalTime ++ "s"
      Nothing -> return ()