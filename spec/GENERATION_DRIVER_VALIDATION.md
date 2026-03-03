# Generation Driver and Validation System Design

## 1. Generation Driver System

### 1.1 Core Driver Architecture

```haskell
-- In ProgBuilder/GenerationDriver.hs
module ProgBuilder.GenerationDriver where

import Fundamentals.Generation
import TreeSitterGrammarNodes qualified as TSGN
import Data.Map qualified as Map
import Control.Monad.State
import System.Random (StdGen, newStdGen, randomR)

-- Generation driver state
data DriverState = DriverState
  { grammar :: TSGN.Grammar
  , context :: GenerationContext
  , generatedCount :: Int
  , errors :: [GenerationError]
  , statistics :: GenerationStatistics
  , randomGenerator :: StdGen
  } deriving (Show)

-- Generation statistics
data GenerationStatistics = GenerationStatistics
  { totalGenerated :: Int
  , validSentences :: Int
  , averageLength :: Double
  , depthDistribution :: Map Int Int
  , choiceDistribution :: Map Text (Map Int Int)
  , timeSpent :: NominalDiffTime
  } deriving (Show)

-- Generation driver monad
type GenerationDriver = StateT DriverState IO

-- Main driver function
driveGeneration :: TSGN.Grammar -> GenerationConfig -> Int -> GenerationDriver [Text]
driveGeneration grammar config targetCount = do
  -- Initialize state
  modify $ \s -> s
    { grammar = grammar
    , context = createInitialContext grammar config
    , generatedCount = 0
    , statistics = emptyStatistics
    }

  -- Generate sentences
  sentences <- generateSentences targetCount

  -- Finalize and report
  finalStats <- gets statistics
  liftIO $ printStatistics finalStats

  return sentences

generateSentences :: Int -> GenerationDriver [Text]
generateSentences targetCount
  | targetCount <= 0 = return []
  | otherwise = do
      sentence <- generateSingleSentence
      remaining <- generateSentences (targetCount - 1)
      return (sentence : remaining)

generateSingleSentence :: GenerationDriver Text
generateSingleSentence = do
  state <- get
  let startRule = getStartRule (grammar state)
      ctx = context state

  -- Generate from start rule
  case evaluateNode ctx startRule of
    Just (sentence, newCtx) -> do
      -- Update statistics
      modify $ \s -> s
        { generatedCount = generatedCount s + 1
        , context = newCtx
        , statistics = updateStatistics (statistics s) sentence
        }
      return sentence

    Nothing -> do
      -- Record error and retry with different strategy
      modify $ \s -> s
        { errors = GenerationFailed startRule : errors s
        , context = retryWithNewStrategy (context s)
        }
      generateSingleSentence  -- Retry
```

### 1.2 Batch Generation with Parallelism

```haskell
parallelGeneration :: TSGN.Grammar -> GenerationConfig -> Int -> Int -> IO [Text]
parallelGeneration grammar config batchSize totalCount = do
  let batches = chunk batchSize totalCount
  results <- mapConcurrently (generateBatch grammar config) batches
  return (concat results)

generateBatch :: TSGN.Grammar -> GenerationConfig -> Int -> IO [Text]
generateBatch grammar config count = do
  seed <- randomIO
  let ctx = createInitialContext grammar config { seed = Just seed }
  evalStateT (driveGeneration grammar config count) (initialState grammar ctx)

-- Smart batching based on complexity
adaptiveBatching :: TSGN.Grammar -> GenerationConfig -> Int -> IO [[Text]]
adaptiveBatching grammar config totalCount = do
  let complexity = estimateGrammarComplexity grammar
      batchSize = calculateOptimalBatchSize complexity config
      numBatches = ceiling (fromIntegral totalCount / fromIntegral batchSize)

  forConcurrently [1..numBatches] $ \batchNum -> do
    let offset = (batchNum - 1) * batchSize
        remaining = totalCount - offset
        currentBatchSize = min batchSize remaining
        batchConfig = config
          { seed = Just (seed config + batchNum) }

    generateBatch grammar batchConfig currentBatchSize
```

### 1.3 Incremental Generation

```haskell
data GenerationStream = GenerationStream
  { streamId :: Text
  , grammar :: TSGN.Grammar
  , config :: GenerationConfig
  , state :: DriverState
  , buffer :: [Text]
  } deriving (Show)

createStream :: TSGN.Grammar -> GenerationConfig -> IO GenerationStream
createStream grammar config = do
  initialSeed <- randomIO
  let ctx = createInitialContext grammar config { seed = Just initialSeed }
      initialState = DriverState grammar ctx 0 [] emptyStatistics (mkStdGen initialSeed)
  return $ GenerationStream
    { streamId = T.pack (show initialSeed)
    , grammar = grammar
    , config = config
    , state = initialState
    , buffer = []
    }

nextFromStream :: GenerationStream -> IO (Text, GenerationStream)
nextFromStream stream = do
  if not (null (buffer stream))
    then
      let (sentence:remaining) = buffer stream
      in return (sentence, stream { buffer = remaining })
    else do
      -- Generate more sentences
      let batchSize = 10  -- Pre-fetch buffer
      newSentences <- generateBatch (grammar stream) (config stream) batchSize
      case newSentences of
        [] -> error "Failed to generate sentences"
        (first:rest) ->
          return (first, stream { buffer = rest })

-- Stream with filtering
filteredStream :: GenerationStream -> (Text -> Bool) -> IO (Maybe Text, GenerationStream)
filteredStream stream predicate = do
  (sentence, newStream) <- nextFromStream stream
  if predicate sentence
    then return (Just sentence, newStream)
    else filteredStream newStream predicate  -- Skip and continue
```

## 2. Validation System

### 2.1 Validation Architecture

```haskell
-- In Validation/Validator.hs
module Validation.Validator where

import TreeSitterGrammarNodes qualified as TSGN
import Data.Text.Lazy (Text)
import System.Process (readProcess)

-- Validation results
data ValidationResult = ValidationResult
  { isValid :: Bool
  , errors :: [ValidationError]
  , warnings :: [ValidationWarning]
  , parseTree :: Maybe Text  -- Optional: parsed AST
  , statistics :: ValidationStatistics
  } deriving (Show)

data ValidationError
  = ParseError Text
  | SyntaxError Text
  | SemanticError Text
  | GrammarMismatch Text
  deriving (Show)

data ValidationWarning
  = PerformanceWarning Text
  | StyleWarning Text
  | ComplexityWarning Text
  deriving (Show)

-- Validator interface
class Validator a where
  validate :: a -> Text -> IO ValidationResult
  quickValidate :: a -> Text -> Bool
  detailedValidate :: a -> Text -> IO (Bool, [ValidationError])

-- Tree-sitter based validator
data TreeSitterValidator = TreeSitterValidator
  { grammarPath :: FilePath
  , parserCommand :: Text
  , timeoutSeconds :: Int
  } deriving (Show)

instance Validator TreeSitterValidator where
  validate validator sentence = do
    -- Parse with tree-sitter
    parseResult <- parseWithTreeSitter validator sentence

    case parseResult of
      Left err -> return $ ValidationResult
        { isValid = False
        , errors = [ParseError err]
        , warnings = []
        , parseTree = Nothing
        , statistics = emptyValidationStats
        }

      Right parseTree -> do
        -- Additional validations
        syntaxErrors <- checkSyntax validator parseTree
        semanticErrors <- checkSemantics validator parseTree
        grammarErrors <- checkGrammarConformance validator parseTree

        let allErrors = syntaxErrors ++ semanticErrors ++ grammarErrors
            warnings = generateWarnings parseTree

        return $ ValidationResult
          { isValid = null allErrors
          , errors = allErrors
          , warnings = warnings
          , parseTree = Just parseTree
          , statistics = calculateStats parseTree
          }

  quickValidate validator sentence = do
    result <- parseWithTreeSitter validator sentence
    case result of
      Left _ -> return False
      Right _ -> return True
```

### 2.2 Multi-Level Validation

```haskell
-- Layered validation
data ValidationLevel
  = SyntaxOnly
  | SyntaxAndSemantics
  | FullValidation
  deriving (Show, Eq)

layeredValidation :: TreeSitterValidator -> ValidationLevel -> Text -> IO ValidationResult
layeredValidation validator level sentence = do
  -- Level 1: Basic parse
  parseResult <- parseWithTreeSitter validator sentence
  case parseResult of
    Left err -> return $ simpleInvalidResult [ParseError err]
    Right parseTree -> do
      case level of
        SyntaxOnly -> return $ ValidationResult
          { isValid = True
          , errors = []
          , warnings = []
          , parseTree = Just parseTree
          , statistics = basicStats parseTree
          }

        SyntaxAndSemantics -> do
          semanticErrors <- checkSemantics validator parseTree
          return $ ValidationResult
            { isValid = null semanticErrors
            , errors = semanticErrors
            , warnings = []
            , parseTree = Just parseTree
            , statistics = basicStats parseTree
            }

        FullValidation -> do
          semanticErrors <- checkSemantics validator parseTree
          styleWarnings <- checkStyle parseTree
          perfWarnings <- checkPerformance parseTree
          grammarErrors <- checkGrammarConformance validator parseTree

          let allErrors = semanticErrors ++ grammarErrors
              allWarnings = styleWarnings ++ perfWarnings

          return $ ValidationResult
            { isValid = null allErrors
            , errors = allErrors
            , warnings = allWarnings
            , parseTree = Just parseTree
            , statistics = detailedStats parseTree
            }
```

### 2.3 Property-Based Validation

```haskell
-- Property-based testing for generated code
data ValidationProperty = ValidationProperty
  { propertyName :: Text
  , testFunction :: Text -> Bool
  , description :: Text
  } deriving (Show)

-- Common validation properties
commonProperties :: [ValidationProperty]
commonProperties =
  [ ValidationProperty
      { propertyName = "parses_correctly"
      , testFunction = \s -> length s > 0  -- Simplified
      , description = "Generated sentence parses without errors"
      }
  , ValidationProperty
      { propertyName = "no_empty_blocks"
      , testFunction = not . T.isInfixOf "{}"
      , description = "No empty code blocks"
      }
  , ValidationProperty
      { propertyName = "balanced_parentheses"
      , testFunction = hasBalancedParentheses
      , description = "Parentheses are properly balanced"
      }
  , ValidationProperty
      { propertyName = "valid_identifiers"
      , testFunction = hasValidIdentifiers
      , description = "All identifiers follow language rules"
      }
  ]

-- Property test runner
runPropertyTests :: [ValidationProperty] -> [Text] -> PropertyTestResults
runPropertyTests properties sentences =
  let results = map (testPropertyOnSentences properties) sentences
      summary = summarizeResults results
  in PropertyTestResults
    { totalTests = length sentences * length properties
    , passedTests = countPassed results
    , failedTests = countFailed results
    , failingProperties = findFailingProperties results
    , worstSentences = findWorstSentences results
    }
```

## 3. Integration with Generation Driver

### 3.1 Validated Generation

```haskell
-- Generation with built-in validation
validatedGeneration :: TSGN.Grammar -> GenerationConfig -> Validator -> Int
                   -> IO [(Text, ValidationResult)]
validatedGeneration grammar config validator count = do
  sentences <- parallelGeneration grammar config count 10
  results <- mapConcurrently (validateSentence validator) sentences
  return (zip sentences results)

-- Filter to only valid sentences
generateValidSentences :: TSGN.Grammar -> GenerationConfig -> Validator -> Int
                      -> IO [Text]
generateValidSentences grammar config validator targetCount = do
  let batchSize = 50
      attemptsLimit = targetCount * 3  -- Allow some failures

  go batchSize attemptsLimit []
  where
    go :: Int -> Int -> [Text] -> IO [Text]
    go _ 0 acc = return (reverse acc)  -- Out of attempts
    go _ _ acc | length acc >= targetCount = return (reverse acc)

    go batchSize remaining acc = do
      -- Generate a batch
      batch <- generateBatch grammar config batchSize

      -- Validate in parallel
      validationResults <- mapConcurrently (quickValidate validator) batch

      -- Keep valid sentences
      let validSentences = map fst $ filter snd $ zip batch validationResults
          newAcc = validSentences ++ acc
          validCount = length newAcc
          needed = targetCount - validCount

      if needed <= 0
        then return (reverse newAcc)
        else do
          let nextBatchSize = min batchSize needed
          go nextBatchSize (remaining - batchSize) newAcc
```

### 3.2 Feedback-Driven Generation

```haskell
-- Generation that learns from validation feedback
data FeedbackState = FeedbackState
  { successfulPatterns :: [GrammarPattern]
  , failedPatterns :: [GrammarPattern]
  , adjustmentFactors :: Map Text Double
  } deriving (Show)

feedbackDrivenGeneration :: TSGN.Grammar -> GenerationConfig -> Validator
                        -> FeedbackState -> Int -> IO ([Text], FeedbackState)
feedbackDrivenGeneration grammar config validator feedbackState count = do
  (sentences, newFeedback) <- foldM generateOne ([], feedbackState) [1..count]

  return (reverse sentences, newFeedback)
  where
    generateOne :: ([Text], FeedbackState) -> Int
                -> IO ([Text], FeedbackState)
    generateOne (acc, state) _ = do
      -- Adjust generation based on feedback
      let adjustedConfig = applyFeedback config state

      -- Generate sentence
      sentence <- generateSingle grammar adjustedConfig

      -- Validate
      result <- quickValidate validator sentence

      -- Update feedback state
      let patterns = extractPatterns sentence
          newState = if result
            then state
              { successfulPatterns = patterns ++ successfulPatterns state
              , adjustmentFactors = reinforceFactors patterns (adjustmentFactors state)
              }
            else state
              { failedPatterns = patterns ++ failedPatterns state
              , adjustmentFactors = penalizeFactors patterns (adjustmentFactors state)
              }

      return (sentence : acc, newState)
```

## 4. Statistical Analysis and Reporting

### 4.1 Generation Statistics

```haskell
data ComprehensiveStatistics = ComprehensiveStatistics
  { generationStats :: GenerationStatistics
  , validationStats :: ValidationStatistics
  , performanceStats :: PerformanceStatistics
  , qualityMetrics :: QualityMetrics
  } deriving (Show)

collectStatistics :: [Text] -> [ValidationResult] -> ComprehensiveStatistics
collectStatistics sentences validationResults =
  ComprehensiveStatistics
    { generationStats = analyzeGeneration sentences
    , validationStats = analyzeValidation validationResults
    , performanceStats = analyzePerformance sentences
    , qualityMetrics = calculateQualityMetrics sentences validationResults
    }

-- Quality metrics
data QualityMetrics = QualityMetrics
  { validityRate :: Double        -- Percentage of valid sentences
  , uniquenessRate :: Double      -- Percentage of unique sentences
  , coverageScore :: Double       -- Grammar coverage percentage
  , complexityScore :: Double     -- Average complexity
  , diversityIndex :: Double      -- Measure of diversity
  } deriving (Show)

calculateQualityMetrics :: [Text] -> [ValidationResult] -> QualityMetrics
calculateQualityMetrics sentences results =
  let total = length sentences
      validCount = length (filter isValid results)
      uniqueCount = length (nub sentences)
      coverage = calculateGrammarCoverage sentences
      avgComplexity = average (map estimateComplexity sentences)
      diversity = calculateDiversityIndex sentences
  in QualityMetrics
    { validityRate = fromIntegral validCount / fromIntegral total * 100
    , uniquenessRate = fromIntegral uniqueCount / fromIntegral total * 100
    , coverageScore = coverage
    , complexityScore = avgComplexity
    , diversityIndex = diversity
    }
```

### 4.2 Report Generation

```haskell
generateReport :: ComprehensiveStatistics -> Text
generateReport stats = T.unlines
  [ "=== Generation Report ==="
  , ""
  , "Summary:"
  , T.concat ["  Total generated: ", T.pack (show (totalGenerated (generationStats stats)))]
  , T.concat ["  Valid sentences: ", T.pack (show (validSentences (validationStats stats)))]
  , T.concat ["  Validity rate: ", T.pack (show (validityRate (qualityMetrics stats))), "%"]
  , ""
  , "Quality Metrics:"
  , T.concat ["  Uniqueness: ", T.pack (show (uniquenessRate (qualityMetrics stats))), "%"]
  , T.concat ["  Grammar coverage: ", T.pack (show (coverageScore (qualityMetrics stats))), "%"]
  , T.concat ["  Average complexity: ", T.pack (show (complexityScore (qualityMetrics stats)))]
  , T.concat ["  Diversity index: ", T.pack (show (diversityIndex (qualityMetrics stats)))]
  , ""
  , "Performance:"
  , T.concat ["  Time spent: ", T.pack (show (timeSpent (performanceStats stats))), "s"]
  , T.concat ["  Sentences per second: ", T.pack (show (sentencesPerSecond (performanceStats stats)))]
  , ""
  , "Detailed Statistics:"
  , T.pack (show (depthDistribution (generationStats stats)))
  ]

-- HTML report
generateHTMLReport :: ComprehensiveStatistics -> Text
generateHTMLReport stats = T.unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "  <title>Generation Report</title>"
  , "  <style>"
  , "    body { font-family: Arial, sans-serif; margin: 40px; }"
  , "    .metric { margin: 10px 0; padding: 10px; background: #f5f5f5; }"
  , "    .good { color: green; }"
  , "    .warning { color: orange; }"
  , "    .bad { color: red; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <h1>Generation Report</h1>"
  , "  <div class='metric'>"
  , T.concat ["    <h2>Validity Rate: <span class='", validityClass, "'>"]
  , T.concat [T.pack (show (validityRate (qualityMetrics stats))), "%</span></h2>"]
  , "  </div>"
  -- ... more metrics
  , "</body>"
  , "</html>"
  ]
  where
    validityClass
      | validityRate (qualityMetrics stats) > 90 = "good"
      | validityRate (qualityMetrics stats) > 70 = "warning"
      | otherwise = "bad"
```

## 5. Configuration Management

### 5.1 Configuration Files

```yaml
# generation-config.yaml
generation:
  strategy: "guided"
  max_depth: 10
  max_repetitions: 5
  prefer_short: true

validation:
  level: "full"
  timeout_seconds: 30
  fail_fast: false

output:
  format: "both"  # text, html, or both
  directory: "./output"
  report_name: "generation_report"

feedback:
  enabled: true
  learning_rate: 0.1
  history_size: 1000
```

### 5.2 Configuration Loading

```haskell
loadConfiguration :: FilePath -> IO GenerationConfig
loadConfiguration configFile = do
  content <- readFile configFile
  case decodeEither' content of
    Left err -> error $ "Failed to parse config: " ++ show err
    Right config -> return config

-- Dynamic configuration updates
updateConfiguration :: GenerationConfig -> ValidationResults -> GenerationConfig
updateConfiguration config results =
  let validityRate = calculateValidityRate results
      -- Adjust based on performance
      newMaxDepth = if validityRate < 0.7
                    then maxDepth config - 1
                    else maxDepth config
      -- Adjust strategy
      newStrategy = if validityRate < 0.5
                    then GuidedStrategy [PreferShort True]
                    else strategy config
  in config
    { maxDepth = newMaxDepth
    , strategy = newStrategy
    }
```

## 6. Error Handling and Recovery

### 6.1 Error Types

```haskell
data GenerationError
  = GrammarError Text
  | StrategyError Text
  | ResourceError Text
  | ValidationError Text
  | TimeoutError
  deriving (Show)

data RecoveryAction
  = RetryWithNewSeed
  | ReduceComplexity
  | SwitchStrategy
  | SkipAndContinue
  | AbortGeneration
  deriving (Show)

handleGenerationError :: GenerationError -> DriverState -> IO (RecoveryAction, DriverState)
handleGenerationError err state =
  case err of
    GrammarError msg -> do
      putStrLn $ "Grammar error: " ++ T.unpack msg
      return (SkipAndContinue, state)

    StrategyError msg -> do
      putStrLn $ "Strategy error: " ++ T.unpack msg
      let newStrategy = fallbackStrategy (context state)
          newCtx = (context state) { strategy = newStrategy }
      return (RetryWithNewSeed, state { context = newCtx })

    ResourceError msg -> do
      putStrLn $ "Resource error: " ++ T.unpack msg
      -- Wait and retry
      threadDelay 1000000  -- 1 second
      return (RetryWithNewSeed, state)

    ValidationError msg -> do
      putStrLn $ "Validation error: " ++ T.unpack msg
      return (SkipAndContinue, state)

    TimeoutError -> do
      putStrLn "Generation timeout"
      let reducedCtx = reduceComplexity (context state)
      return (ReduceComplexity, state { context = reducedCtx })
```

### 6.2 Resilient Generation

```haskell
resilientGeneration :: TSGN.Grammar -> GenerationConfig -> Int -> IO [Text]
resilientGeneration grammar config targetCount =
  go 0 [] (createInitialState grammar config)
  where
    maxAttempts = targetCount * 5
    go :: Int -> [Text] -> DriverState -> IO [Text]
    go attempts acc state
      | length acc >= targetCount = return (reverse acc)
      | attempts >= maxAttempts = do
          putStrLn $ "Warning: Only generated " ++ show (length acc) ++ " out of " ++ show targetCount
          return (reverse acc)

    go attempts acc state = do
      result <- try $ generateSingleSentenceDriver state

      case result of
        Left (SomeException e) -> do
          putStrLn $ "Exception during generation: " ++ show e
          let recovery = determineRecovery e
          (action, newState) <- handleRecovery recovery state
          case action of
            AbortGeneration -> return (reverse acc)
            _ -> go (attempts + 1) acc newState

        Right (sentence, newState) -> do
          -- Quick validation
          isValid <- quickValidate defaultValidator sentence
          if isValid
            then go (attempts + 1) (sentence : acc) newState
            else go (attempts + 1) acc newState  -- Skip invalid
```

## 7. Testing the Driver and Validation

### 7.1 Unit Tests

```haskell
testGenerationDriver :: TestTree
testGenerationDriver = testGroup "Generation Driver"
  [ testCase "Basic generation" $ do
      let grammar = sampleGrammar
          config = defaultConfig
          targetCount = 10
      sentences <- driveGeneration grammar config targetCount
      assertEqual "Should generate requested count" targetCount (length sentences)

  , testCase "Validation integration" $ do
      let grammar = sampleGrammar
          config = defaultConfig
          validator = defaultValidator
      results <- validatedGeneration grammar config validator 5
      let validCount = length (filter (isValid . snd) results)
      assertBool "Should have some valid sentences" (validCount > 0)

  , testCase "Error recovery" $ do
      let grammar = problematicGrammar
          config = defaultConfig
      sentences <- resilientGeneration grammar config 3
      assertBool "Should handle errors gracefully" (length sentences <= 3)
  ]
```

### 7.2 Property Tests

```haskell
prop_generation_completeness :: Property
prop_generation_completeness =
  forAll (choose (1, 100)) $ \count ->
    forAll genGrammar $ \grammar ->
      ioProperty $ do
        sentences <- driveGeneration grammar defaultConfig count
        return $ length sentences == count

prop_validation_soundness :: Property
prop_validation_soundness =
  forAll genSentence $ \sentence ->
    ioProperty $ do
      result <- quickValidate defaultValidator sentence
      -- If validator says valid, it should parse
      if result
        then do
          parseResult <- parseWithTreeSitter defaultValidator sentence
          return $ isRight parseResult
        else return True  -- Invalid is always sound
```

This comprehensive design for the generation driver and validation system provides robust, configurable, and testable infrastructure for grammar-based code generation with built-in quality assurance.