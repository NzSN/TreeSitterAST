# Choice Resolution and Repetition Handling Design

## 1. Choice Resolution System

### 1.1 Choice Node Representation

```haskell
-- In Fundamentals/Generation.hs
data ChoiceNode = ChoiceNode
  { alternatives :: [GrammarNode Text]
  , choiceId :: Text
  , ruleName :: Text
  , metadata :: ChoiceMetadata
  } deriving (Show, Eq)

data ChoiceMetadata = ChoiceMetadata
  { isTopLevel :: Bool
  , depth :: Int
  , parentRule :: Maybe Text
  , constraints :: [ChoiceConstraint]
  } deriving (Show, Eq)

data ChoiceConstraint
  = MustSelect Int                    -- Must select specific alternative
  | AvoidSelect Int                   -- Avoid specific alternative
  | PreferType Text                   -- Prefer alternatives containing type
  | AvoidType Text                    -- Avoid alternatives containing type
  | MaxDepth Int                      -- Maximum recursion depth
  | ProbabilityDistribution [Double]  -- Custom probability distribution
  deriving (Show, Eq)
```

### 1.2 Choice Resolution Strategies

#### Random Selection
```haskell
randomSelection :: Maybe Int -> [GrammarNode Text] -> Maybe (Int, GenerationContext)
randomSelection seed alternatives = do
  let gen = maybe newStdGen mkStdGen seed
      (idx, gen') = randomR (0, length alternatives - 1) gen
  return (idx, ctx { randomGen = Just gen' })
```

#### Guided Selection
```haskell
guidedSelection :: [ChoiceConstraint] -> [GrammarNode Text] -> Maybe (Int, GenerationContext)
guidedSelection constraints alternatives =
  let scored = zip [0..] $ map (scoreAlternative constraints) alternatives
      filtered = filter ((> 0) . snd) scored
      sorted = sortBy (comparing (negate . snd)) filtered
  in case sorted of
      [] -> Nothing  -- No valid alternatives
      ((idx, _):_) -> Just (idx, ctx)
  where
    scoreAlternative :: [ChoiceConstraint] -> GrammarNode Text -> Double
    scoreAlternative constrs alt =
      foldl' (\score constr -> score * constraintWeight constr alt) 1.0 constrs

    constraintWeight :: ChoiceConstraint -> GrammarNode Text -> Double
    constraintWeight (MustSelect targetIdx) alt
      | idx == targetIdx = 1.0
      | otherwise = 0.0
    constraintWeight (AvoidSelect targetIdx) alt
      | idx == targetIdx = 0.0
      | otherwise = 1.0
    constraintWeight (PreferType typ) alt =
      if containsType typ alt then 1.5 else 1.0
    constraintWeight (AvoidType typ) alt =
      if containsType typ alt then 0.5 else 1.0
    constraintWeight _ _ = 1.0
```

#### Exhaustive Selection
```haskell
exhaustiveSelection :: GenerationContext -> [GrammarNode Text] -> Maybe (Int, GenerationContext)
exhaustiveSelection ctx alternatives =
  let history = choiceHistory ctx
      -- Find next unexplored alternative
      unexplored = findUnexploredAlternative history alternatives
  in case unexplored of
      Just idx -> Just (idx, ctx { choiceHistory = recordChoice ctx idx })
      Nothing -> Nothing  -- All alternatives explored
  where
    findUnexploredAlternative :: [ChoiceDecision] -> [GrammarNode Text] -> Maybe Int
    findUnexploredAlternative history alts =
      find (\idx -> not $ alreadyExplored history idx) [0..length alts - 1]

    alreadyExplored :: [ChoiceDecision] -> Int -> Bool
    alreadyExplored history idx =
      any (\dec -> alternativeIndex dec == idx) history
```

### 1.3 Smart Choice Resolution

#### Context-Aware Selection
```haskell
contextAwareSelection :: GenerationContext -> [GrammarNode Text] -> Maybe (Int, GenerationContext)
contextAwareSelection ctx alternatives =
  let -- Analyze context
      currentDepth = depth ctx
      parentType = getParentNodeType ctx
      siblingTypes = getSiblingTypes ctx

      -- Score each alternative based on context
      scores = zipWith (scoreAlternative ctx) [0..] alternatives

      -- Apply strategy
      selectedIdx = case strategy ctx of
        RandomStrategy seed -> selectRandomWeighted scores seed
        GuidedStrategy constrs -> selectGuided scores constrs
        ExhaustiveStrategy _ -> selectExhaustive scores (choiceHistory ctx)

  in selectedIdx
  where
    scoreAlternative :: GenerationContext -> Int -> GrammarNode Text -> Double
    scoreAlternative ctx idx alt =
      let baseScore = 1.0
          depthScore = scoreDepth ctx alt
          typeScore = scoreTypeCompatibility ctx alt
          complexityScore = scoreComplexity alt
      in baseScore * depthScore * typeScore * complexityScore

    scoreDepth :: GenerationContext -> GrammarNode Text -> Double
    scoreDepth ctx alt
      | depth ctx >= maxDepth ctx = scoreShallow alt
      | otherwise = 1.0

    scoreShallow :: GrammarNode Text -> Double
    scoreShallow alt = 1.0 / (1.0 + fromIntegral (estimateDepth alt))
```

#### Probability-Based Selection
```haskell
probabilityBasedSelection :: ProbabilityDistribution -> [GrammarNode Text] -> Maybe Int
probabilityBasedSelection (ProbabilityDistribution probs) alternatives
  | length probs == length alternatives = do
      let total = sum probs
          normalized = map (/ total) probs
          cumProbs = scanl1 (+) normalized
          r = randomDouble
          idx = length (takeWhile (<= r) cumProbs)
      Just idx
  | otherwise = Nothing
```

### 1.4 Choice History and Backtracking

```haskell
data ChoiceDecision = ChoiceDecision
  { ruleName :: Text
  , choiceId :: Text
  , alternativeIndex :: Int
  , alternativeContent :: GrammarNode Text
  , timestamp :: UTCTime
  } deriving (Show, Eq)

data BacktrackPoint = BacktrackPoint
  { context :: GenerationContext
  , remainingAlternatives :: [Int]
  , decisionPath :: [ChoiceDecision]
  } deriving (Show)

backtrack :: [BacktrackPoint] -> Maybe (GenerationContext, [BacktrackPoint])
backtrack [] = Nothing
backtrack (point:rest) =
  case remainingAlternatives point of
    [] -> backtrack rest  -- No more alternatives at this point
    (nextIdx:remaining) ->
      let newDecision = ChoiceDecision
            { ruleName = currentRule point.context
            , choiceId = "choice_" <> T.pack (show (length point.decisionPath))
            , alternativeIndex = nextIdx
            , alternativeContent = getAlternative nextIdx
            , timestamp = getCurrentTime
            }
          newContext = point.context
            { choiceHistory = newDecision : point.decisionPath }
          newPoint = point { remainingAlternatives = remaining }
      in Just (newContext, newPoint : rest)
```

## 2. Repetition Handling System

### 2.1 Repetition Node Representation

```haskell
data RepetitionNode = RepetitionNode
  { content :: GrammarNode Text
  , minCount :: Int
  , maxCount :: Int
  , separator :: Maybe Text
  , isRepeat1 :: Bool  -- True for REPEAT1 (1 or more)
  } deriving (Show, Eq)
```

### 2.2 Count Determination Strategies

#### Random Count Selection
```haskell
randomCountSelection :: GenerationContext -> RepetitionNode -> Maybe (Int, GenerationContext)
randomCountSelection ctx repNode = do
  let (minC, maxC) = (minCount repNode, maxCount repNode)
      count = if minC == maxC
              then minC
              else minC + randomR (0, maxC - minC) (randomGen ctx)
  return (count, ctx)
```

#### Guided Count Selection
```haskell
guidedCountSelection :: GenerationContext -> RepetitionNode -> Maybe (Int, GenerationContext)
guidedCountSelection ctx repNode =
  let constraints = getConstraints ctx
      preferredCount = estimateOptimalCount constraints repNode
      boundedCount = clamp (minCount repNode) (maxCount repNode) preferredCount
  in Just (boundedCount, ctx)
  where
    estimateOptimalCount :: [GenerationConstraint] -> RepetitionNode -> Int
    estimateOptimalCount constrs node =
      foldl' applyConstraint (minCount node) constrs

    applyConstraint :: Int -> GenerationConstraint -> Int
    applyConstraint current (MaxLength len) =
      min current (len `div` estimateItemLength node)
    applyConstraint current (PreferShort _) =
      min current (minCount node + 1)
    applyConstraint current (PreferLong _) =
      max current (maxCount node)
    applyConstraint current _ = current
```

#### Exponential Backoff for Deep Recursion
```haskell
exponentialBackoffCount :: GenerationContext -> RepetitionNode -> Maybe (Int, GenerationContext)
exponentialBackoffCount ctx repNode
  | depth ctx > maxDepth ctx `div` 2 =
      -- Reduce count when deep in recursion
      let reductionFactor = 2 ^ (depth ctx - maxDepth ctx `div` 2)
          reducedMax = max 1 (maxCount repNode `div` reductionFactor)
          count = min (minCount repNode) reducedMax
      in Just (count, ctx)
  | otherwise = randomCountSelection ctx repNode
```

### 2.3 Separator Generation

```haskell
generateSeparator :: GrammarNode Text -> Maybe Text
generateSeparator node =
  case node of
    TSGN.Seq [TSGN.StringLiteral sep, _] -> Just sep
    TSGN.Field _ content -> generateSeparator content
    TSGN.Alias content _ _ -> generateSeparator content
    _ -> defaultSeparator node

defaultSeparator :: GrammarNode Text -> Maybe Text
defaultSeparator node =
  case inferNodeType node of
    "argument_list" -> Just ", "
    "statement_list" -> Just "; "
    "binary_expression" -> Just " "  -- Operator will be inserted
    _ -> Nothing
```

### 2.4 Smart Repetition Generation

#### Adaptive Repetition
```haskell
adaptiveRepetition :: GenerationContext -> RepetitionNode -> Maybe (Text, GenerationContext)
adaptiveRepetition ctx repNode = do
  -- Determine count based on context
  count <- determineAdaptiveCount ctx repNode

  -- Generate items with potential early termination
  (items, ctx') <- generateAdaptiveItems ctx repNode.content count

  -- Apply separator
  let separator = fromMaybe "" (separator repNode)
      result = T.intercalate separator items

  return (result, ctx')
  where
    determineAdaptiveCount :: GenerationContext -> RepetitionNode -> Maybe Int
    determineAdaptiveCount ctx node
      | depth ctx >= maxDepth ctx = Just (minCount node)
      | willExceedLengthLimit ctx node = Just (minCount node)
      | otherwise = randomCountSelection ctx node

    generateAdaptiveItems :: GenerationContext -> GrammarNode Text -> Int
                         -> Maybe ([Text], GenerationContext)
    generateAdaptiveItems ctx content 0 = Just ([], ctx)
    generateAdaptiveItems ctx content n = do
      (first, ctx1) <- evaluateNode ctx content
      -- Check if we should continue
      if shouldContinueGeneration ctx1 (n - 1)
        then do
          (rest, ctxN) <- generateAdaptiveItems ctx1 content (n - 1)
          return (first : rest, ctxN)
        else
          return ([first], ctx1)
```

#### Pattern-Aware Repetition
```haskell
patternAwareRepetition :: GenerationContext -> RepetitionNode -> Maybe (Text, GenerationContext)
patternAwareRepetition ctx repNode =
  case detectPattern repNode.content of
    Just patternType -> generatePattern patternType ctx repNode
    Nothing -> standardRepetition ctx repNode

data RepetitionPattern
  = ListPattern { delimiter :: Text }
  | AlternatingPattern { evenItem :: GrammarNode Text, oddItem :: GrammarNode Text }
  | IncrementalPattern { base :: GrammarNode Text, increment :: GrammarNode Text }
  deriving (Show)

detectPattern :: GrammarNode Text -> Maybe RepetitionPattern
detectPattern node =
  case node of
    TSGN.Seq [item, TSGN.StringLiteral delim, TSGN.Repeat rest]
      | rest == item -> Just (ListPattern delim)
    TSGN.Choice [even, odd] -> Just (AlternatingPattern even odd)
    _ -> Nothing
```

## 3. Integration with Existing System

### 3.1 Modified Inference System

```haskell
-- Enhance Inference.hs to track choice and repetition metadata
data EnhancedGrammarNode = EnhancedGrammarNode
  { originalNode :: TSGN.GrammarNode Text
  , choiceInfo :: Maybe ChoiceInfo
  , repetitionInfo :: Maybe RepetitionInfo
  , estimatedComplexity :: Double
  } deriving (Show)

annotateWithMetadata :: TSGN.GrammarNode Text -> EnhancedGrammarNode
annotateWithMetadata node =
  EnhancedGrammarNode
    { originalNode = node
    , choiceInfo = extractChoiceInfo node
    , repetitionInfo = extractRepetitionInfo node
    , estimatedComplexity = estimateComplexity node
    }
```

### 3.2 Enhanced Property Extraction

```haskell
-- In ProgBuilderDescription.hs
data EnhancedProperty
  = EnhancedProperty
  { property :: Property
  , generationHints :: GenerationHints
  } deriving (Show)

data GenerationHints = GenerationHints
  { isOptional :: Bool
  , isRepeatable :: Bool
  , preferredValues :: [Text]
  , valueConstraints :: [ValueConstraint]
  } deriving (Show)

extractWithHints :: TSGN.Node -> [EnhancedProperty]
extractWithHints node =
  let props = propsOfNode node
      hints = map (extractHints node) props
  in zipWith EnhancedProperty props hints
```

## 4. Configuration and Customization

### 4.1 Configuration Types

```haskell
data GenerationConfig = GenerationConfig
  { choiceResolution :: ChoiceResolutionConfig
  , repetitionHandling :: RepetitionConfig
  , constraints :: GlobalConstraints
  , outputFormat :: OutputFormat
  } deriving (Show)

data ChoiceResolutionConfig = ChoiceResolutionConfig
  { defaultStrategy :: ChoiceStrategy
  , fallbackStrategy :: ChoiceStrategy
  , maxBacktrackDepth :: Int
  , enableSmartSelection :: Bool
  , customProbabilities :: Map Text [Double]
  } deriving (Show)

data RepetitionConfig = RepetitionConfig
  { defaultMinCount :: Int
  , defaultMaxCount :: Int
  , adaptiveCounting :: Bool
  , lengthAware :: Bool
  , separatorInference :: Bool
  } deriving (Show)
```

### 4.2 Command Line Interface

```haskell
-- In app/Args.hs
data CodeGenArgs = CodeGenArgs
  { inputFile :: FilePath
  , outputDir :: FilePath
  , generationStrategy :: Maybe GenerationStrategy
  , choiceConfig :: Maybe ChoiceResolutionConfig
  , repetitionConfig :: Maybe RepetitionConfig
  , maxDepth :: Maybe Int
  , seed :: Maybe Int
  } deriving (Show)

parseCodeGenArgs :: Parser CodeGenArgs
parseCodeGenArgs = CodeGenArgs
  <$> argument str (metavar "GRAMMAR_JSON")
  <*> strOption
      ( long "output-dir"
      <> short 'o'
      <> metavar "DIR"
      <> help "Output directory for generated files"
      )
  <*> optional (option (maybeReader parseStrategy)
      ( long "strategy"
      <> metavar "STRATEGY"
      <> help "Generation strategy (random|guided|exhaustive)"
      ))
  <*> optional (option (maybeReader parseChoiceConfig)
      ( long "choice-config"
      <> metavar "CONFIG"
      <> help "Choice resolution configuration"
      ))
  <*> optional (option (maybeReader parseRepetitionConfig)
      ( long "repetition-config"
      <> metavar "CONFIG"
      <> help "Repetition handling configuration"
      ))
```

## 5. Testing and Validation

### 5.1 Test Cases for Choice Resolution

```haskell
testChoiceResolution :: TestTree
testChoiceResolution = testGroup "Choice Resolution"
  [ testCase "Random selection within bounds" $
      let alts = [leaf "a", leaf "b", leaf "c"]
          ctx = defaultContext { strategy = RandomStrategy (Just 42) }
      in do
        (idx, _) <- randomSelection (Just 42) alts ctx
        assertBool "Index within bounds" (idx >= 0 && idx < length alts)

  , testCase "Guided selection with constraints" $
      let alts = [symbol "if_statement", symbol "while_statement"]
          constraints = [PreferType "if_statement"]
          ctx = defaultContext { strategy = GuidedStrategy constraints }
      in do
        (idx, _) <- guidedSelection constraints alts ctx
        assertEqual "Should select if_statement" 0 idx

  , testCase "Exhaustive selection cycles through all" $
      let alts = [leaf "a", leaf "b"]
          ctx = defaultContext { strategy = ExhaustiveStrategy 10 }
      in do
        (idx1, ctx1) <- exhaustiveSelection ctx alts
        (idx2, ctx2) <- exhaustiveSelection ctx1 alts
        assertBool "Should select different alternatives" (idx1 /= idx2)
  ]
```

### 5.2 Test Cases for Repetition Handling

```haskell
testRepetitionHandling :: TestTree
testRepetitionHandling = testGroup "Repetition Handling"
  [ testCase "Count within bounds" $
      let repNode = RepetitionNode (leaf "x") 0 5 Nothing False
          ctx = defaultContext
      in do
        (count, _) <- randomCountSelection ctx repNode
        assertBool "Count within min-max" (count >= 0 && count <= 5)

  , testCase "Repeat1 has minimum 1" $
      let repNode = RepetitionNode (leaf "x") 1 5 Nothing True
          ctx = defaultContext
      in do
        (count, _) <- randomCountSelection ctx repNode
        assertBool "Repeat1 count >= 1" (count >= 1)

  , testCase "Separator application" $
      let items = ["a", "b", "c"]
          separator = ", "
          expected = "a, b, c"
      in assertEqual "Correct separator joining" expected
           (T.intercalate separator items)
  ]
```

## 6. Performance Optimizations

### 6.1 Memoization

```haskell
memoizedChoiceSelection :: GenerationContext -> [GrammarNode Text] -> Maybe (Int, GenerationContext)
memoizedChoiceSelection ctx alternatives =
  let key = (hash ctx, hash alternatives)
  in case lookup key (choiceCache ctx) of
      Just result -> Just result
      Nothing -> do
        result <- selectAlternative ctx alternatives
        let newCache = (key, result) : choiceCache ctx
        return (fst result, ctx { choiceCache = newCache })
```

### 6.2 Early Pruning

```haskell
pruneAlternatives :: GenerationContext -> [GrammarNode Text] -> [GrammarNode Text]
pruneAlternatives ctx alternatives =
  filter (isFeasible ctx) alternatives
  where
    isFeasible :: GenerationContext -> GrammarNode Text -> Bool
    isFeasible ctx node =
      let estimatedDepth = estimateDepth node
          currentDepth = depth ctx
      in currentDepth + estimatedDepth <= maxDepth ctx
```

## 7. Error Handling and Recovery

### 7.1 Choice Resolution Errors

```haskell
data ChoiceError
  = NoValidAlternative [GrammarNode Text]
  | ConstraintConflict [ChoiceConstraint]
  | DepthLimitExceeded Int Int
  | ProbabilityError Text
  deriving (Show)

handleChoiceError :: ChoiceError -> GenerationContext -> Maybe (Int, GenerationContext)
handleChoiceError err ctx =
  case err of
    NoValidAlternative alts ->
      -- Try with relaxed constraints
      let relaxedCtx = relaxConstraints ctx
      in selectAlternative relaxedCtx alts

    DepthLimitExceeded current maxAllowed ->
      -- Select shallowest alternative
      let shallowest = findShallowestAlternative alts
      in case shallowest of
          Just idx -> Just (idx, ctx)
          Nothing -> Nothing

    _ -> Nothing  -- Unrecoverable
```

### 7.2 Repetition Errors

```haskell
data RepetitionError
  = CountOutOfBounds Int Int Int  -- actual, min, max
  | SeparatorMismatch Text Text   -- expected, actual
  | InfiniteLoopDetected
  deriving (Show)

recoverFromRepetitionError :: RepetitionError -> RepetitionNode -> Maybe RepetitionNode
recoverFromRepetitionError err node =
  case err of
    CountOutOfBounds actual minC maxC ->
      let corrected = clamp minC maxC actual
      in Just node { minCount = corrected, maxCount = corrected }

    InfiniteLoopDetected ->
      -- Add a hard limit
      Just node { maxCount = min 10 (maxCount node) }

    _ -> Nothing
```

This design provides a comprehensive system for handling choice resolution and repetition in grammar-based code generation, with support for multiple strategies, configuration, and error recovery.