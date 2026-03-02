{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Core generation algorithms for Tree-sitter grammar nodes.
-- Provides generation strategies, evaluation functions, and context management
-- for generating sentences from grammar definitions.
module Fundamentals.Generation where

import TreeSitterGrammarNodes qualified as TSGN
import Data.Map qualified as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import System.Random (StdGen, newStdGen, randomR)
import Control.Monad.State (State, get, put, evalState)

-- | Generation strategies for controlling how sentences are generated.
data GenerationStrategy
  = RandomStrategy { seed :: Maybe Int }
  | GuidedStrategy { constraints :: [Constraint] }
  | ExhaustiveStrategy { exhaustiveMaxDepth :: Int }
  deriving (Show, Eq)

-- | Constraints for guided generation.
data Constraint
  = PreferAlternative Text Int  -- ^ Prefer alternative at index for rule
  | MaxDepth Int                -- ^ Maximum recursion depth
  | PreferRepetition Int Int    -- ^ Prefer repetition count (min, max)
  | AvoidRule Text              -- ^ Avoid expanding this rule
  deriving (Show, Eq)

-- | Generation context containing strategy, depth limits, symbol table, and history.
data GenerationContext = GenerationContext
  { strategy :: GenerationStrategy
  , depth :: Int
  , maxDepth :: Int
  , symbolTable :: Map.Map Text (TSGN.Node)
  , choiceHistory :: [ChoiceDecision]
  } deriving (Show)

-- | Decision made at a CHOICE node.
data ChoiceDecision = ChoiceDecision
  { ruleName :: Text
  , alternativeIndex :: Int
  , alternativeName :: Text
  } deriving (Show, Eq)

-- | Factory method configuration (for later phases).
data FactoryConfig = FactoryConfig
  { generateFactories :: Bool
  , factoryStyle :: FactoryStyle
  } deriving (Show)

-- | Factory style options.
data FactoryStyle
  = StaticMethods      -- ^ static createX() methods
  | BuilderPattern     -- ^ fluent builder pattern
  | FunctionalStyle    -- ^ pure functions
  deriving (Show, Eq)

-- | State monad for generation, carrying random generator and context.
type GenerationState a = State (StdGen, GenerationContext) a

-- | Create a default generation context.
defaultContext :: GenerationStrategy -> TSGN.Nodes -> IO (StdGen, GenerationContext)
defaultContext strategy nodes = do
  gen <- newStdGen
  -- Convert from Map String Node to Map Text Node
  let symbolTable = Map.fromList $ map (\(k, v) -> (T.pack k, v)) $ Map.toList nodes
  let maxDepth' = case strategy of
        ExhaustiveStrategy depthLimit -> depthLimit
        _ -> 10  -- Reasonable default to prevent infinite recursion
  let ctx = GenerationContext
        { strategy = strategy
        , depth = 0
        , maxDepth = maxDepth'
        , symbolTable = symbolTable
        , choiceHistory = []
        }
  return (gen, ctx)

-- | Evaluate a grammar node to generate a sentence.
-- Returns the generated text and updated state.
evaluateNode :: TSGN.Node -> GenerationState (Maybe Text)
evaluateNode node = do
  (_gen, ctx) <- get
  if ctx.depth > ctx.maxDepth
    then return Nothing  -- Depth limit exceeded
    else case node of
      TSGN.Seq members -> evaluateSeq members
      TSGN.Choice alternatives -> evaluateChoice alternatives
      TSGN.Repeat content -> evaluateRepeat content
      TSGN.Repeat1 content -> evaluateRepeat1 content
      TSGN.Symbol name -> evaluateSymbol name
      TSGN.StringLiteral value -> evaluateStringLiteral value
      TSGN.Pattern value -> evaluatePattern value
      TSGN.Blank -> evaluateBlank
      TSGN.Field fieldName content -> evaluateField fieldName content
      TSGN.Alias content named aliasValue -> evaluateAlias content named aliasValue
      TSGN.Token content -> evaluateToken content
      TSGN.ImmediateToken content -> evaluateImmediateToken content
      TSGN.Prec precedence content -> evaluatePrec precedence content
      TSGN.PrecLeft precedence content -> evaluatePrecLeft precedence content
      TSGN.PrecRight precedence content -> evaluatePrecRight precedence content
      TSGN.PrecDynamic precedence content -> evaluatePrecDynamic precedence content
      TSGN.Reserved content contextName -> evaluateReserved content contextName
      TSGN.Empty -> evaluateEmpty

-- | Evaluate a SEQ node: evaluate all members and concatenate results.
evaluateSeq :: [TSGN.Node] -> GenerationState (Maybe Text)
evaluateSeq members = do
  (gen, ctx) <- get
  put (gen, ctx { depth = ctx.depth + 1 })
  results <- mapM evaluateNode members
  (gen', ctx') <- get
  put (gen', ctx' { depth = ctx.depth })
  case sequence results of
    Just texts -> return $ Just $ T.concat texts
    Nothing -> return Nothing

-- | Evaluate a CHOICE node: select one alternative based on strategy.
evaluateChoice :: [TSGN.Node] -> GenerationState (Maybe Text)
evaluateChoice alternatives
  | null alternatives = return Nothing
  | otherwise = do
      (gen, ctx) <- get
      case ctx.strategy of
        RandomStrategy _ -> do
          let (idx, newGen) = randomR (0, length alternatives - 1) gen
          put (newGen, ctx)
          evaluateNode (alternatives !! idx)
        GuidedStrategy constraints -> do
          -- For now, just use first alternative
          -- TODO: Use constraints to select alternative
          evaluateNode (alternatives !! 0)
        ExhaustiveStrategy maxDepth' -> do
          -- Try alternatives in order until one succeeds
          let currentDepth = ctx.depth
          if currentDepth > maxDepth'
            then return Nothing
            else tryAlternatives alternatives 0
          where
            tryAlternatives :: [TSGN.Node] -> Int -> GenerationState (Maybe Text)
            tryAlternatives [] _ = return Nothing
            tryAlternatives (alt:alts) idx = do
              result <- evaluateNode alt
              case result of
                Just _ -> return result
                Nothing -> tryAlternatives alts (idx + 1)

-- | Evaluate a REPEAT node: generate 0-n repetitions.
evaluateRepeat :: TSGN.Node -> GenerationState (Maybe Text)
evaluateRepeat content = do
  count <- determineCount 0 5  -- Default: 0 to 5 repetitions
  generateRepeated content count

-- | Evaluate a REPEAT1 node: generate 1-n repetitions.
evaluateRepeat1 :: TSGN.Node -> GenerationState (Maybe Text)
evaluateRepeat1 content = do
  count <- determineCount 1 5  -- Default: 1 to 5 repetitions
  generateRepeated content count

-- | Evaluate a SYMBOL node: look up and evaluate referenced rule.
evaluateSymbol :: Text -> GenerationState (Maybe Text)
evaluateSymbol name = do
  (_gen, ctx) <- get
  case Map.lookup name ctx.symbolTable of
    Just node -> evaluateNode node
    Nothing -> return Nothing

-- | Evaluate a STRING literal node: return the literal value.
evaluateStringLiteral :: Text -> GenerationState (Maybe Text)
evaluateStringLiteral value = return $ Just value

-- | Evaluate a PATTERN node: return the pattern value.
evaluatePattern :: Text -> GenerationState (Maybe Text)
evaluatePattern value = return $ Just value

-- | Evaluate a BLANK node: return empty string.
evaluateBlank :: GenerationState (Maybe Text)
evaluateBlank = return $ Just ""

-- | Evaluate a FIELD node: generate with field annotation.
evaluateField :: Text -> TSGN.Node -> GenerationState (Maybe Text)
evaluateField _fieldName content = evaluateNode content

-- | Evaluate an ALIAS node: evaluate the content.
evaluateAlias :: TSGN.Node -> Bool -> Text -> GenerationState (Maybe Text)
evaluateAlias content _named _aliasValue = evaluateNode content

-- | Evaluate a TOKEN node: evaluate the content.
evaluateToken :: TSGN.Node -> GenerationState (Maybe Text)
evaluateToken content = evaluateNode content

-- | Evaluate an IMMEDIATE_TOKEN node: evaluate the content.
evaluateImmediateToken :: TSGN.Node -> GenerationState (Maybe Text)
evaluateImmediateToken content = evaluateNode content

-- | Evaluate a PREC node: evaluate the content.
evaluatePrec :: TSGN.PrecedenceValue -> TSGN.Node -> GenerationState (Maybe Text)
evaluatePrec _precedence content = evaluateNode content

-- | Evaluate a PREC_LEFT node: evaluate the content.
evaluatePrecLeft :: TSGN.PrecedenceValue -> TSGN.Node -> GenerationState (Maybe Text)
evaluatePrecLeft _precedence content = evaluateNode content

-- | Evaluate a PREC_RIGHT node: evaluate the content.
evaluatePrecRight :: TSGN.PrecedenceValue -> TSGN.Node -> GenerationState (Maybe Text)
evaluatePrecRight _precedence content = evaluateNode content

-- | Evaluate a PREC_DYNAMIC node: evaluate the content.
evaluatePrecDynamic :: TSGN.PrecedenceValue -> TSGN.Node -> GenerationState (Maybe Text)
evaluatePrecDynamic _precedence content = evaluateNode content

-- | Evaluate a RESERVED node: evaluate the content.
evaluateReserved :: TSGN.Node -> Text -> GenerationState (Maybe Text)
evaluateReserved content _contextName = evaluateNode content

-- | Evaluate an EMPTY node: return empty string.
evaluateEmpty :: GenerationState (Maybe Text)
evaluateEmpty = return $ Just ""

-- | Determine repetition count based on strategy.
determineCount :: Int -> Int -> GenerationState Int
determineCount minCount maxCount = do
  (gen, ctx) <- get
  case ctx.strategy of
    RandomStrategy _ -> do
      let (count, newGen) = randomR (minCount, maxCount) gen
      put (newGen, ctx)
      return count
    GuidedStrategy _ -> return minCount  -- Conservative for guided
    ExhaustiveStrategy _ -> return minCount  -- Conservative for exhaustive

-- | Generate repeated content.
generateRepeated :: TSGN.Node -> Int -> GenerationState (Maybe Text)
generateRepeated content count
  | count <= 0 = return $ Just ""
  | otherwise = do
      mFirst <- evaluateNode content
      case mFirst of
        Nothing -> return Nothing
        Just firstResult -> do
          mRest <- generateRepeated content (count - 1)
          case mRest of
            Nothing -> return Nothing
            Just restResult -> do
              let separator = determineSeparator content
              return $ Just $ firstResult <> separator <> restResult

-- | Determine separator for repeated content.
determineSeparator :: TSGN.Node -> Text
determineSeparator _ = " "

-- | Generate a sentence from a grammar rule.
generateFromRule :: Text -> GenerationState (Maybe Text)
generateFromRule ruleName = do
  (_gen, ctx) <- get
  case Map.lookup ruleName ctx.symbolTable of
    Just node -> evaluateNode node
    Nothing -> return Nothing

-- | Run generation with initial state.
runGeneration :: (StdGen, GenerationContext) -> GenerationState a -> a
runGeneration initState action = evalState action initState

-- | Generate a sentence from a rule using the given strategy.
generateSentence :: GenerationStrategy -> TSGN.Nodes -> Text -> IO (Maybe Text)
generateSentence strategy nodes ruleName = do
  initState <- defaultContext strategy nodes
  return $ runGeneration initState $ generateFromRule ruleName

-- | Generate multiple sentences from a grammar.
generateSentences :: TSGN.Grammar -> GenerationStrategy -> Int -> IO [Text]
generateSentences grammar strategy count = do
  let symbolTable = TSGN.grammarNodes grammar
  initState <- defaultContext strategy symbolTable
  let ruleNames = map T.pack $ Map.keys symbolTable
  return $ take count $ concatMap (\ruleName ->
    case runGeneration initState (generateFromRule ruleName) of
      Just sentence -> [sentence]
      Nothing -> []) ruleNames