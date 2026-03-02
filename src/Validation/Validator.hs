{-# LANGUAGE OverloadedStrings #-}

-- | Validation module for generated code.
-- Provides validation of generated sentences against the grammar,
-- syntax checking, and semantic validation.
module Validation.Validator where

import TreeSitterGrammarNodes qualified as TSGN
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Map qualified as Map
import Control.Monad (foldM)

-- | Validation level.
data ValidationLevel
  = SyntaxOnly      -- ^ Only check syntax (parse tree structure)
  | WithSemantics   -- ^ Also check semantic constraints
  | FullValidation  -- ^ Full validation including type checking
  deriving (Show, Eq, Enum, Bounded)

-- | Validation result.
data ValidationResult
  = Valid
  | Invalid { reason :: Text, details :: Maybe Text }
  deriving (Show, Eq)

-- | Validation context.
data ValidationContext = ValidationContext
  { grammar :: TSGN.Grammar
  , level :: ValidationLevel
  , symbolTable :: Map.Map Text TSGN.Node
  , errors :: [Text]
  } deriving (Show)

-- | Validate a generated sentence against a grammar rule.
validateSentence :: TSGN.Grammar -> ValidationLevel -> Text -> Text -> ValidationResult
validateSentence grammar' level' ruleName' sentence = do
  let symbolTable' = TSGN.grammarNodes grammar'
  case Map.lookup ruleName' symbolTable' of
    Nothing -> Invalid { reason = "Rule not found: " <> ruleName', details = Nothing }
    Just ruleNode ->
      let ctx = ValidationContext
            { grammar = grammar'
            , level = level'
            , symbolTable = symbolTable'
            , errors = []
            }
      in case validateNode ruleNode sentence ctx of
          Left err -> Invalid { reason = "Validation failed", details = Just err }
          Right _ -> Valid

-- | Validate a node against input text.
validateNode :: TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validateNode node input ctx = case node of
  TSGN.Seq members -> validateSeq members input ctx
  TSGN.Choice alternatives -> validateChoice alternatives input ctx
  TSGN.Repeat content -> validateRepeat content input ctx
  TSGN.Repeat1 content -> validateRepeat1 content input ctx
  TSGN.Symbol name -> validateSymbol name input ctx
  TSGN.StringLiteral expected -> validateStringLiteral expected input ctx
  TSGN.Pattern pattern -> validatePattern pattern input ctx
  TSGN.Blank -> validateBlank input ctx
  TSGN.Field fieldName content -> validateField fieldName content input ctx
  TSGN.Alias content named aliasValue -> validateAlias content named aliasValue input ctx
  TSGN.Token content -> validateToken content input ctx
  TSGN.ImmediateToken content -> validateImmediateToken content input ctx
  TSGN.Prec precedence content -> validatePrec precedence content input ctx
  TSGN.PrecLeft precedence content -> validatePrecLeft precedence content input ctx
  TSGN.PrecRight precedence content -> validatePrecRight precedence content input ctx
  TSGN.PrecDynamic precedence content -> validatePrecDynamic precedence content input ctx
  TSGN.Reserved content contextName -> validateReserved content contextName input ctx
  TSGN.Empty -> validateEmpty input ctx

-- | Validate SEQ node.
validateSeq :: [TSGN.Node] -> Text -> ValidationContext -> Either Text ValidationContext
validateSeq [] input ctx
  | T.null input = Right ctx
  | otherwise = Left "Expected empty input for empty SEQ"
validateSeq (first:rest) input ctx = do
  -- For now, simple validation: just check if we can validate first node
  -- In real implementation, would need to parse and match structure
  ctx' <- validateNode first input ctx
  -- Recursively validate rest (would need to consume input)
  validateSeq rest input ctx'

-- | Validate CHOICE node.
validateChoice :: [TSGN.Node] -> Text -> ValidationContext -> Either Text ValidationContext
validateChoice [] _ ctx = Left "No alternatives in CHOICE"
validateChoice alternatives input ctx =
  let tryAlternatives [] = Left "No alternative matched"
      tryAlternatives (alt:alts) =
        case validateNode alt input ctx of
          Right ctx' -> Right ctx'
          Left _ -> tryAlternatives alts
  in tryAlternatives alternatives

-- | Validate REPEAT node.
validateRepeat :: TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validateRepeat content input ctx
  | T.null input = Right ctx  -- 0 repetitions is valid
  | otherwise = validateNode content input ctx  -- For now, just try once

-- | Validate REPEAT1 node.
validateRepeat1 :: TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validateRepeat1 content input ctx
  | T.null input = Left "REPEAT1 requires at least one repetition"
  | otherwise = validateNode content input ctx  -- For now, just try once

-- | Validate SYMBOL node.
validateSymbol :: Text -> Text -> ValidationContext -> Either Text ValidationContext
validateSymbol name input ctx =
  case Map.lookup name ctx.symbolTable of
    Nothing -> Left $ "Symbol not found: " <> name
    Just node -> validateNode node input ctx

-- | Validate STRING literal node.
validateStringLiteral :: Text -> Text -> ValidationContext -> Either Text ValidationContext
validateStringLiteral expected input ctx
  | input == expected = Right ctx
  | otherwise = Left $ "Expected string literal: " <> expected <> ", got: " <> input

-- | Validate PATTERN node.
validatePattern :: Text -> Text -> ValidationContext -> Either Text ValidationContext
validatePattern _pattern input ctx
  | not (T.null input) = Right ctx  -- For now, any non-empty input matches pattern
  | otherwise = Left "Pattern requires non-empty input"

-- | Validate BLANK node.
validateBlank :: Text -> ValidationContext -> Either Text ValidationContext
validateBlank input ctx
  | T.null input = Right ctx
  | otherwise = Left "BLANK requires empty input"

-- | Validate FIELD node.
validateField :: Text -> TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validateField fieldName content input ctx = do
  -- For now, just validate content
  validateNode content input ctx

-- | Validate ALIAS node.
validateAlias :: TSGN.Node -> Bool -> Text -> Text -> ValidationContext -> Either Text ValidationContext
validateAlias content _named _aliasValue input ctx =
  validateNode content input ctx

-- | Validate TOKEN node.
validateToken :: TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validateToken content input ctx =
  validateNode content input ctx

-- | Validate IMMEDIATE_TOKEN node.
validateImmediateToken :: TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validateImmediateToken content input ctx =
  validateNode content input ctx

-- | Validate PREC node.
validatePrec :: TSGN.PrecedenceValue -> TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validatePrec _precedence content input ctx =
  validateNode content input ctx

-- | Validate PREC_LEFT node.
validatePrecLeft :: TSGN.PrecedenceValue -> TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validatePrecLeft _precedence content input ctx =
  validateNode content input ctx

-- | Validate PREC_RIGHT node.
validatePrecRight :: TSGN.PrecedenceValue -> TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validatePrecRight _precedence content input ctx =
  validateNode content input ctx

-- | Validate PREC_DYNAMIC node.
validatePrecDynamic :: TSGN.PrecedenceValue -> TSGN.Node -> Text -> ValidationContext -> Either Text ValidationContext
validatePrecDynamic _precedence content input ctx =
  validateNode content input ctx

-- | Validate RESERVED node.
validateReserved :: TSGN.Node -> Text -> Text -> ValidationContext -> Either Text ValidationContext
validateReserved content _contextName input ctx =
  validateNode content input ctx

-- | Validate EMPTY node.
validateEmpty :: Text -> ValidationContext -> Either Text ValidationContext
validateEmpty input ctx
  | T.null input = Right ctx
  | otherwise = Left "EMPTY requires empty input"

-- | Batch validate multiple sentences.
batchValidate :: TSGN.Grammar -> ValidationLevel -> [(Text, Text)] -> [(Text, ValidationResult)]
batchValidate grammar' level' sentences =
  map (\(ruleName', sentence') -> (ruleName', validateSentence grammar' level' ruleName' sentence')) sentences

-- | Check if all validations pass.
allValid :: [(Text, ValidationResult)] -> Bool
allValid = all (\(_, result) -> result == Valid)

-- | Get failed validations.
getFailures :: [(Text, ValidationResult)] -> [(Text, Text)]
getFailures = foldr collect []
  where
    collect (ruleName, Invalid reason _details) acc = (ruleName, reason) : acc
    collect _ acc = acc