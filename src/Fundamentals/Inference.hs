{-# LANGUAGE OverloadedStrings #-}

-- | Logics that used to do transform a GrammarNode into
-- a form that acceptable in performance aspect. And inference
-- out a pattern of expressions that used to evaluate a GrammarNode
-- into correspond source code that used to generate sentence of
-- target language.
module Fundamentals.Inference where

import Control.Monad.State (State, get, modify, put, runState)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Traversable (forM)
import TreeSitterGrammarNodes qualified as TSGN

type TextGN = TSGN.GrammarNode Text

data TransitionPoint a
  = SEQ {seqNode :: a}
  | CHOICE
      { ruleName :: Text,
        choiceIdentifier :: a,
        alternativeIndex :: Int,
        alternativeNode :: a
      }
  deriving (Eq, Ord, Show)

data InferenceMeta a = IM
  { node :: a,
    currentRule :: Maybe Text,
    choiceDepth :: Int,
    isRootNode :: Bool,
    transitionPoints :: [TransitionPoint a],
    usedIdentifiers :: Set.Set Text
  }
  deriving (Eq, Ord, Show)

type InferGN = State (InferenceMeta TextGN)

-- | Split CHOICE node: empty its members if it's top-most CHOICE.
splitChoiceRule :: TSGN.Node -> InferGN TSGN.Node
splitChoiceRule n = case n of
  TSGN.Choice _ -> do
    meta <- get
    -- Only empty if we're at top-most CHOICE level and root node
    if choiceDepth meta == 1 && isRootNode meta
      then return (TSGN.Choice [])
      else return n
  _ -> return n

-- | Traverse a node with rule name context and CHOICE depth tracking.
-- The 'isRoot' flag indicates whether this node is the root of a grammar rule.
traverseWithRuleContext :: Bool -> Text -> (TSGN.Node -> InferGN TSGN.Node) -> TSGN.Node -> InferGN TSGN.Node
traverseWithRuleContext isRoot ruleName f n = do
  oldMeta <- get
  -- Update context with current node and rule
  put $ oldMeta {node = n, currentRule = Just ruleName, isRootNode = isRoot}
  nodeResult <- case n of
    TSGN.Choice members -> do
      -- Increment choice depth
      modify (\m -> m {choiceDepth = choiceDepth m + 1})
      meta <- get
      let isTopMost = choiceDepth meta == 1
      branchResult <-
        if isTopMost && isRoot
          then do
            -- Process each alternative, capturing them as transition points
            processedMembers <- forM (zip [0 ..] members) $ \(idx, member) -> do
              -- Process the alternative (not root anymore)
              processedAlt <- traverseWithRuleContext False ruleName f member
              -- Store as transition point with rule name and index
              let choiceId = TSGN.Symbol (ruleName <> "_choice")
              modify
                ( \m ->
                    m
                      { transitionPoints = CHOICE ruleName choiceId idx processedAlt : transitionPoints m
                      }
                )
              return processedAlt
            -- Apply transformation to the CHOICE node itself (will be emptied by splitChoiceRule)
            f (TSGN.Choice processedMembers)
          else do
            -- Nested CHOICE or non-root: process but don't create transition points
            processedMembers <- forM members $ \member -> do
              traverseWithRuleContext False ruleName f member
            f (TSGN.Choice processedMembers)
      -- Decrement choice depth
      modify (\m -> m {choiceDepth = choiceDepth m - 1})
      return branchResult
    TSGN.Seq members -> do
      processedMembers <- forM members $ \member -> do
        traverseWithRuleContext False ruleName f member
      f (TSGN.Seq processedMembers)
    TSGN.Repeat content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.Repeat processedContent)
    TSGN.Repeat1 content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.Repeat1 processedContent)
    TSGN.Field fieldName content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.Field fieldName processedContent)
    TSGN.Alias content named aliasValue -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.Alias processedContent named aliasValue)
    TSGN.Token content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.Token processedContent)
    TSGN.ImmediateToken content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.ImmediateToken processedContent)
    TSGN.Prec precedence content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.Prec precedence processedContent)
    TSGN.PrecLeft precedence content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.PrecLeft precedence processedContent)
    TSGN.PrecRight precedence content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.PrecRight precedence processedContent)
    TSGN.PrecDynamic precedence content -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.PrecDynamic precedence processedContent)
    TSGN.Reserved content contextName -> do
      processedContent <- traverseWithRuleContext False ruleName f content
      f (TSGN.Reserved processedContent contextName)
    -- Leaf nodes: Symbol, StringLiteral, Pattern, Blank, Empty
    _ -> f n
  -- Restore rule context and node
  modify (\meta -> meta {currentRule = currentRule oldMeta, node = node oldMeta, isRootNode = isRootNode oldMeta})
  return nodeResult

-- | Transform a single rule, splitting its CHOICE nodes.
transformRule :: (String, TSGN.Node) -> InferGN (String, TSGN.Node)
transformRule (ruleNameStr, node) = do
  let ruleName = T.pack ruleNameStr
  transformed <- traverseWithRuleContext True ruleName splitChoiceRule node
  return (ruleNameStr, transformed)

-- | Process grammar: empty top-most CHOICE nodes and collect transition points.
splitChoicesInGrammar :: TSGN.Grammar -> (TSGN.Grammar, InferenceMeta TSGN.Node)
splitChoicesInGrammar grammar =
  let rules = TSGN.grammarNodes grammar
      initialState = IM TSGN.Empty Nothing 0 False [] Set.empty
      (processedRules, finalState) =
        runState
          (mapM transformRule (Map.toList rules))
          initialState
   in ( grammar {TSGN.grammarNodes = Map.fromList processedRules},
        finalState
      )

-- | Extract precedence identifier and stripped node if node is a precedence wrapper.
precedenceIdentifier :: TSGN.Node -> Maybe (T.Text, TSGN.Node)
precedenceIdentifier node = case node of
  TSGN.Prec prec content -> Just (precToText prec, content)
  TSGN.PrecLeft prec content -> Just (T.append "left_" (precToText prec), content)
  TSGN.PrecRight prec content -> Just (T.append "right_" (precToText prec), content)
  TSGN.PrecDynamic prec content -> Just (T.append "dynamic_" (precToText prec), content)
  _ -> Nothing
  where
    precToText :: TSGN.PrecedenceValue -> T.Text
    precToText (TSGN.NamedPrecedence s) = s
    precToText (TSGN.NumericPrecedence n)
      | n < 0 = T.append "neg_" (T.pack $ show (abs n))
      | otherwise = T.pack (show n)

-- | Extract symbol name identifier.
symbolIdentifier :: TSGN.Node -> Maybe (T.Text, TSGN.Node)
symbolIdentifier node = case node of
  TSGN.Symbol name -> Just (name, node)
  _ -> Nothing

-- | Extract identifier and possibly stripped node.
-- Returns (identifier, node) where identifier is from precedence or symbol,
-- and node is stripped of precedence wrapper if applicable.
extractIdentifierAndNode :: TSGN.Node -> Maybe (T.Text, TSGN.Node)
extractIdentifierAndNode node =
  case precedenceIdentifier node of
    Just (precId, strippedNode) -> Just (precId, strippedNode)
    Nothing -> case symbolIdentifier node of
      Just (symbolId, symbolNode) -> Just (symbolId, symbolNode)
      Nothing -> Nothing

-- | Collect transition points for nested CHOICE nodes within a node.
-- Returns additional transition points that should be added to InferenceMeta.
collectNestedChoiceTransitionPoints :: T.Text -> TSGN.Node -> [TransitionPoint TSGN.Node]
collectNestedChoiceTransitionPoints parentName node = case node of
  TSGN.Choice members -> concatMap (\(idx, member) -> CHOICE parentName (TSGN.Symbol (parentName <> "_nested_choice")) idx member : collectNestedChoiceTransitionPoints parentName member) (zip [0 ..] members)
  TSGN.Seq members -> concatMap (collectNestedChoiceTransitionPoints parentName) members
  TSGN.Repeat content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.Repeat1 content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.Field _ content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.Alias content _ _ -> collectNestedChoiceTransitionPoints parentName content
  TSGN.Token content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.ImmediateToken content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.Prec _ content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.PrecLeft _ content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.PrecRight _ content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.PrecDynamic _ content -> collectNestedChoiceTransitionPoints parentName content
  TSGN.Reserved content _ -> collectNestedChoiceTransitionPoints parentName content
  _ -> []

-- | Add transition points for a new rule: SEQ point for the rule itself,
-- plus CHOICE points for any nested CHOICE nodes.
addTransitionPointsForNewRule :: InferenceMeta TSGN.Node -> T.Text -> TSGN.Node -> InferenceMeta TSGN.Node
addTransitionPointsForNewRule meta ruleName node =
  let seqPoint = SEQ node
      nestedPoints = collectNestedChoiceTransitionPoints ruleName node
   in meta {transitionPoints = seqPoint : nestedPoints ++ transitionPoints meta}

-- | Process alternatives of a single rule, generating unique rule names.
-- Omits index when identifier is unique within the rule.
processRuleAlternatives :: T.Text -> [(Int, TSGN.Node)] -> Set.Set Text -> ([(T.Text, TSGN.Node)], Set.Set Text)
processRuleAlternatives ruleName alternatives usedIdents =
  let -- Extract identifier and possibly stripped node for each alternative
      processAlt (idx, node) =
        case extractIdentifierAndNode node of
          Just (ident, stripped) -> (idx, Just ident, stripped)
          Nothing -> (idx, Nothing, node)

      withIdent = map processAlt alternatives

      -- Count occurrences of each identifier (Nothing treated as distinct)
      identCounts =
        Map.fromListWith (+) $
          map (\(_, mbIdent, _) -> (mbIdent, 1 :: Int)) withIdent

      -- Generate name for a single alternative
      generateName idx mbIdent =
        case mbIdent of
          Nothing ->
            -- No identifier: always use index
            ruleName <> "_" <> T.pack (show idx)
          Just ident ->
            case Map.lookup (Just ident) identCounts of
              Just 1 ->
                -- Unique identifier: omit index
                ruleName <> "_" <> ident
              _ ->
                -- Duplicate identifier or not found (should not happen): include index
                ruleName <> "_" <> ident <> "_" <> T.pack (show idx)

      newRules = map (\(idx, mbIdent, node) -> (generateName idx mbIdent, node)) withIdent

      newIdents =
        Set.union usedIdents $
          Set.fromList $
            mapMaybe (\(_, mbIdent, _) -> mbIdent) withIdent
   in (newRules, newIdents)

-- | Create new grammar rules from CHOICE transition points.
addAlternativeRules :: TSGN.Grammar -> InferenceMeta TSGN.Node -> (TSGN.Grammar, InferenceMeta TSGN.Node)
addAlternativeRules grammar inferenceMeta =
  let points = transitionPoints inferenceMeta
      -- Filter only CHOICE transition points
      choicePoints = [tp | tp@CHOICE {} <- points]

      -- Group alternatives by rule name
      groupedByRule =
        Map.fromListWith (++) $
          map
            ( \tp -> case tp of
                CHOICE ruleName _ idx altNode ->
                  (ruleName, [(idx, altNode)])
                _ -> error "groupedByRule: expected CHOICE"
            )
            choicePoints

      -- Process each rule's alternatives, accumulating updated inference meta
      (finalMeta, newRulesList) =
        Map.foldrWithKey
          ( \ruleName alts (metaAcc, rulesAcc) ->
              let (newRules', newUsed) = processRuleAlternatives ruleName alts (usedIdentifiers metaAcc)
                  -- Update used identifiers
                  metaWithUsed = metaAcc {usedIdentifiers = newUsed}
                  -- Add transition points for each new rule
                  metaWithPoints = foldl' (\meta (name, node) -> addTransitionPointsForNewRule meta name node) metaWithUsed newRules'
               in (metaWithPoints, newRules' ++ rulesAcc)
          )
          (inferenceMeta, [])
          groupedByRule

      -- Convert to list of (String, Node)
      newRules = map (\(name, node) -> (T.unpack name, node)) newRulesList

      -- Add to existing grammar nodes
      existingRules = TSGN.grammarNodes grammar
      updatedRules = Map.fromList newRules `Map.union` existingRules
   in (grammar {TSGN.grammarNodes = updatedRules}, finalMeta)

-- | Complete transformation pipeline: split CHOICE nodes and create new rules.
transformGrammarWithChoiceSplitting :: TSGN.Grammar -> TSGN.Grammar
transformGrammarWithChoiceSplitting grammar =
  let (grammarWithEmptyChoices, inferenceMeta) = splitChoicesInGrammar grammar
   in fst $ addAlternativeRules grammarWithEmptyChoices inferenceMeta

-- | trans
trans :: TSGN.Grammar -> TSGN.Grammar
trans = transformGrammarWithChoiceSplitting
