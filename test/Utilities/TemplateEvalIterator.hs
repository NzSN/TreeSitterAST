{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | TemplateEvalIterator systematically generates test cases covering all
-- evaluation paths through a grammar's CHOICE nodes.
--
-- For a grammar with nested CHOICE nodes, it generates test cases for
-- every combination of alternative selections.
--
-- Example:
--   expression: CHOICE [identifier, number, binary_expr]
--   statement: CHOICE [var_decl, return_stmt]
--
-- Would generate 3 × 2 = 6 test cases covering all paths.
module Utilities.TemplateEvalIterator
  ( -- * Core Types
    EvalPath (..),
    LocationPath,
    LocationSegment (..),
    ChoiceInfo (..),
    EvalState (..),
    IterationResult (..),
    ContextProvider (..),

    -- * Path Enumeration
    findAllChoices,
    findAllChoicesInGrammar,
    enumeratePaths,
    enumerateTopLevelPaths,
    totallyCover,

    -- * Evaluation Execution
    executePath,
    iterateAllEvaluations,
    iterateAllEvaluationsWithContext,

    -- * Test Generation
    generateTestCases,
    generateTestCasesWithContext,
    evalStatesToTestTree,

    -- * Default Context Providers
    defaultContextProvider,
    noOpContextProvider,

    -- * Helpers
    getAlternativeName,
  )
where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import ProgBuilder.ECMA.ProgBuilderForECMA (descript)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import TreeSitterGrammarNodes (Node)
import TreeSitterGrammarNodes qualified as TSGN

-- | Location path within a node tree (e.g., ["members", "0", "content"])
-- Used to identify WHERE a CHOICE is nested within a rule
type LocationPath = [LocationSegment]

-- | A segment of a location path
data LocationSegment
  = -- | Index in a SEQ or CHOICE members list
    MemberIndex Int
  | -- | The 'content' field (Repeat, Repeat1, Field, Token, etc.)
    ContentKey
  | -- | A field name in FIELD nodes
    FieldName String
  deriving (Show, Eq, Ord)

-- | Information about a discovered CHOICE node
data ChoiceInfo = ChoiceInfo
  { choiceLocation :: LocationPath,
    choiceAlternatives :: [String],
    -- | Nesting depth (0 = top-level)
    choiceDepth :: Int
  }
  deriving (Show, Eq)

-- | A single step in an evaluation path through the grammar
data EvalPath = EvalPath
  { ruleName :: String,
    -- | Where the CHOICE is located (empty = top-level)
    locationPath :: LocationPath,
    alternativeIndex :: Int,
    alternativeName :: String
  }
  deriving (Show, Eq, Ord)

-- | The result of evaluating a specific path
data EvalState = EvalState
  { -- | The sequence of CHOICE alternatives taken
    path :: [EvalPath],
    -- | The TypeScript class instantiated
    templateClass :: String,
    -- | Output from evaluate()
    generatedCode :: String,
    -- | Does the generated code compile?
    compilable :: Bool,
    -- | Error if compilation failed
    compilationError :: Maybe String
  }
  deriving (Show, Eq)

-- | Result of iterating through all evaluations
data IterationResult = IterationResult
  { totalPaths :: Int,
    successfulPaths :: Int,
    failedPaths :: Int,
    states :: [EvalState]
  }
  deriving (Show, Eq)

-- | A context provider adds necessary declarations to make generated code compilable
-- Example: Adding `const a = 1;` before `let result = a + b;`
newtype ContextProvider = ContextProvider
  { provideContext :: String -> String -- generatedCode -> fullCode with context
  }

-- | Default context provider that adds common variable declarations
defaultContextProvider :: ContextProvider
defaultContextProvider = ContextProvider $ \generatedCode ->
  -- Analyze the generated code and add appropriate context
  let identifiers = extractIdentifiers generatedCode
      varDeclarations = concatMap (\id' -> "const " ++ id' ++ " = 1;\n") identifiers
   in varDeclarations ++ generatedCode
  where
    -- Extract identifier names from generated code (simple heuristic)
    extractIdentifiers :: String -> [String]
    extractIdentifiers code =
      let words' = words code
          -- Filter out keywords and operators
          keywords =
            Set.fromList
              [ "let",
                "const",
                "var",
                "return",
                "if",
                "else",
                "for",
                "while",
                "function",
                "class",
                "new",
                "true",
                "false",
                "null",
                "undefined",
                "+",
                "-",
                "*",
                "/",
                "=",
                ";",
                "(",
                ")",
                "{",
                "}"
              ]
          isIdentifier w =
            not (Set.member w keywords)
              && not (null w)
              && not (all (`elem` ("0123456789" :: String)) w)
          -- Simple pattern: word followed by something that looks like use
          potentialIds = filter isIdentifier words'
       in take 5 $ Set.toList $ Set.fromList potentialIds -- Limit to avoid noise

-- | No-op context provider (passes through generated code unchanged)
noOpContextProvider :: ContextProvider
noOpContextProvider = ContextProvider id

-- | Get the alternative name for a node (used in CHOICE members)
getAlternativeName :: Node -> String
getAlternativeName n = case n of
  TSGN.Symbol name -> T.unpack name
  TSGN.StringLiteral value -> T.unpack value
  TSGN.Pattern _ -> "pattern"
  TSGN.Blank -> "blank"
  TSGN.Seq _ -> "seq"
  TSGN.Choice _ -> "nested_choice"
  TSGN.Repeat _ -> "repeat"
  TSGN.Repeat1 _ -> "repeat1"
  TSGN.Field fieldName _ -> T.unpack fieldName
  TSGN.Alias _ _ aliasValue -> T.unpack aliasValue
  TSGN.Token _ -> "token"
  TSGN.ImmediateToken _ -> "immediate_token"
  TSGN.Prec _ _ -> "prec"
  TSGN.PrecLeft _ _ -> "prec_left"
  TSGN.PrecRight _ _ -> "prec_right"
  TSGN.PrecDynamic _ _ -> "prec_dynamic"
  TSGN.Reserved _ _ -> "reserved"
  TSGN.Empty -> "empty"

-- | Recursively find all CHOICE nodes within a Node, including nested ones.
-- Returns a list of ChoiceInfo with location paths.
findAllChoices :: Node -> LocationPath -> Int -> [ChoiceInfo]
findAllChoices node currentPath depth = case node of
  TSGN.Choice members ->
    -- This node IS a CHOICE - record it and continue searching in members
    let alternatives = map getAlternativeName members
        thisChoice =
          ChoiceInfo
            { choiceLocation = currentPath,
              choiceAlternatives = alternatives,
              choiceDepth = depth
            }
        -- Also search for nested CHOICEs within each alternative
        nestedChoices =
          concat $
            zipWith (\idx member -> findAllChoices member (currentPath ++ [MemberIndex idx]) (depth + 1)) [0 ..] members
     in thisChoice : nestedChoices
  TSGN.Seq members ->
    -- Search within each member of the sequence
    concat $
      zipWith (\idx member -> findAllChoices member (currentPath ++ [MemberIndex idx]) depth) [0 ..] members
  TSGN.Repeat content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.Repeat1 content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.Field fieldName content ->
    findAllChoices content (currentPath ++ [FieldName (T.unpack fieldName)]) depth
  TSGN.Alias content _ _ ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.Token content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.ImmediateToken content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.Prec _ content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.PrecLeft _ content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.PrecRight _ content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.PrecDynamic _ content ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  TSGN.Reserved content _ ->
    findAllChoices content (currentPath ++ [ContentKey]) depth
  -- Leaf nodes - no nested CHOICEs possible
  TSGN.Symbol _ -> []
  TSGN.StringLiteral _ -> []
  TSGN.Pattern _ -> []
  TSGN.Blank -> []
  TSGN.Empty -> []

-- | Find all CHOICE nodes in a grammar (across all rules)
-- Returns a map from rule name to list of ChoiceInfo within that rule.
findAllChoicesInGrammar :: Map String Node -> Map String [ChoiceInfo]
findAllChoicesInGrammar rules = Map.mapWithKey findChoicesInRule rules
  where
    findChoicesInRule :: String -> Node -> [ChoiceInfo]
    findChoicesInRule _ruleName node = findAllChoices node [] 0

-- | Enumerate all possible paths through ALL nodes in the grammar.
-- Every rule in the Map will have at least one path covering it.
-- Rules with CHOICEs will have paths for each combination of alternatives.
-- Rules without CHOICEs will have a single path to cover the rule.
-- Each path contains unique EvalPath entries (no duplicates).
enumeratePaths :: Map String Node -> [[EvalPath]]
enumeratePaths rules =
  let allChoices = findAllChoicesInGrammar rules
      -- Generate paths for each rule (every rule gets at least one path)
      rulePaths = Map.mapWithKey pathsForRule allChoices
      -- Generate all combinations across rules
      rawPaths = generateCrossRuleCombinations $ Map.toList rulePaths
   in -- Deduplicate each path to ensure unique EvalPath entries
      rawPaths
  where
    -- Generate paths for a single rule
    pathsForRule :: String -> [ChoiceInfo] -> [[EvalPath]]
    pathsForRule ruleName choices =
      if null choices
        then -- No CHOICEs: create a single path covering this rule
          [[EvalPath ruleName [] 0 ruleName]]
        else -- Has CHOICEs: generate all combinations of alternatives
          let choiceAlternatives = map (choiceToAlternatives ruleName) choices
           in generateCombinations choiceAlternatives

    -- Convert a single ChoiceInfo to list of EvalPath options (one per alternative)
    choiceToAlternatives :: String -> ChoiceInfo -> [EvalPath]
    choiceToAlternatives ruleName ChoiceInfo {..} =
      zipWith (EvalPath ruleName choiceLocation) [0 ..] choiceAlternatives

    -- Generate all combinations of alternatives across multiple choices within a rule
    generateCombinations :: [[EvalPath]] -> [[EvalPath]]
    generateCombinations [] = [[]]
    generateCombinations (firstChoices : rest) =
      [p : ps | p <- firstChoices, ps <- generateCombinations rest]

    -- Generate all combinations across rules (Cartesian product)
    generateCrossRuleCombinations :: [(String, [[EvalPath]])] -> [[EvalPath]]
    generateCrossRuleCombinations [] = [[]]
    generateCrossRuleCombinations [(_, paths)] = paths
    generateCrossRuleCombinations ((_, paths) : rest) =
      let restPaths = generateCrossRuleCombinations rest
       in [cp ++ rp | cp <- paths, rp <- restPaths]

-- | To check that whether every path of Node has correspond EvalPath matched.
-- Logically, this function expect that exists a surjective function from
-- [Node] to [[EvalPath]].
--
-- This verifies complete coverage by checking:
-- 1. Every rule in the grammar appears in every path (Cartesian product structure)
-- 2. Every CHOICE alternative appears in at least one path
-- 3. Each path has unique EvalPath entries (no duplicates)
--
-- Note: SEQ descendants are implicitly covered because:
--   - If a SEQ contains no CHOICE, it's covered by the default path
--   - If a SEQ contains a CHOICE, the CHOICE's locationPath includes the SEQ member indices
totallyCover :: Map String Node -> [[EvalPath]] -> Bool
totallyCover rules paths =
  not (null paths) && allRulesInPaths && allAlternativesCovered && allPathsHaveUniqueEntries
  where
    -- Check that every rule appears in every path (Cartesian product structure)
    allRulesInPaths :: Bool
    allRulesInPaths =
      let ruleSet = Set.fromList $ Map.keys rules
          rulesInPath path = Set.fromList $ map ruleName path
       in all (\p -> ruleSet `Set.isSubsetOf` rulesInPath p) paths

    -- Get all CHOICEs in the grammar
    allChoices :: Map String [ChoiceInfo]
    allChoices = findAllChoicesInGrammar rules

    -- Check that every alternative of every CHOICE is covered
    allAlternativesCovered :: Bool
    allAlternativesCovered =
      let expectedAlternatives = getAllExpectedAlternatives allChoices
          coveredAlternatives = getAllCoveredAlternatives paths
       in expectedAlternatives `Set.isSubsetOf` coveredAlternatives

    -- Check that each path has unique EvalPath entries
    allPathsHaveUniqueEntries :: Bool
    allPathsHaveUniqueEntries = all hasUniqueEntries paths
      where
        hasUniqueEntries path =
          let keys = map (\e -> (ruleName e, locationPath e, alternativeIndex e)) path
           in length keys == length (Set.fromList keys)

-- | Extract all expected alternatives from grammar CHOICEs
getAllExpectedAlternatives :: Map String [ChoiceInfo] -> Set.Set (String, LocationPath, Int)
getAllExpectedAlternatives choices =
  Set.fromList $
    concatMap extractFromRule (Map.toList choices)
  where
    extractFromRule :: (String, [ChoiceInfo]) -> [(String, LocationPath, Int)]
    extractFromRule (ruleName', choiceList) =
      concatMap (extractFromChoice ruleName') choiceList

    extractFromChoice :: String -> ChoiceInfo -> [(String, LocationPath, Int)]
    extractFromChoice ruleName' ChoiceInfo {..} =
      zipWith (\idx _ -> (ruleName', choiceLocation, idx)) [0 ..] choiceAlternatives

-- | Extract all covered alternatives from paths
getAllCoveredAlternatives :: [[EvalPath]] -> Set.Set (String, LocationPath, Int)
getAllCoveredAlternatives paths =
  Set.fromList $
    concatMap extractFromPath paths
  where
    extractFromPath :: [EvalPath] -> [(String, LocationPath, Int)]
    extractFromPath = map extractFromEvalPath

    extractFromEvalPath :: EvalPath -> (String, LocationPath, Int)
    extractFromEvalPath EvalPath {..} = (ruleName, locationPath, alternativeIndex)

-- | Enumerate paths for a single top-level rule (now includes nested CHOICEs)
enumerateTopLevelPaths :: String -> Node -> [[EvalPath]]
enumerateTopLevelPaths ruleName node =
  let choices = findAllChoices node [] 0
   in if null choices
        then [[EvalPath ruleName [] 0 ruleName]]
        else -- Has CHOICEs: generate all combinations
          let choiceAlternatives = map (choiceToAlternatives ruleName) choices
           in generateCombinations choiceAlternatives
  where
    choiceToAlternatives :: String -> ChoiceInfo -> [EvalPath]
    choiceToAlternatives rn ChoiceInfo {..} =
      zipWith (EvalPath rn choiceLocation) [0 ..] choiceAlternatives

    generateCombinations :: [[EvalPath]] -> [[EvalPath]]
    generateCombinations [] = [[]]
    generateCombinations (firstChoices : rest) =
      [p : ps | p <- firstChoices, ps <- generateCombinations rest]

-- | Generate TypeScript harness code for a specific evaluation path
-- Takes the grammar rules to determine correct constructor signatures
generateHarnessForPath :: T.Text -> Map String Node -> [EvalPath] -> T.Text
generateHarnessForPath templateCode rules path =
  let instantiationCodes = generateInstantiationCode rules path 0
      resultCount = length instantiationCodes
      resultVars = map (\i -> T.pack $ "result_" ++ show i) [0 .. resultCount - 1]
      resultsArray = T.concat ["const results = [", T.intercalate ", " resultVars, "];\n"]
   in T.concat
        [ templateCode,
          "\n// Test harness for path: ",
          T.pack (show path),
          "\n",
          T.concat instantiationCodes,
          resultsArray,
          "console.log(results.map(r => r.evaluate()));\n"
        ]

-- | Get the TypeScript class name for a path
classNameForPath :: EvalPath -> String
classNameForPath EvalPath {..} =
  let base = capitalize ruleName
   in case alternativeName of
        n
          | n == ruleName -> base ++ "_T"
          | otherwise -> base ++ "_" ++ n ++ "_T"

-- | Generate instantiation code for template classes
-- Each EvalPath in the list corresponds to a DIFFERENT RULE (not nested).
-- Returns a list of code snippets, one per rule/EvalPath.
generateInstantiationCode :: Map String Node -> [EvalPath] -> Int -> [T.Text]
generateInstantiationCode _ [] _ = []
generateInstantiationCode rules (p : ps) idx =
  -- Each EvalPath is for a different rule (Cartesian product across rules)
  let className = classNameForPath p
      mrule = Map.lookup (ruleName p) rules
      selectedAlt = Just (locationPath p, alternativeIndex p, alternativeName p)
      code = case mrule of
        Nothing ->
          -- Rule not found, use fallback
          T.concat ["const result_", T.pack (show idx), " = new ", T.pack className, "();\n"]
        Just rule ->
          fst $ generateNodeInstantiation className rule idx selectedAlt
   in code : generateInstantiationCode rules ps (idx + 1)

-- | Generate instantiation for a leaf or interior node
-- selectedAlt: Maybe (locationPath, alternativeIndex, alternativeName) - the selected CHOICE alternative
-- Returns (code, nextIndex) where nextIndex is the next result index to use
generateNodeInstantiation :: String -> Node -> Int -> Maybe (LocationPath, Int, String) -> (T.Text, Int)
generateNodeInstantiation className node idx selectedAlt = case node of
  TSGN.StringLiteral _ ->
    -- StringLiteral: no-arg constructor
    (T.concat ["const result_", T.pack (show idx), " = new ", T.pack className, "();\n"], idx + 1)
  TSGN.Pattern _ ->
    -- Pattern: accept value parameter
    (T.concat ["const result_", T.pack (show idx), " = new ", T.pack className, "('test_value');\n"], idx + 1)
  TSGN.Blank ->
    -- Blank: no-arg constructor
    (T.concat ["const result_", T.pack (show idx), " = new ", T.pack className, "();\n"], idx + 1)
  TSGN.Empty ->
    -- Empty: no-arg constructor
    (T.concat ["const result_", T.pack (show idx), " = new ", T.pack className, "();\n"], idx + 1)
  TSGN.Token content ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.ImmediateToken content ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.Alias content _ _ ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.Prec _ content ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.PrecLeft _ content ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.PrecRight _ content ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.PrecDynamic _ content ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.Reserved content _ ->
    generateNodeInstantiation className content idx selectedAlt
  TSGN.Seq members ->
    -- SEQ node: generate child instances and pass to constructor
    generateSeqInstantiation className members idx selectedAlt
  TSGN.Choice members ->
    -- CHOICE node: instantiate only the selected alternative
    generateChoiceInstantiation className members idx selectedAlt
  TSGN.Symbol _ ->
    -- SYMBOL: needs to be resolved, use placeholder
    (T.concat ["const result_", T.pack (show idx), " = new ", T.pack className, "(leaf);\n"], idx + 1)
  TSGN.Repeat content ->
    -- REPEAT: instantiate the content
    generateNodeInstantiation className content idx selectedAlt
  TSGN.Repeat1 content ->
    -- REPEAT1: instantiate the content
    generateNodeInstantiation className content idx selectedAlt
  TSGN.Field _ content ->
    -- FIELD: instantiate the content
    generateNodeInstantiation className content idx selectedAlt

-- | Generate instantiation for the selected alternative of a CHOICE node
-- First instantiates children of the alternative, then creates the CHOICE instance.
-- selectedAlt: Maybe (locationPath, alternativeIndex, alternativeName)
-- Returns (code, nextIndex) where nextIndex is the next result index to use
generateChoiceInstantiation :: String -> [Node] -> Int -> Maybe (LocationPath, Int, String) -> (T.Text, Int)
generateChoiceInstantiation className alternatives idx selectedAlt =
  case selectedAlt of
    Nothing ->
      -- No selection info: instantiate first alternative as fallback
      instantiateAlternative 0 idx
    Just (_, altIdx, _) ->
      -- Instantiate only the selected alternative
      instantiateAlternative altIdx idx
  where
    instantiateAlternative :: Int -> Int -> (T.Text, Int)
    instantiateAlternative altIdx currentIdx =
      if altIdx >= 0 && altIdx < length alternatives
        then
          let member = alternatives !! altIdx
              -- First, instantiate any children the alternative needs
              (childCode, nextIdx) = instantiateAlternativeChildren member currentIdx
              -- Then create the choice instance
              choiceCode = T.concat ["const result_", T.pack (show nextIdx), " = new ", T.pack className, "();\n"]
           in (childCode `T.append` choiceCode, nextIdx + 1)
        else
          -- Fallback: empty instantiation
          (T.concat ["const result_", T.pack (show currentIdx), " = new ", T.pack className, "();\n"], currentIdx + 1)

    instantiateAlternativeChildren :: Node -> Int -> (T.Text, Int)
    instantiateAlternativeChildren member currentIdx = case member of
      TSGN.Seq seqMembers ->
        -- SEQ alternative: instantiate each member
        let (childDecls, finalIdx) = instantiateSeqMembers seqMembers currentIdx
         in (childDecls, finalIdx)
      TSGN.Choice nestedMembers ->
        -- Nested CHOICE: recursively handle
        generateChoiceInstantiation className nestedMembers currentIdx selectedAlt
      TSGN.Repeat content ->
        -- REPEAT: instantiate the content
        instantiateAlternativeChildren content currentIdx
      TSGN.Repeat1 content ->
        -- REPEAT1: instantiate the content
        instantiateAlternativeChildren content currentIdx
      TSGN.Token content ->
        instantiateAlternativeChildren content currentIdx
      TSGN.ImmediateToken content ->
        instantiateAlternativeChildren content currentIdx
      TSGN.Alias content _ _ ->
        instantiateAlternativeChildren content currentIdx
      TSGN.Prec _ content ->
        instantiateAlternativeChildren content currentIdx
      TSGN.PrecLeft _ content ->
        instantiateAlternativeChildren content currentIdx
      TSGN.PrecRight _ content ->
        instantiateAlternativeChildren content currentIdx
      TSGN.PrecDynamic _ content ->
        instantiateAlternativeChildren content currentIdx
      TSGN.Reserved content _ ->
        instantiateAlternativeChildren content currentIdx
      TSGN.Field _ content ->
        instantiateAlternativeChildren content currentIdx
      TSGN.Symbol _ ->
        -- SYMBOL: leaf node, no children to instantiate
        (T.empty, currentIdx)
      TSGN.StringLiteral _ ->
        -- StringLiteral: leaf node
        (T.empty, currentIdx)
      TSGN.Pattern _ ->
        -- Pattern: leaf node
        (T.empty, currentIdx)
      TSGN.Blank ->
        -- Blank: leaf node
        (T.empty, currentIdx)
      TSGN.Empty ->
        -- Empty: leaf node
        (T.empty, currentIdx)

    instantiateSeqMembers :: [Node] -> Int -> (T.Text, Int)
    instantiateSeqMembers seqMembers startIdx =
      let results =
            scanl
              ( \(code, currentIdx) node ->
                  let (childCode, nextIdx) = instantiateMember node currentIdx
                   in (code `T.append` childCode, nextIdx)
              )
              (T.empty, startIdx)
              seqMembers
          (finalCode, finalIdx) = last results
       in (finalCode, finalIdx)

    instantiateMember :: Node -> Int -> (T.Text, Int)
    instantiateMember member currentIdx = case member of
      TSGN.Symbol name ->
        let childClass = capitalize (T.unpack name) ++ "_T"
         in (T.concat ["const child_", T.pack (show currentIdx), " = new ", T.pack childClass, "();\n"], currentIdx + 1)
      TSGN.StringLiteral _ ->
        (T.concat ["const child_", T.pack (show currentIdx), " = new SyntaticLeaf('literal');\n"], currentIdx + 1)
      TSGN.Pattern _ ->
        (T.concat ["const child_", T.pack (show currentIdx), " = new SyntaticLeaf('test');\n"], currentIdx + 1)
      TSGN.Blank ->
        (T.concat ["const child_", T.pack (show currentIdx), " = new SyntaticLeaf('');\n"], currentIdx + 1)
      TSGN.Empty ->
        (T.concat ["const child_", T.pack (show currentIdx), " = new SyntaticLeaf('');\n"], currentIdx + 1)
      TSGN.Choice nestedMembers ->
        -- Nested CHOICE within alternative
        generateChoiceInstantiation "NestedChoice_T" nestedMembers currentIdx selectedAlt
      TSGN.Seq seqMembers ->
        -- SEQ within SEQ member
        generateSeqInstantiation "SeqMember_T" seqMembers currentIdx selectedAlt
      TSGN.Repeat content ->
        -- REPEAT: instantiate the content
        instantiateMember content currentIdx
      TSGN.Repeat1 content ->
        -- REPEAT1: instantiate the content
        instantiateMember content currentIdx
      TSGN.Token content ->
        instantiateMember content currentIdx
      TSGN.ImmediateToken content ->
        instantiateMember content currentIdx
      TSGN.Field _ content ->
        instantiateMember content currentIdx
      TSGN.Alias content _ _ ->
        instantiateMember content currentIdx
      TSGN.Prec _ content ->
        instantiateMember content currentIdx
      TSGN.PrecLeft _ content ->
        instantiateMember content currentIdx
      TSGN.PrecRight _ content ->
        instantiateMember content currentIdx
      TSGN.PrecDynamic _ content ->
        instantiateMember content currentIdx
      TSGN.Reserved content _ ->
        instantiateMember content currentIdx

-- | Generate instantiation for a SEQ node with multiple children
-- Instantiates each child before creating the SEQ instance.
-- selectedAlt is passed through for any nested CHOICE nodes
-- Returns (code, nextIndex) where nextIndex is the next result index to use
generateSeqInstantiation :: String -> [Node] -> Int -> Maybe (LocationPath, Int, String) -> (T.Text, Int)
generateSeqInstantiation className members idx selectedAlt =
  let -- Instantiate each child and collect results
      (childCode, childArgs, finalIdx) = instantiateChildren members idx
      -- Create the SEQ instance
      seqCode = T.concat ["const result_", T.pack (show finalIdx), " = new ", T.pack className, "(", T.intercalate ", " childArgs, ");\n"]
   in (childCode `T.append` seqCode, finalIdx + 1)
  where
    instantiateChildren :: [Node] -> Int -> (T.Text, [T.Text], Int)
    instantiateChildren [] currentIdx = (T.empty, [], currentIdx)
    instantiateChildren (member : rest) currentIdx =
      let (memberCode, memberArg, nextIdx) = instantiateChild member currentIdx
          (restCode, restArgs, finalIdx) = instantiateChildren rest nextIdx
       in (memberCode `T.append` restCode, memberArg : restArgs, finalIdx)

    instantiateChild :: Node -> Int -> (T.Text, T.Text, Int)
    instantiateChild member currentIdx = case member of
      TSGN.Symbol name ->
        let childClass = capitalize (T.unpack name) ++ "_T"
            arg = T.concat ["child_", T.pack (show currentIdx)]
         in (T.concat ["const ", arg, " = new ", T.pack childClass, "();\n"], arg, currentIdx + 1)
      TSGN.StringLiteral _ ->
        let arg = T.concat ["child_", T.pack (show currentIdx)]
         in (T.concat ["const ", arg, " = new SyntaticLeaf('literal');\n"], arg, currentIdx + 1)
      TSGN.Pattern _ ->
        let arg = T.concat ["child_", T.pack (show currentIdx)]
         in (T.concat ["const ", arg, " = new SyntaticLeaf('test');\n"], arg, currentIdx + 1)
      TSGN.Blank ->
        let arg = T.concat ["child_", T.pack (show currentIdx)]
         in (T.concat ["const ", arg, " = new SyntaticLeaf('');\n"], arg, currentIdx + 1)
      TSGN.Empty ->
        let arg = T.concat ["child_", T.pack (show currentIdx)]
         in (T.concat ["const ", arg, " = new SyntaticLeaf('');\n"], arg, currentIdx + 1)
      TSGN.Choice choiceMembers ->
        -- CHOICE within SEQ: instantiate the selected alternative
        let arg = T.concat ["result_", T.pack (show currentIdx)]
            (code, nextIdx) = generateChoiceInstantiation "ChoiceMember_T" choiceMembers currentIdx selectedAlt
         in (code, arg, nextIdx)
      TSGN.Seq seqMembers ->
        -- SEQ within SEQ: recursively instantiate
        let arg = T.concat ["result_", T.pack (show currentIdx)]
            (code, nextIdx) = generateSeqInstantiation "SeqMember_T" seqMembers currentIdx selectedAlt
         in (code, arg, nextIdx)
      TSGN.Repeat content ->
        -- REPEAT: instantiate the content
        instantiateChild content currentIdx
      TSGN.Repeat1 content ->
        -- REPEAT1: instantiate the content
        instantiateChild content currentIdx
      TSGN.Token content ->
        instantiateChild content currentIdx
      TSGN.ImmediateToken content ->
        instantiateChild content currentIdx
      TSGN.Field _ content ->
        instantiateChild content currentIdx
      TSGN.Alias content _ _ ->
        instantiateChild content currentIdx
      TSGN.Prec _ content ->
        instantiateChild content currentIdx
      TSGN.PrecLeft _ content ->
        instantiateChild content currentIdx
      TSGN.PrecRight _ content ->
        instantiateChild content currentIdx
      TSGN.PrecDynamic _ content ->
        instantiateChild content currentIdx
      TSGN.Reserved content _ ->
        instantiateChild content currentIdx

-- | Execute a specific evaluation path and return the state (without context)
executePath ::
  T.Text ->
  Map String Node ->
  [EvalPath] ->
  String ->
  IO (Either String String)
executePath templateCode rules path testName = do
  let sanitizedTestName = map sanitizeChar testName
      outDir = "/tmp/eval_iter_" ++ sanitizedTestName
      harnessCode = generateHarnessForPath templateCode rules path

  -- Create output directory
  _ <- readProcessWithExitCode "mkdir" ["-p", outDir] ""

  -- Write TypeScript files
  TIO.writeFile (outDir ++ "/template.ts") templateCode
  TIO.writeFile (outDir ++ "/harness.ts") harnessCode

  -- Compile TypeScript
  (compileExitCode, _compileStdout, compileStderr) <-
    readProcessWithExitCode
      "tsc"
      [ "--target",
        "ES2020",
        "--module",
        "commonjs",
        "--moduleResolution",
        "node",
        "--outDir",
        outDir,
        outDir ++ "/template.ts",
        outDir ++ "/harness.ts"
      ]
      ""

  case compileExitCode of
    ExitFailure code -> do
      return $
        Left $
          "TypeScript compilation failed: "
            ++ show code
            ++ "\n"
            ++ if null compileStderr
              then _compileStdout
              else compileStderr
    ExitSuccess -> do
      -- Execute with Node.js
      (runExitCode, runStdout, runStderr) <-
        readProcessWithExitCode "node" [outDir ++ "/harness.js"] ""

      case runExitCode of
        ExitFailure code -> do
          return $
            Left $
              "Node.js execution failed: "
                ++ show code
                ++ "\n"
                ++ if null runStderr
                  then runStdout
                  else runStderr
        ExitSuccess ->
          return $ Right runStdout
  where
    sanitizeChar c
      | c `elem` (" -()" :: String) = '_'
      | otherwise = c

-- | Iterate through all possible evaluation paths in a grammar.
-- Accepts a custom evaluation function that receives the execution result
-- (stdout) from running the harness code for each path.
--
-- Usage:
-- @
--   result <- iterateAllEvaluations "grammar.json" $ \output -> do
--     -- Custom evaluation logic on harness execution output
--     putStrLn $ "Got: " ++ output
--     return $ parseResult output
-- @
iterateAllEvaluations ::
  FilePath ->
  (String -> IO a) ->
  IO (Either String [a])
iterateAllEvaluations grammarPath evalFunc =
  iterateAllEvaluationsWithContext grammarPath noOpContextProvider evalFunc

-- | Iterate through all possible evaluation paths with context provider.
-- Verifies that all paths are covered before executing evaluations.
-- Accepts a custom evaluation function that receives the execution result
-- (stdout) from running the harness code for each path.
iterateAllEvaluationsWithContext ::
  FilePath ->
  ContextProvider ->
  (String -> IO a) ->
  IO (Either String [a])
iterateAllEvaluationsWithContext grammarPath contextProvider evalFunc = do
  result <- runMaybeT $ TSGN.parseGrammarFromFile grammarPath
  case result of
    Nothing -> return $ Left $ "Failed to parse grammar: " ++ grammarPath
    Just grammar -> do
      -- Use the original grammar (after convert/resolveAlias) for path enumeration
      -- This preserves the CHOICE structure for enumeration
      let originalRules = TSGN.grammarNodes (TSGN.orig (TSGN.convert grammar))
          paths = enumeratePaths originalRules
          templateCode = T.pack $ descript grammar

      -- Verify that all paths are covered
      if not (totallyCover originalRules paths)
        then return $ Left "Path coverage verification failed: not all grammar paths are covered by enumerated paths"
        else do
          -- Execute each path and collect results
          results <- mapM (executeAndEvaluate originalRules templateCode contextProvider) (zip [0 ..] paths)
          return $ Right results
  where
    executeAndEvaluate rules templateCode ctxProvider (idx, path) = do
      let testName = "path_" ++ show idx
      execResult <- executePath templateCode rules path testName
      case execResult of
        Left err -> do
          -- Apply context provider to error message for consistency
          let _codeWithContext = provideContext ctxProvider err
          evalFunc $ "ERROR: " ++ err
        Right output -> do
          -- Apply context provider (for logging/debugging)
          let _codeWithContext = provideContext ctxProvider output
          -- Apply user's evaluation function to the execution output
          evalFunc output

-- | Generate test cases from grammar file (without context)
-- Uses default TypeScript compilation and execution as the evaluation method.
generateTestCases ::
  FilePath ->
  IO (Either String [Bool])
generateTestCases grammarPath = do
  result <- iterateAllEvaluations grammarPath $ \output -> do
    -- Default evaluation: check if output is non-empty (indicates success)
    return $ not (null output)
  case result of
    Left err -> return $ Left err
    Right successes -> return $ Right successes

-- | Generate test cases from grammar file with context provider
-- Uses default TypeScript compilation and execution as the evaluation method.
generateTestCasesWithContext ::
  FilePath ->
  ContextProvider ->
  IO (Either String [Bool])
generateTestCasesWithContext grammarPath contextProvider = do
  result <- iterateAllEvaluationsWithContext grammarPath contextProvider $ \output -> do
    -- Default evaluation: check if output is non-empty (indicates success)
    return $ not (null output)
  case result of
    Left err -> return $ Left err
    Right successes -> return $ Right successes

-- | Convert EvalStates to a TestTree (for integration with Tasty)
evalStatesToTestTree :: [EvalState] -> IO ()
evalStatesToTestTree states = do
  putStrLn $ "Total paths: " ++ show (length states)
  putStrLn $ "Successful: " ++ show (length (filter compilable states))
  putStrLn $ "Failed: " ++ show (length (filter (not . compilable) states))
  mapM_ printState states
  where
    printState :: EvalState -> IO ()
    printState EvalState {..} = do
      putStrLn $ "\n=== Path: " ++ show path ++ " ==="
      putStrLn $ "Generated: " ++ generatedCode
      putStrLn $ "Compilable: " ++ show compilable
      case compilationError of
        Just err -> putStrLn $ "Error: " ++ take 500 err -- Truncate long errors
        Nothing -> return ()

-- | Helper to capitalize first character
capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs
  where
    toUpper ch
      | ch >= 'a' && ch <= 'z' = toEnum (fromEnum ch - 32)
      | otherwise = ch
