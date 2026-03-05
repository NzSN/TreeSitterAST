{-# LANGUAGE PatternGuards #-}

module ProgBuilder.ProgBuilderDescription
  ( Property (..),
    propsOfNode,
    uniqueBranch,
    convergeNamedProp,
    propType,
    toGrammarNodeWithProp,
    extractProperties,
    GrammarNodeWithProp,
  )
where

import Data.List (nub, partition)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as T
import TreeSitterGrammarNodes qualified as TSGN

-- | Generation hints for properties
data GenerationHint
  = LiteralHint T.Text  -- ^ Literal string value
  | PatternHint T.Text  -- ^ Regex pattern
  | SymbolHint T.Text   -- ^ Symbol reference
  | FieldHint T.Text    -- ^ Field name
  | DefaultHint T.Text  -- ^ Default value
  | RangeHint Int Int   -- ^ Min/max range (for repetitions)
  | ChoiceHint [T.Text] -- ^ Possible values (for choices)
  deriving (Eq, Ord, Show)

data Property
  = StrProp {str_value :: T.Text, hint :: Maybe GenerationHint}
  | SymbolProp {p_type :: T.Text, hint :: Maybe GenerationHint}
  | NamedProp {p_name :: T.Text, p_types :: [Property], hint :: Maybe GenerationHint}
  deriving (Eq, Ord, Show)
type GrammarNodeWithProp = TSGN.GrammarNode Property

propType :: Property -> Maybe T.Text
propType x
  | (StrProp _ _) <- x = Nothing
  | (SymbolProp s _) <- x = Just s
  | (NamedProp s _ _) <- x = Just s

-- | Extract properties from a grammar node via an intermediate annotated tree.
-- First generate a GrammarNodeWithProp where leaf values are Property,
-- then extract the flattened list.
propsOfNode :: TSGN.Node -> [Property]
propsOfNode = extractProperties . toGrammarNodeWithProp

toGrammarNodeWithProp :: TSGN.Node -> GrammarNodeWithProp
toGrammarNodeWithProp x
  | (TSGN.Seq ns) <- x = TSGN.Seq (map toGrammarNodeWithProp ns)
  | (TSGN.Choice ns) <- x = TSGN.Choice (map toGrammarNodeWithProp ns)
  | (TSGN.Repeat n) <- x = TSGN.Repeat (toGrammarNodeWithProp n)
  | (TSGN.Repeat1 n) <- x = TSGN.Repeat1 (toGrammarNodeWithProp n)
  | (TSGN.Symbol n) <- x = TSGN.Symbol (SymbolProp n (Just (SymbolHint n)))
  | (TSGN.StringLiteral n) <- x = TSGN.StringLiteral (StrProp n (Just (LiteralHint n)))
  | (TSGN.Pattern n) <- x = TSGN.Pattern (StrProp n (Just (PatternHint n)))
  | (TSGN.Token n) <- x = toGrammarNodeWithProp n
  | (TSGN.ImmediateToken n) <- x = toGrammarNodeWithProp n
  | (TSGN.Field f_name n) <- x =
      let fieldNameProp = StrProp f_name (Just (FieldHint f_name))
          contentNode = toGrammarNodeWithProp n
      in TSGN.Field fieldNameProp contentNode
  | (TSGN.Prec _ n) <- x = toGrammarNodeWithProp n
  | (TSGN.PrecLeft _ n) <- x = toGrammarNodeWithProp n
  | (TSGN.PrecRight _ n) <- x = toGrammarNodeWithProp n
  | (TSGN.PrecDynamic _ n) <- x = toGrammarNodeWithProp n
  | (TSGN.Reserved n _) <- x = toGrammarNodeWithProp n
  -- Named alias
  | (TSGN.Alias n True _) <- x = toGrammarNodeWithProp n
  -- Literal alias
  | (TSGN.Alias n False _) <- x = toGrammarNodeWithProp n
  | otherwise = TSGN.Empty

extractProperties :: GrammarNodeWithProp -> [Property]
extractProperties x = case x of
  TSGN.Symbol prop -> [prop]
  TSGN.StringLiteral prop -> [prop]
  TSGN.Pattern prop -> [prop]
  TSGN.Blank -> []
  TSGN.Empty -> []
  TSGN.Seq ns -> seqProc (map extractProperties ns)
  TSGN.Choice ns -> choiceProc (map extractProperties ns)
  TSGN.Repeat n -> extractProperties n
  TSGN.Repeat1 n -> extractProperties n
  TSGN.Token n -> extractProperties n
  TSGN.ImmediateToken n -> extractProperties n
  TSGN.Field fieldNameProp contentNode ->
      let props = extractProperties contentNode
      in case fieldNameProp of
            StrProp name _ -> [NamedProp name props (Just (FieldHint name))]
            _ -> error "fieldNameProp must be StrProp"
  TSGN.Prec _ n -> extractProperties n
  TSGN.PrecLeft _ n -> extractProperties n
  TSGN.PrecRight _ n -> extractProperties n
  TSGN.PrecDynamic _ n -> extractProperties n
  TSGN.Reserved n _ -> extractProperties n
  TSGN.Alias n _ _ -> extractProperties n
  where
    choiceProc :: [[Property]] -> [Property]
    choiceProc = concat . convergeNamedProp . uniqueBranch

    seqProc :: [[Property]] -> [Property]
    seqProc = concat

-- | Proc utilities section

-- | Transform input A :: [[Property]] into a form where
-- for every a,b in A, intersection of a and b is empty.
uniqueBranch :: [[Property]] -> [[Property]]
uniqueBranch branches = zipWith (curry keepAssigned) [0 ..] branches
  where
    -- Build map from element to list of branch indices where it appears
    elementMap :: Map.Map Property [Int]
    elementMap =
      Map.fromListWith (++) $
        concatMap (\(i, bs) -> map (,[i]) bs) (zip [0 ..] branches)

    -- For each element, choose the minimum branch index
    assignment :: Map.Map Property Int
    assignment = Map.map minimum elementMap

    -- For each branch, keep elements where this branch is chosen
    keepAssigned :: (Int, [Property]) -> [Property]
    keepAssigned (i, bs) = nub (filter (\b -> assignment Map.! b == i) bs)

-- | For NamedProps that with same prop name but differ prop type,
-- this function merge those NamedProps's prop types.
-- Merges NamedProps across branches (not just within each branch).
convergeNamedProp :: [[Property]] -> [[Property]]
convergeNamedProp branches =
  let -- Step 1: Recursively merge NamedProps within each branch (including nested)
      processedBranches = map mergeNamedPropsInArray branches

      -- Step 2: Extract top-level NamedProps from each branch
      (branchNamedProps, branchNonNamedProps) =
        unzip $
          map (partition isNamedProp) processedBranches

      -- Step 3: Merge top-level NamedProps across branches
      (mergedTopNamedProps, nameToMinBranch) = mergeTopNamedPropsAcrossBranches branchNamedProps

      -- Step 4: Reassign merged top-level NamedProps to branches based on minimum branch index
      resultBranches =
        zipWith
          ( \i nonNamed ->
              nonNamed
                ++ [ prop
                   | (name, prop) <- Map.toList mergedTopNamedProps,
                     nameToMinBranch Map.! name == i
                   ]
          )
          [0 ..]
          branchNonNamedProps
   in resultBranches
  where
    -- Helper to check if a property is a NamedProp
    isNamedProp :: Property -> Bool
    isNamedProp (NamedProp _ _ _) = True
    isNamedProp _ = False

    -- Merge top-level NamedProps across branches, returning:
    -- (Map name -> merged NamedProp, Map name -> minimum branch index)
    mergeTopNamedPropsAcrossBranches :: [[Property]] -> (Map.Map T.Text Property, Map.Map T.Text Int)
    mergeTopNamedPropsAcrossBranches branchNamedProps =
      let -- Collect all NamedProps with their branch indices
          namedPropsWithBranches =
            concat $
              zipWith (\i props -> map (i,) props) [0 ..] branchNamedProps

          -- Build map: name -> (accumulated types, list of branch indices)
          nameAccum =
            foldr
              ( \(i, prop) acc -> case prop of
                  NamedProp name types _ ->
                    let (typesAccum, brs) = Map.findWithDefault ([], []) name acc
                        newTypesAccum = nub (typesAccum ++ types)
                        newBranches = i : brs
                     in Map.insert name (newTypesAccum, newBranches) acc
                  _ -> acc
              )
              Map.empty
              namedPropsWithBranches

          -- Convert to final maps
          mergedMap = Map.map (\(typesAccum, _) -> NamedProp (T.pack "") typesAccum Nothing) nameAccum
          -- Fix the names (they were lost in the map)
          mergedMapWithNames = Map.mapWithKey (\name prop -> case prop of NamedProp _ types hint -> processProperty (NamedProp name types hint); _ -> prop) mergedMap
          minBranchMap = Map.map (\(_, brs) -> minimum (brs :: [Int])) nameAccum
       in (mergedMapWithNames, minBranchMap)

    -- Recursively merge NamedProps within an array (including nested arrays)
    -- This is the original per-array merging logic
    mergeNamedPropsInArray :: [Property] -> [Property]
    mergeNamedPropsInArray props =
      let processed = map processProperty props
          (namedProps, others) = partitionNamedProps processed
          mergedNamedProps = Map.elems $ foldr insertNamedProp Map.empty namedProps
       in others ++ mergedNamedProps

    -- Process a single property (recursive)
    processProperty :: Property -> Property
    processProperty prop = case prop of
      NamedProp name types hint ->
        let processedTypes = mergeNamedPropsInArray types
         in NamedProp name processedTypes hint
      other -> other

    -- Partition properties into NamedProps and others
    partitionNamedProps :: [Property] -> ([Property], [Property])
    partitionNamedProps = foldr splitter ([], [])
      where
        splitter p@(NamedProp _ _ _) (named, others) = (p : named, others)
        splitter p (named, others) = (named, p : others)

    -- Insert a NamedProp into a map keyed by p_name, merging p_types
    insertNamedProp :: Property -> Map.Map T.Text Property -> Map.Map T.Text Property
    insertNamedProp (NamedProp name types hint) acc =
      Map.insertWith mergeTypes name (NamedProp name types hint) acc
    insertNamedProp _ acc = acc -- Should never happen, only NamedProps passed

    -- Merge two NamedProps with same name by concatenating and deduplicating their p_types
    -- Map.insertWith calls mergeTypes newValue oldValue
    mergeTypes :: Property -> Property -> Property
    mergeTypes newValue oldValue = case (newValue, oldValue) of
      (NamedProp name1 types1 hint1, NamedProp _ types2 hint2) ->
        -- Keep hint from new value, or old if new has none
        let hint = case hint1 of
              Nothing -> hint2
              Just _ -> hint1
        in NamedProp name1 (nub (types2 ++ types1)) hint -- old types then new types
      _ -> error "mergeTypes: expected NamedProp"
