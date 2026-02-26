{-# LANGUAGE PatternGuards #-}

module ProgBuilder.ProgBuilderDescription
  ( Property (..),
    propsOfNode,
    uniqueBranch,
    convergeNamedProp,
    propIdent,
  )
where

import Data.List (nub, partition)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as T
import TreeSitterGrammarNodes qualified as TSGN

data Property
  = StrProp {str_value :: T.Text}
  | SymbolProp {p_type :: T.Text}
  | NamedProp {p_name :: T.Text, p_types :: [Property]}
  deriving (Eq, Ord, Show)

propIdent :: Property -> T.Text
propIdent x
  | (StrProp s) <- x = s
  | (SymbolProp s) <- x = s
  | (NamedProp s _) <- x = s

propsOfNode :: TSGN.Node -> [Property]
propsOfNode x
  | (TSGN.Seq ns) <- x = seqProc $ map propsOfNode ns
  | (TSGN.Choice ns) <- x = choiceProc $ map propsOfNode ns
  | (TSGN.Repeat n) <- x = propsOfNode n
  | (TSGN.Repeat1 n) <- x = propsOfNode n
  | (TSGN.Symbol n) <- x = [SymbolProp n]
  | (TSGN.StringLiteral n) <- x = [StrProp n]
  | (TSGN.Token n) <- x = propsOfNode n
  | (TSGN.ImmediateToken n) <- x = propsOfNode n
  | (TSGN.Field f_name n) <- x = [NamedProp f_name $ propsOfNode n]
  | (TSGN.Prec _ n) <- x = propsOfNode n
  | (TSGN.PrecLeft _ n) <- x = propsOfNode n
  | (TSGN.PrecRight _ n) <- x = propsOfNode n
  | (TSGN.PrecDynamic _ n) <- x = propsOfNode n
  | (TSGN.Reserved n _) <- x = propsOfNode n
  -- Named alias
  | (TSGN.Alias n True _) <- x = propsOfNode n
  -- Literal alias
  | (TSGN.Alias n False _) <- x = propsOfNode n
  | _ <- x = []
  where
    -- \| Information of how choice and seq is organized will lost
    -- after propsOfNode, hence those processing that required
    -- these information will be handled here.
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
    isNamedProp (NamedProp _ _) = True
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
                  NamedProp name types ->
                    let (typesAccum, brs) = Map.findWithDefault ([], []) name acc
                        newTypesAccum = nub (typesAccum ++ types)
                        newBranches = i : brs
                     in Map.insert name (newTypesAccum, newBranches) acc
                  _ -> acc
              )
              Map.empty
              namedPropsWithBranches

          -- Convert to final maps
          mergedMap = Map.map (\(typesAccum, _) -> NamedProp (T.pack "") typesAccum) nameAccum
          -- Fix the names (they were lost in the map)
          mergedMapWithNames = Map.mapWithKey (\name prop -> case prop of NamedProp _ types -> processProperty (NamedProp name types); _ -> prop) mergedMap
          minBranchMap = Map.map (\(_, brs) -> minimum brs) nameAccum
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
      NamedProp name types ->
        let processedTypes = mergeNamedPropsInArray types
         in NamedProp name processedTypes
      other -> other

    -- Partition properties into NamedProps and others
    partitionNamedProps :: [Property] -> ([Property], [Property])
    partitionNamedProps = foldr splitter ([], [])
      where
        splitter p@(NamedProp _ _) (named, others) = (p : named, others)
        splitter p (named, others) = (named, p : others)

    -- Insert a NamedProp into a map keyed by p_name, merging p_types
    insertNamedProp :: Property -> Map.Map T.Text Property -> Map.Map T.Text Property
    insertNamedProp (NamedProp name types) acc =
      Map.insertWith mergeTypes name (NamedProp name types) acc
    insertNamedProp _ acc = acc -- Should never happen, only NamedProps passed

    -- Merge two NamedProps with same name by concatenating and deduplicating their p_types
    -- Map.insertWith calls mergeTypes newValue oldValue
    mergeTypes :: Property -> Property -> Property
    mergeTypes newValue oldValue = case (newValue, oldValue) of
      (NamedProp name1 types1, NamedProp _ types2) ->
        NamedProp name1 (nub (types2 ++ types1)) -- old types then new types
      _ -> error "mergeTypes: expected NamedProp"
