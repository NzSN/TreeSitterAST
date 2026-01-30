{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module ProgBuilder.ProgBuilderDescription
  ( Property (..),
    PropertyVar (..),
    propsOfNode,
  )
where

import TreeSitterGrammarNodes qualified as TSGN

-- Property Literal suppose to correspond to
-- leaf nodes.
data PropertyVar
  = PropertyLiteral { prop_name :: String }
    | PropertyRef { prop_ref :: String }
    | PropertyField {
        prop_field_name :: String,
        prop_field_type :: String }
  deriving (Show, Eq, Ord)

data Property
  = Property {eval :: PropertyVar}
  | Repeat {prop :: Property}
  | Choice {props :: [Property]}
  deriving (Eq, Ord, Show)

propsOfNode :: TSGN.Node -> [Property]
propsOfNode (TSGN.Seq members) =
  concatMap propsOfNode members
propsOfNode (TSGN.Choice ns) = [Choice (concatMap propsOfNode ns)]
propsOfNode n
  | (TSGN.Repeat content) <- n = withRepeat content
  | (TSGN.Repeat1 content) <- n = withRepeat content
  where
    withRepeat :: TSGN.Node -> [Property]
    withRepeat node =
      [ Repeat
          ( case propsOfNode node of
              -- PRECONDITION: Repeat must has only one sub-rule.
              [] -> error "Invalid Syntax"
              (Repeat _) : _ -> error "Nested Repeat is not expected"
              p@(Property _) : _ -> p
              p@(Choice _) : _ -> p
          )
      ]
propsOfNode (TSGN.Field field_name field_type) = [Property (PropertyField field_name field_type)]
propsOfNode (TSGN.Symbol content) = [Property (PropertyLiteral content)]
propsOfNode (TSGN.Prec _ content) = propsOfNode content
propsOfNode (TSGN.PrecLeft _ content) = propsOfNode content
propsOfNode (TSGN.PrecRight _ content) = propsOfNode content
propsOfNode (TSGN.PrecDynamic _ content) = propsOfNode content
propsOfNode (TSGN.Token content) = propsOfNode content
propsOfNode (TSGN.ImmediateToken content) = propsOfNode content
propsOfNode (TSGN.Alias content _ _) = propsOfNode content
propsOfNode (TSGN.Reserved content _) = propsOfNode content
propsOfNode _ = []
