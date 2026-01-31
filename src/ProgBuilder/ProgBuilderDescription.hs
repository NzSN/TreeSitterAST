{-# LANGUAGE MultilineStrings #-}

module ProgBuilder.ProgBuilderDescription
  ( Interior (..),
    Leaf (..),
    fromTSGN,
  )
where

import TreeSitterGrammarNodes qualified as TSGN

-- Interior Literal suppose to correspond to
-- leaf nodes.
data Leaf
  = InteriorLiteral {prop_name :: String}
  | InteriorRef     {prop_ref :: String}
  deriving (Show, Eq, Ord)

data Interior
  = Leaf    {eval :: Leaf}
  | Seq     {props_seq :: [Interior]}
  | Repeat  {prop :: Interior}
  | Repeat1 {prop_repeat1 :: Interior}
  | Choice  {props :: [Interior]}
  | Field   {field_name :: String,
              named_prop :: Interior}
  | Empty
  deriving (Eq, Ord, Show)

fromTSGN :: TSGN.Node -> Interior
fromTSGN (TSGN.Seq members) =
  Seq $ map fromTSGN members
fromTSGN (TSGN.Choice ns) = Choice (map fromTSGN ns)
fromTSGN (TSGN.Repeat n) = Repeat $
                                 case fromTSGN n of
                                   (Repeat n_) -> n_
                                   (Repeat1 n_) -> n_
                                   p -> p
fromTSGN (TSGN.Repeat1 n) = Repeat1 $
                            case fromTSGN n of
                              (Repeat n_)-> n_
                              (Repeat1 n_) -> n_
                              p -> p
fromTSGN (TSGN.StringLiteral str) = Leaf $ InteriorLiteral str
fromTSGN (TSGN.Field field_name field_type) = Field field_name $ fromTSGN field_type
fromTSGN (TSGN.Symbol content) = Leaf (InteriorRef content)
fromTSGN (TSGN.Prec _ content) = fromTSGN content
fromTSGN (TSGN.PrecLeft _ content) = fromTSGN content
fromTSGN (TSGN.PrecRight _ content) = fromTSGN content
fromTSGN (TSGN.PrecDynamic _ content) = fromTSGN content
fromTSGN (TSGN.Token content) = fromTSGN content
fromTSGN (TSGN.ImmediateToken content) = fromTSGN content
fromTSGN (TSGN.Alias content _ _) = fromTSGN content
fromTSGN (TSGN.Reserved content _) = fromTSGN content
fromTSGN _ = Empty
