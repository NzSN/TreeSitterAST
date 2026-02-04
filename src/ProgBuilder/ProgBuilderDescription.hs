{-# LANGUAGE PatternGuards #-}
module ProgBuilder.ProgBuilderDescription
(Property(..),
 propsOfNode,
) where

import qualified Data.Text.Lazy as T

import TreeSitterGrammarNodes qualified as TSGN

data Property =
  StrProp    {str_value :: T.Text} |
  SymbolProp {p_type :: T.Text}    |
  NamedProp  {p_name :: T.Text, p_types :: [Property]}
propsOfNode :: TSGN.Node -> [Property]
propsOfNode x
  | (TSGN.Seq ns)           <- x = concatMap propsOfNode ns
  | (TSGN.Choice ns)        <- x = concatMap propsOfNode ns
  | (TSGN.Repeat n)         <- x = propsOfNode n
  | (TSGN.Repeat1 n)        <- x = propsOfNode n
  | (TSGN.Symbol n)         <- x = [SymbolProp n]
  | (TSGN.StringLiteral n)  <- x = [StrProp n]
  | (TSGN.Token n)          <- x = propsOfNode n
  | (TSGN.ImmediateToken n) <- x = propsOfNode n
  | (TSGN.Field f_name n)   <- x = [NamedProp f_name $ propsOfNode n]
  | (TSGN.Prec _ n)         <- x = propsOfNode n
  | (TSGN.PrecLeft _ n)     <- x = propsOfNode n
  | (TSGN.PrecRight _ n)    <- x = propsOfNode n
  | (TSGN.PrecDynamic _ n)  <- x = propsOfNode n
  | (TSGN.Reserved n _)     <- x = propsOfNode n
  | _                       <- x = []
