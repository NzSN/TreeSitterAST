{-# LANGUAGE PatternGuards #-}
module ProgBuilder.ProgBuilderDescription
(Property(..),
 propsOfNode,
) where

import TreeSitterGrammarNodes qualified as TSGN

data Property =
  SymbolProp      {p_type :: String} |
  StrProp   {str_value :: String} |
  NamedProp {p_name :: String, p_types :: [Property]}
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
