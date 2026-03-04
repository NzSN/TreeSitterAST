{-# LANGUAGE OverloadedStrings #-}

module Utility where

import Data.Char (toLower, toUpper)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)

upper_the_first_char :: String -> String
upper_the_first_char (x:xs) = toUpper x : map toLower xs
upper_the_first_char [] = ""

-- | Escape a string for use in TypeScript double-quoted string literal.
-- Escapes double quotes (") and backslashes (\).
escapeTypeScriptString :: Text -> Text
escapeTypeScriptString s = T.concatMap escapeChar s
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

