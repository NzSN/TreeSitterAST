module Utility where

import Data.Char (toLower, toUpper)

upper_the_first_char :: String -> String
upper_the_first_char (x:xs) = toUpper x : map toLower xs
upper_the_first_char [] = ""

validate_field_ident :: String -> String
validate_field_ident s
  | s == "constructor" = "constructor_f"
  | otherwise = s
