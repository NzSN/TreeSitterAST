{-# LANGUAGE OverloadedStrings #-}
module Template.TypeScriptTemplate where

import Template.Template (Template(..), TArray(..))

import Formatting ((%), commaSep, text)
import Formatting.Formatters (build)
import Data.Text.Lazy (Text)

import_statement :: Template (TArray Text -> Text -> String)
import_statement = T $ "import { " % (commaSep build) % " } from '" % text % "';"
