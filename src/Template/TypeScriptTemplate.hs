{-# LANGUAGE OverloadedStrings, TemplateHaskell, TemplateHaskellQuotes #-}
module Template.TypeScriptTemplate where

import Template.Template (Template(..), TArray(..))

import Formatting ((%), commaSep, text, optioned, spaced)
import Formatting.Formatters (build)
import Data.Text.Lazy (Text)

import_statement :: Template (TArray Text -> Text -> String)
import_statement = T $ "import { " % (commaSep build) % " } from '" % text % "';"

export_qualifier :: Template (Text -> String)
export_qualifier = T $ "export " % text

const_qualifier :: Template (Text -> String)
const_qualifier = T $ "const " % text

static_qualifier :: Template (Text -> String)
static_qualifier = T $ "static " % text

variable_decl :: Template (Text -> Text -> String)
variable_decl = T $ "let " % text % " = \"" % text % "\";"

parameter_declare :: Template (Text -> Text -> String)
parameter_declare = T $ text % " : " % text

function_declare :: Template (-- Function Ident
                               Text ->
                               -- Parameters
                               TArray Text ->
                               -- Tail Return Type
                               Maybe Text ->
                               -- Statements
                               TArray Text ->
                               -- Output as string
                               String)
function_declare = T $
  "function " % text % "(" % build % ")" % (optioned (": " % text)) % " { " % build % " }"

private_qualifier :: Template (Text -> String)
private_qualifier = T $ "private " % text

public_qualifier :: Template (Text -> String)
public_qualifier = T $ "public " % text

method_declare :: Template (-- Method Ident
                            Text ->
                            -- Parameters
                            TArray Text ->
                            -- Tail Return Type
                            Maybe Text ->
                            -- Statements
                            TArray Text ->
                            String)
method_declare = T $
  text % "(" % build % ")" % (optioned (": " % text)) % " { " % build % " }"

prop_declare :: Template (Text -> Text -> String)
prop_declare = T $ text % " : " % text

class_declare :: Template (-- Class Ident
                           Text ->
                           -- Base ident
                           Maybe Text ->
                           -- Properties
                           Maybe (TArray Text) ->
                           -- Methods
                           Maybe (TArray Text) ->
                           String)
class_declare = T $
  "class " % text % (optioned (" implements " % text)) % " { " % (optioned (spaced build)) % " " % (optioned (spaced build)) % " }"
