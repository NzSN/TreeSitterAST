{-# LANGUAGE OverloadedStrings #-}
module Template.TypeScriptTemplate where

import Template.Template (Template(..), TArray(..))

import Formatting ((%), commaSep, text)
import Formatting.Formatters (build)
import Data.Text.Lazy (Text)

import_statement :: Template (TArray Text -> Text -> String)
import_statement = T $ "import { " % (commaSep build) % " } from '" % text % "';"

type IsExport = Bool
type IsConst  = Bool
variable_decl :: IsExport -> IsConst -> Template (Text -> Text -> String)
variable_decl True True   = T $ "export const " % text % " = \"" % text % "\";"
variable_decl False True  = T $ "const "        % text % " = \"" % text % "\";"
variable_decl True False  = T $ "export"        % text % " = \"" % text % "\";"
variable_decl False False = T $ "let "          % text % " = \"" % text % "\";"

type ParameterDeclareT = Text
parameter_declare :: Template (Text -> Text -> String)
parameter_declare = T $ text % " : " % text


type TypeIdent = Text
type FunctionIdent = Text
type FunctionDeclare = Template (FunctionIdent ->
                                 -- Parameter of functions
                                 TArray ParameterDeclareT ->
                                 -- Tail Return type of function
                                 TypeIdent ->
                                 -- Statements inside the body of Function
                                 TArray (Template a)
                                  )
function_declare :: Template (FunctionIdent ->
                               -- Parameter of functions
                               TArray ParameterDeclareT ->
                               -- Tail Return type of function
                               TypeIdent ->
                               -- Statements inside the body of Function
                               TArray Text
                               -- Output as string
                               -> String)
function_declare = T $
  "function " % text %
