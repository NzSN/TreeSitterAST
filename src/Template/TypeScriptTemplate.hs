{-# LANGUAGE OverloadedStrings, TemplateHaskell, TemplateHaskellQuotes #-}
module Template.TypeScriptTemplate
  (import_statement,
   import_all_statement,
   export_qualifier,
   const_qualifier,
   static_qualifier,
   variable_decl,
   parameter_declare,
   function_declare,
   private_qualifier,
   public_qualifier,
   method_declare,
   prop_declare,
   const_declare,
   class_declare,
   interface_declare,
   function_call,
   var_ref,
   node_type_assertion,
   subtype_prop_initialize,
   prop_initialize,
   prop_initialize_array,
   field_initialize,
   array_field_initialize,
   case_expression,
   switch_statements,
   node_processor_proc_template,
   node_build_template
   ) where

import Data.Maybe
import Data.List

import Template.Template (Template(..), TArray(..), inst)
import Formatting ((%), commaSep, text, optioned, spaced, formatToString)
import Formatting.Formatters (build)
import Data.Text.Lazy (Text, unpack, pack)
import BackendDescription.NodeDescriptionHelper

import_statement :: Template (TArray Text -> Text -> Text)
import_statement = T $ "import { " % (commaSep build) % " } from '" % text % "';"

import_all_statement :: Template (Text -> Text -> Text)
import_all_statement = T $ "import * as " % text % " from '" % text % "';"

export_qualifier :: Template (Text -> Text)
export_qualifier = T $ "export " % text

const_qualifier :: Template (Text -> Text)
const_qualifier = T $ "const " % text

static_qualifier :: Template (Text -> Text)
static_qualifier = T $ "static " % text

variable_decl :: Template (Text -> Text -> Text)
variable_decl = T $ "let " % text % " = " % text % ";"

parameter_declare :: Template (Text -> Text -> Text)
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
                               Text)
function_declare = T $
  "function " % text % "(" % commaSep build % ")" % (optioned (": " % text)) % " { " % build % " }"

private_qualifier :: Template (Text -> Text)
private_qualifier = T $ "private " % text

public_qualifier :: Template (Text -> Text)
public_qualifier = T $ "public " % text

method_declare :: Template (-- Method Ident
                            Text ->
                            -- Parameters
                            TArray Text ->
                            -- Tail Return Type
                            Maybe Text ->
                            -- Statements
                            TArray Text ->
                            Text)
method_declare = T $
  text % "(" % build % ")" % (optioned (": " % text)) % " { " % build % " }"

prop_declare :: Template (Text -> Text -> Text)
prop_declare = T $ text % " : " % text % ";"

const_declare :: Template (TArray Text -> -- Parameters
                           TArray Text -> -- Statements
                           Text)
const_declare = T $ "constructor(" % build % ") { " % build % " }"

composed_type_declare :: Bool -> -- Is Interface ?
                         Template (
                           -- Class Ident
                           Text ->
                           -- Base ident
                           Maybe Text ->
                           -- Properties
                           Maybe (TArray Text) ->
                           -- Methods
                           Maybe (TArray Text) ->
                           Text)
composed_type_declare True = T $
  "interface " % text % (optioned (" extends " % text)) % " { " % (optioned (spaced build)) % " " % (optioned (spaced build)) % " }"
composed_type_declare False = T $
  "class " % text % (optioned (" extends " % text)) % " { " % (optioned (spaced build)) % " " % (optioned (spaced build)) % " }"

class_declare :: Template (-- Class Ident
                           Text ->
                           -- Base ident
                           Maybe Text ->
                           -- Properties
                           Maybe (TArray Text) ->
                           -- Methods
                           Maybe (TArray Text) ->
                           Text)
class_declare = composed_type_declare False

interface_declare :: Template (-- Class Ident
                           Text ->
                           -- Base ident
                           Maybe Text ->
                           -- Properties
                           Maybe (TArray Text) ->
                           -- Methods
                           Maybe (TArray Text) ->
                           Text)
interface_declare = composed_type_declare True

function_call :: Template (Text -> -- Function Identifier
                           Maybe (TArray Text) -> -- Parameters
                           Text)
function_call = T $
  text % "(" % optioned build % ");"

var_ref :: Template (Text -> Text)
var_ref = T $ text

-- Special Templates
node_type_assertion :: Template (Text -> Text)
node_type_assertion = T $ "assert(node.type == \'" % text % "\');"

subtype_prop_initialize :: Text -> Text
subtype_prop_initialize n_type = inst
  (T $ "if (node.type == \"" % text % "\") { this." % text % " = new " % text % "(node); }")
  n_type (pack $ node_name_ident $ unpack n_type) (pack $ node_type_ident $ unpack n_type)

prop_initialize :: Text -> Text -> Text -> Text
prop_initialize = inst
  (T $
   "{\n\
    \ let r = (new Searcher(node, \"" % text % "\")).searching_next(node.walk());\n\
    \ if (r != null) { this." % text % " = new " % text % "(r); }\n\
    \}")

prop_initialize_array :: Text -> Text -> Text -> Text
prop_initialize_array = inst
  (T $
   "this." % text % " = (new Searcher(node, \"" % text % "\")).searching_all(node.walk()).map(((n:Node) => new " % text % "(n)));")

field_initialize :: [Text] -> Text -> [Text] -> Text
field_initialize supertypes field_name types =
  let s = inst (T $
                "{\n\
                \ let n: Node | null = node.childForFieldName(\"" % text % "\");\n")
                field_name
  in pack $ (unpack s) ++ (unpack $ field_type_check field_name types) ++ "\n}\n"
   where
     field_type_check :: Text -> [Text] -> Text
     field_type_check _ [] = pack ""
     field_type_check prop_name (x:xs) =
       if isNothing $ flip find supertypes $ \y -> x == y
        -- Not a Supertype
        then pack $
             (formatToString (
                 "if (n != null && n.type == \"" % text % "\") { \n\
                 \  this." % text % " = new " % text % "(n); \n\
                 \}\n")
              x
              (pack $ node_name_ident $ unpack prop_name)
              (pack $ node_type_ident $ unpack x))
             ++ (unpack $ field_type_check prop_name xs)
        -- A Supertype
        else pack $
             (formatToString (
                 "if (n != null) { this." % text % " = new " % text % "(n);}")
                 (pack $ node_name_ident $ unpack prop_name)
                 (pack $ node_type_ident $ unpack x ))
             ++ (unpack $ field_type_check prop_name xs)

array_field_initialize :: [Text] -> Text -> [Text] -> Text
array_field_initialize supertypes field_name types =
  let s = inst (T $
                "{\n\
                \ let ns: (Node | null)[] = node.childrenForFieldName(\"" % text % "\");\n\
                \ ns.forEach((n: Node | null) => {\n\
                \ if (n == null) return;\n")
                field_name
  in pack $ (unpack s) ++ (unpack $ field_type_check field_name types) ++ "}) \n}\n"
   where
     field_type_check :: Text -> [Text] -> Text
     field_type_check _ [] = pack ""
     field_type_check prop_name (x:xs) =
       if isNothing $ flip find supertypes $ \y -> x == y
        -- Not a Supertype
        then pack $
             (formatToString (
                 "if (n.type == \"" % text % "\") { \n\
                 \  this." % text % ".push(new " % text % "(n)); \n\
                 \}\n")
              x (pack $ node_name_ident $ unpack prop_name) (pack $ node_type_ident $ unpack x))
             ++ (unpack $ field_type_check prop_name xs)
        -- A Supertype
        else pack $
             (formatToString (
                "this." % text % ".push(new " % text % "(n));")
                (pack $ node_name_ident $ unpack prop_name) (pack $ node_type_ident $ unpack x))
             ++ (unpack $ field_type_check prop_name xs)

case_expression :: Text -> Text -> Text
case_expression = inst $ T $ "case \"" % text % "\": " % text

switch_statements :: Text -> TArray Text -> Text
switch_statements = inst $ T $ "switch (" % text % ") { " % build % " }"

node_processor_proc_template :: Text -> Text -> Text -> Text
node_processor_proc_template =
  inst (T $ "if (processor." % text % " != undefined) { results = processor." % text % "(node as " % text % "); } break;")

node_build_template :: Text -> Text
node_build_template n_type =
  inst (T $ "return new " % text % "(node);")
        (pack $ node_type_ident $ unpack n_type)
