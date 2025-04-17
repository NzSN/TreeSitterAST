{-# LANGUAGE OverloadedStrings, TemplateHaskell, TemplateHaskellQuotes #-}
module Template.TypeScriptTemplate
  (import_statement,
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
   function_call,
   var_ref,
   node_type_assertion,
   prop_initialize,
   prop_initialize_array,
   field_initialize,
   array_field_initialize
   ) where

import Template.Template (Template(..), TArray(..), inst)

import Formatting ((%+),(%), commaSep, text, optioned, spaced, Format, formatToString)
import Formatting.Formatters (build)
import Data.Text.Lazy (Text, unpack, pack)
import Utility (upper_the_first_char, validate_field_ident)

import_statement :: Template (TArray Text -> Text -> Text)
import_statement = T $ "import { " % (commaSep build) % " } from '" % text % "';"

export_qualifier :: Template (Text -> Text)
export_qualifier = T $ "export " % text

const_qualifier :: Template (Text -> Text)
const_qualifier = T $ "const " % text

static_qualifier :: Template (Text -> Text)
static_qualifier = T $ "static " % text

variable_decl :: Template (Text -> Text -> Text)
variable_decl = T $ "let " % text % " = \"" % text % "\";"

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
  "function " % text % "(" % build % ")" % (optioned (": " % text)) % " { " % build % " }"

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
prop_declare = T $ text % " : " % text

const_declare :: Template (TArray Text -> -- Parameters
                           TArray Text -> -- Statements
                           Text)
const_declare = T $ "constructor(" % build % ") { " % build % " }"

class_declare :: Template (-- Class Ident
                           Text ->
                           -- Base ident
                           Maybe Text ->
                           -- Properties
                           Maybe (TArray Text) ->
                           -- Methods
                           Maybe (TArray Text) ->
                           Text)
class_declare = T $
  "class " % text % (optioned (" extends " % text)) % " { " % (optioned (spaced build)) % " " % (optioned (spaced build)) % " }"

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

prop_initialize :: Text -> Text
prop_initialize prop_name = inst
  (T $
   "{\n\
    \ let r = (new Searcher(node, \"" % text % "\")).searching_next(node.walk());\n\
    \ if (r != null) { this." % text % " = new " % text % "(r); }\n\
    \}")
  prop_name prop_name (pack $ upper_the_first_char $ unpack prop_name)

prop_initialize_array :: Text -> Text
prop_initialize_array prop_name = inst
  (T $
   "this." % text % " = (new Searcher(node, \"" % text % "\")).searching_all(node.walk()).map(((n:Node) => new " % text % "(n)));")
  prop_name prop_name (pack $ upper_the_first_char $ unpack prop_name)

field_initialize :: Text -> [Text] -> Text
field_initialize field_name types =
  let s = inst (T $
                "{\n\
                \ let n: Node | null = node.childForFieldName(\"" % text % "\"); \n\
                \ assert(n != null);\n")
                field_name
  in pack $ (unpack s) ++ (unpack $ field_type_check field_name types) ++ "\n}\n"
   where
     field_type_check :: Text -> [Text] -> Text
     field_type_check _ [] = pack ""
     field_type_check prop_name (x:xs) =
       pack $
        (formatToString (
            "if (node.type == \"" % text % "\") { \n\
            \  this." % text % " = new " % text % "(node); \n\
            \}\n")
          x (pack $ validate_field_ident $ unpack prop_name) (pack $ upper_the_first_char $ unpack x))
        ++ (unpack $ field_type_check prop_name xs)

array_field_initialize :: Text -> [Text] -> Text
array_field_initialize field_name types =
  let s = inst (T $
                "{\n\
                \ let ns: (Node | null)[] = node.childrenForFieldName(\"" % text % "\");\n\
                \ ns.forEach((n: Node | null) => {")
                field_name
  in pack $ (unpack s) ++ (unpack $ field_type_check field_name types) ++ "}) \n}\n"
   where
     field_type_check :: Text -> [Text] -> Text
     field_type_check _ [] = pack ""
     field_type_check prop_name (x:xs) =
       pack $
        (formatToString (
            "if (node.type == \"" % text % "\") { \n\
            \  this." % text % ".push(new " % text % "(node)); \n\
            \}\n")
          x prop_name (pack $ upper_the_first_char $ unpack x))
        ++ (unpack $ field_type_check prop_name xs)
