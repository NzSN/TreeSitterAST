{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ECMA.ProgBuilderForECMA where

import Control.Monad.State
import Data.List as L
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Text.Lazy qualified as T
import Debug.Trace (trace)
import Fundamentals.Inference (trans)
import ProgBuilder.FieldConversion (evalFieldName, evalFieldType, extractFields, fieldsFromProperties, isStrProp, propFromTSGN, toGrammarNodeWithField)
import ProgBuilder.ProgBuilderDescription (toGrammarNodeWithProp)
import ProgBuilder.Types (AnnoatedField (..), Field (..), GrammarNodeWithField)
import Template.Template qualified as TT
import Template.TypeScriptTemplate qualified as TTS
import TreeSitterGrammarNodes (isLeaf)
import TreeSitterGrammarNodes qualified as TSGN
import TypedASTGenerator.NodeDescriptionHelper
import Utility (escapeTypeScriptString, upper_the_first_char)

-- | Transform fields within CHOICE nodes to be sum types with 'undefined'
-- This ensures that fields that may not be present in all CHOICE alternatives
-- are properly typed as optional (union with undefined)
transformChoiceFields :: GrammarNodeWithField -> GrammarNodeWithField
transformChoiceFields = transformNode False
  where
    transformNode :: Bool -> GrammarNodeWithField -> GrammarNodeWithField
    transformNode inChoice n = case n of
      TSGN.Seq members -> TSGN.Seq (map (transformNode inChoice) members)
      TSGN.Choice alternatives ->
        -- When we enter a CHOICE node, set inChoice to True for its alternatives
        TSGN.Choice (map (transformNode True) alternatives)
      TSGN.Repeat content -> TSGN.Repeat (transformNode inChoice content)
      TSGN.Repeat1 content -> TSGN.Repeat1 (transformNode inChoice content)
      TSGN.Symbol af -> TSGN.Symbol (if inChoice then transformAnnoatedField af else af)
      TSGN.StringLiteral af -> TSGN.StringLiteral (if inChoice then transformAnnoatedField af else af)
      TSGN.Pattern af -> TSGN.Pattern (if inChoice then transformAnnoatedField af else af)
      TSGN.Blank -> TSGN.Blank
      TSGN.Field fname content -> TSGN.Field fname (transformNode inChoice content)
      TSGN.Alias content named aliasVal -> TSGN.Alias (transformNode inChoice content) named aliasVal
      TSGN.Token content -> TSGN.Token (transformNode inChoice content)
      TSGN.ImmediateToken content -> TSGN.ImmediateToken (transformNode inChoice content)
      TSGN.Prec prec content -> TSGN.Prec prec (transformNode inChoice content)
      TSGN.PrecLeft prec content -> TSGN.PrecLeft prec (transformNode inChoice content)
      TSGN.PrecRight prec content -> TSGN.PrecRight prec (transformNode inChoice content)
      TSGN.PrecDynamic prec content -> TSGN.PrecDynamic prec (transformNode inChoice content)
      TSGN.Reserved content ctx -> TSGN.Reserved (transformNode inChoice content) ctx
      TSGN.Empty -> TSGN.Empty

    -- Transform an annotated field within a CHOICE node
    transformAnnoatedField :: AnnoatedField -> AnnoatedField
    transformAnnoatedField (AnnoatedField original field idx) =
      case field of
        -- For regular fields within CHOICE, convert to sum type with undefined
        Field name ftype ->
          AnnoatedField original (SumField name [ftype, "undefined"]) idx
        -- For sum fields within CHOICE, add undefined if not already present
        SumField name ftypes ->
          let ftypes' = if "undefined" `elem` ftypes then ftypes else "undefined" : ftypes
           in AnnoatedField original (SumField name ftypes') idx
        -- Empty fields stay empty
        EmptyField -> AnnoatedField original EmptyField idx

descript :: TSGN.Grammar -> String
descript grammar =
  let processed = TSGN.convert grammar
      origGrammar = TSGN.orig processed
      transformedGrammar = trans origGrammar
      rules = TSGN.grammarNodes transformedGrammar
      parentMap = computeParentMap rules
      -- Sort rules so that parent CHOICE classes come before their alternatives
      ruleNames = Map.keys rules
      sortedRuleNames = L.sortBy (compareRules parentMap) ruleNames
      builder_def = L.foldl' (\acc name -> T.concat [acc, build name (rules Map.! name) parentMap]) "" sortedRuleNames
   in T.unpack $ T.concat [prologue, builder_def]
  where
    computeParentMap :: Map.Map String TSGN.Node -> Map.Map String String
    computeParentMap rules' = Map.fromList $ catMaybes $ map (findParent rules') (Map.keys rules')
      where
        -- Find parent CHOICE class for an alternative rule.
        -- An alternative rule name is: parentName ++ "_" ++ suffix
        -- We check if any existing rule name is a prefix of this name (followed by "_")
        findParent :: Map.Map String TSGN.Node -> String -> Maybe (String, String)
        findParent rs name =
          let allRules = Map.keys rs
              -- Find rules where name starts with ruleName ++ "_"
              matchingParents = filter (\p -> name /= p && (p ++ "_") `L.isPrefixOf` name) allRules
              -- Only keep parents that are CHOICE nodes with empty members
              validParents = filter (\p -> isChoiceWithEmptyMembers (rs Map.! p)) matchingParents
           in listToMaybe $ map (\p -> (name, p)) validParents

        isChoiceWithEmptyMembers :: TSGN.Node -> Bool
        isChoiceWithEmptyMembers node = case node of
          TSGN.Choice [] -> True
          _ -> False

    -- Compare two rule names for sorting: parent CHOICE classes before alternatives
    compareRules :: Map.Map String String -> String -> String -> Ordering
    compareRules pm a b
      | Map.lookup a pm == Just b = GT -- b is parent of a, so b should come before a
      | Map.lookup b pm == Just a = LT -- a is parent of b, so a should come before b
      | otherwise = compare a b -- arbitrary but consistent alphabetical order

-- imports :: T.Text
-- imports =
--   T.concat [TT.inst
--     TTS.import_statement
--     (TT.TArray ["strict as assert"])
--     "assert", "\n"]

prologue :: T.Text
prologue =
  """
  export class SyntaticNode {
      evaluate(): string {
          throw Error("Interior or Leaf should implement evaluate().");
      }
  }

  export class SyntaticLeaf extends SyntaticNode {
      value_: string;
      constructor(value: string) {
          super();
          this.value_ = value;
      }
      evaluate(): string {
          return this.value_;
      }
  }

  export class SyntaticInterior extends SyntaticNode {
      evaluate(): string {
          throw Error("Instance of Interior should implment evaluate()");
      }
  }\n
  """

build :: String -> TSGN.Node -> Map.Map String String -> T.Text
build name rule parentMap =
  let className = T.pack $ node_type_ident name
      grammarNodeWithField = transformChoiceFields $ toGrammarNodeWithField . toGrammarNodeWithProp $ rule
      fields = extractFields grammarNodeWithField
      isAlternativeOfChoice = Map.member name parentMap
      (baseClass, constructorDef, fieldDecls) =
        if isLeaf rule
          then
            -- Leaf rules: generate constructor based on whether it's a CHOICE alternative
            if isAlternativeOfChoice
              then
                -- Leaf as CHOICE alternative: extends parent CHOICE class
                -- No-arg constructor, evaluate() returns literal
                ( "SyntaticLeaf", -- will be overridden by finalBaseClass
                  leafAlternativeConstructor,
                  Nothing
                )
              else
                -- Regular leaf (not CHOICE alternative): extends SyntaticLeaf
                -- Constructor depends on leaf type
                ( "SyntaticLeaf",
                  generateLeafConstructor rule,
                  Nothing
                )
          else
            ( "SyntaticInterior",
              interiorConstructor fields,
              Just $ TT.TArray $ collapse' fields
            )
      -- If this rule is an alternative of a top-level CHOICE node, inherit from parent CHOICE class
      finalBaseClass =
        if isAlternativeOfChoice
          then T.pack $ node_type_ident (parentMap Map.! name)
          else baseClass
      -- evaluate() method:
      -- - Leaf as CHOICE alternative: generate evaluate() that returns literal
      -- - Regular leaf: use SyntaticLeaf's evaluate()
      -- - Interior: generate custom evaluate()
      evaluateMethod =
        if isLeaf rule
          then
            if isAlternativeOfChoice
              then generateLeafAlternativeEvaluate rule
              else "" -- SyntaticLeaf has its own evaluate()
          else generateEvaluateMethod grammarNodeWithField
      allMethods =
        [constructorDef]
          ++ (if T.null evaluateMethod then [] else [evaluateMethod])
      methods = if null allMethods then Nothing else Just $ TT.TArray allMethods
   in T.concat
        [ TT.inst TTS.export_qualifier $
            TT.inst TTS.class_declare className (Just finalBaseClass) fieldDecls methods,
          "\n"
        ]
  where
    -- \| Generate constructor for regular leaf nodes (not CHOICE alternatives)
    -- StringLiteral/Blank/Token: no-arg constructor, pass literal to super()
    -- Pattern: accept value parameter, validate against pattern, pass to super()
    generateLeafConstructor :: TSGN.Node -> T.Text
    generateLeafConstructor node = case node of
      TSGN.StringLiteral value ->
        -- StringLiteral: no-arg constructor, pass literal to super()
        T.concat
          [ "constructor() { super(\"",
            escapeTypeScriptString value,
            "\"); }"
          ]
      TSGN.Pattern pattern ->
        -- Pattern: accept value, validate against pattern regex, pass to super()
        let regexPattern = patternToRegex pattern
         in T.concat
              [ "constructor(value: string) {\n",
                "  if (!",
                regexPattern,
                ".test(value)) {\n",
                "    throw new Error(`Value '${value}' does not match pattern '",
                escapeTypeScriptString pattern,
                "'`);\n",
                "  }\n",
                "  super(value);\n",
                "}"
              ]
      TSGN.Blank ->
        -- Blank: no-arg constructor, pass empty string to super()
        "constructor() { super(\"\"); }"
      -- Wrapper nodes: unwrap and generate constructor for inner content
      TSGN.Token content -> generateLeafConstructor content
      TSGN.ImmediateToken content -> generateLeafConstructor content
      TSGN.Alias content _ _ -> generateLeafConstructor content
      TSGN.Prec _ content -> generateLeafConstructor content
      TSGN.PrecLeft _ content -> generateLeafConstructor content
      TSGN.PrecRight _ content -> generateLeafConstructor content
      TSGN.PrecDynamic _ content -> generateLeafConstructor content
      TSGN.Reserved content _ -> generateLeafConstructor content
      _ ->
        -- Fallback: no-arg constructor (should not happen for valid leaf nodes)
        "constructor() { super(\"\"); }"

    -- \| Constructor for leaf alternatives of CHOICE nodes
    -- No parameters, just calls super() on parent CHOICE class
    leafAlternativeConstructor :: T.Text
    leafAlternativeConstructor =
      TT.inst
        TTS.const_declare
        (TT.TArray []) -- no parameters
        (TT.TArray [TT.inst TTS.function_call "super" Nothing])

    -- \| Convert a Tree-sitter pattern to a JavaScript RegExp
    patternToRegex :: T.Text -> T.Text
    patternToRegex pattern =
      T.concat ["/^", pattern, "$/"]

    interiorConstructor :: [Field] -> T.Text
    interiorConstructor fields =
      TT.inst
        TTS.const_declare
        (TT.TArray $ filter (not . T.null) $ map generateConstructorParam fields)
        ( TT.TArray
            (interiorPrologueStmts ++ filter (not . T.null) (map generateConstructorAssignment fields))
        )
      where
        generateConstructorParam :: Field -> T.Text
        generateConstructorParam field =
          case field of
            Field name _ ->
              TT.inst TTS.parameter_declare name (evalFieldType field)
            SumField name _ ->
              TT.inst TTS.parameter_declare name (evalFieldType field)
            EmptyField -> "" -- Skip empty fields
        generateConstructorAssignment :: Field -> T.Text
        generateConstructorAssignment field =
          case field of
            Field name _ ->
              T.concat ["this.", name, "_i = ", name, ";"]
            SumField name _ ->
              T.concat ["this.", name, "_i = ", name, ";"]
            EmptyField -> "" -- Skip empty fields
    interiorPrologueStmts :: [T.Text]
    interiorPrologueStmts =
      [TT.inst TTS.function_call "super" Nothing]

eval :: Field -> T.Text
eval f
  | field@(Field _ _) <- f = evaluate field
  | field@(SumField _ _) <- f = evaluate field
  | EmptyField <- f = ""
  where
    evaluate field = T.concat [evalFieldName field, " : ", evalFieldType field]

collapse' :: [Field] -> [T.Text]
collapse' = map (\f -> T.concat [eval f, ";\n"])

-- | Generate evaluate() method for a grammar node
generateEvaluateMethod :: GrammarNodeWithField -> T.Text
generateEvaluateMethod node =
  case node of
    TSGN.Seq members -> generateSeqEvaluate members
    TSGN.Choice alternatives -> generateChoiceEvaluate alternatives
    TSGN.Repeat content -> generateRepeatEvaluate content
    TSGN.Repeat1 content -> generateRepeat1Evaluate content
    TSGN.Symbol annofield -> generateSymbolEvaluate annofield
    TSGN.StringLiteral annofield -> generateLiteralEvaluate annofield
    TSGN.Pattern annofield -> generateLiteralEvaluate annofield
    TSGN.Blank -> generateBlankEvaluate
    TSGN.Field _ content -> generateEvaluateMethod content
    TSGN.Alias content _ _ -> generateEvaluateMethod content
    TSGN.Token content -> generateEvaluateMethod content
    TSGN.ImmediateToken content -> generateEvaluateMethod content
    TSGN.Prec _ content -> generateEvaluateMethod content
    TSGN.PrecLeft _ content -> generateEvaluateMethod content
    TSGN.PrecRight _ content -> generateEvaluateMethod content
    TSGN.PrecDynamic _ content -> generateEvaluateMethod content
    TSGN.Reserved content _ -> generateEvaluateMethod content
    TSGN.Empty -> generateEmptyEvaluate
  where
    -- Helper to get field instance name from AnnoatedField
    annoFieldInstanceName :: AnnoatedField -> T.Text
    annoFieldInstanceName (AnnoatedField _ field idx) =
      case (field, idx) of
        (EmptyField, _) -> ""
        (_, Nothing) -> ""
        (Field name _, Just i) -> T.concat [name, T.pack $ "_" ++ show i, "_i"]
        (SumField name _, Just i) -> T.concat [name, T.pack $ "_" ++ show i, "_i"]

    -- Generate expression to evaluate an annotated field
    evalAnnoatedFieldExpr :: AnnoatedField -> T.Text
    evalAnnoatedFieldExpr annofield@(AnnoatedField original field _) =
      case field of
        EmptyField ->
          case original of
            Just s -> T.concat ["\"", escapeTypeScriptString s, "\""]
            Nothing -> T.concat ["throw new Error(\"Cannot evaluate empty field\");"]
        Field _ ftype ->
          let fieldName = annoFieldInstanceName annofield
           in if isPrimitiveType ftype
                then
                  if ftype == "string"
                    then T.concat ["this.", fieldName]
                    else T.concat ["String(this.", fieldName, ")"]
                else T.concat ["this.", fieldName, ".evaluate()"]
        SumField _ ftypes ->
          let fieldName = annoFieldInstanceName annofield
              -- Check if any non-undefined type is primitive
              nonUndefinedTypes = filter (/= "undefined") ftypes
              hasPrimitive = any isPrimitiveType nonUndefinedTypes
           in if hasPrimitive
                then -- For sum types with primitives, we need to handle string vs number/boolean
                  if "string" `elem` nonUndefinedTypes
                    then T.concat ["this.", fieldName]
                    else T.concat ["String(this.", fieldName, ")"]
                else T.concat ["this.", fieldName, ".evaluate()"]
      where
        isPrimitiveType :: T.Text -> Bool
        isPrimitiveType t = t `elem` ["string", "number", "boolean"]

    -- Helper to generate a throw statement for evaluation errors
    throwEvaluationError :: T.Text -> T.Text
    throwEvaluationError msg = T.concat ["throw new Error(\"", msg, "\")"]

    -- Check if a string is a throw statement (starts with "throw ")
    isThrowStatement :: T.Text -> Bool
    isThrowStatement = T.isPrefixOf "throw "

    -- Helper to find SYMBOL field in a node (unwrapping wrappers)
    -- Shared by both generateChoiceEvaluate and evalNodeExpr for CHOICE
    -- Returns field name for checking, and full expression for evaluation
    findSymbolFieldForChoice :: GrammarNodeWithField -> Maybe (T.Text, T.Text)
    findSymbolFieldForChoice node = case node of
      TSGN.Symbol annofield ->
        let fieldName = annoFieldInstanceName annofield
            expr = evalNodeExpr node
         in Just (fieldName, expr)
      TSGN.Field _ content -> findSymbolFieldForChoice content
      TSGN.Alias content _ _ -> findSymbolFieldForChoice content
      TSGN.Token content -> findSymbolFieldForChoice content
      TSGN.ImmediateToken content -> findSymbolFieldForChoice content
      TSGN.Prec _ content -> findSymbolFieldForChoice content
      TSGN.PrecLeft _ content -> findSymbolFieldForChoice content
      TSGN.PrecRight _ content -> findSymbolFieldForChoice content
      TSGN.PrecDynamic _ content -> findSymbolFieldForChoice content
      TSGN.Reserved content _ -> findSymbolFieldForChoice content
      TSGN.Seq members -> findSymbolFieldForChoiceInSeq members
      TSGN.Choice alts -> L.find isJust (map findSymbolFieldForChoice alts) >>= id
      _ -> Nothing

    -- Find a Symbol field in a SEQ
    findSymbolFieldForChoiceInSeq :: [GrammarNodeWithField] -> Maybe (T.Text, T.Text)
    findSymbolFieldForChoiceInSeq members = do
      (fieldName, _) <- L.find isJust (map findSymbolFieldForChoice members) >>= id
      return (fieldName, evalNodeExpr (TSGN.Seq members))

    -- Build if-else statements for CHOICE alternatives
    -- Used by both generateChoiceEvaluate (wraps in method) and evalNodeExpr (wraps in IIFE)
    buildChoiceIfElseStatements :: [GrammarNodeWithField] -> [T.Text]
    buildChoiceIfElseStatements [] = [T.concat ["throw new Error(\"No alternative matched in CHOICE node\");"]]
    buildChoiceIfElseStatements [alt] =
      -- Last alternative
      case findSymbolFieldForChoice alt of
        Just (fieldName, expr) ->
          [ T.concat ["if (this.", fieldName, " !== undefined) { return ", expr, "; } "],
            T.concat ["throw new Error(\"No alternative matched in CHOICE node\");"]
          ]
        Nothing ->
          [T.concat ["return ", evalNodeExpr alt, ";"]]
    buildChoiceIfElseStatements (alt : rest) =
      case findSymbolFieldForChoice alt of
        Just (fieldName, expr) ->
          let ifStmt = T.concat ["if (this.", fieldName, " !== undefined) { return ", expr, "; } "]
              restStmts = buildChoiceIfElseStatements rest
           in ifStmt : restStmts
        Nothing ->
          [T.concat ["return ", evalNodeExpr alt, ";"]]

    -- Check if an expression is a string literal (e.g., "&&" or 'hello')
    isStringLiteral :: T.Text -> Maybe T.Text
    isStringLiteral expr
      | T.length expr >= 2 && T.head expr == '"' && T.last expr == '"' =
          Just $ T.drop 1 $ T.dropEnd 1 expr -- Extract content between quotes
      | T.length expr >= 2 && T.head expr == '\'' && T.last expr == '\'' =
          Just $ T.drop 1 $ T.dropEnd 1 expr
      | otherwise = Nothing

    -- Build a template literal from a list of expressions
    -- String literals are embedded directly, expressions are wrapped in ${}
    -- Falls back to string concatenation if any content contains backticks or ${
    buildTemplateLiteral :: [T.Text] -> T.Text
    buildTemplateLiteral [] = "\"\"" -- Empty string
    buildTemplateLiteral [expr] =
      case isStringLiteral expr of
        Just content -> T.concat ["\"", content, "\""] -- Single literal stays as string
        Nothing -> T.concat ["`", "${", expr, "}", "`"] -- Single expression
    buildTemplateLiteral exprs =
      -- Check if any string literal contains backticks or ${ (can't use template literals)
      if any hasTemplateLiteralConflict exprs
        then T.intercalate " + " (map formatForConcat exprs)
        else T.concat ["`", T.concat (map formatPart exprs), "`"]
      where
        hasTemplateLiteralConflict :: T.Text -> Bool
        hasTemplateLiteralConflict e =
          case isStringLiteral e of
            Just content -> '`' `elem` T.unpack content || T.isInfixOf "${" content
            Nothing -> False

        formatPart :: T.Text -> T.Text
        formatPart expr =
          case isStringLiteral expr of
            Just content -> content -- Embed string literal content directly
            Nothing -> T.concat ["${", expr, "}"] -- Wrap expression in ${}
        formatForConcat :: T.Text -> T.Text
        formatForConcat expr =
          case isStringLiteral expr of
            Just _ -> expr -- Keep string literal as-is
            Nothing -> expr -- Keep expression as-is

    -- Evaluate a node to an expression (or throw statement)
    evalNodeExpr :: GrammarNodeWithField -> T.Text
    evalNodeExpr n =
      case n of
        TSGN.Seq members ->
          let childCalls = map evalNodeExpr members
              containsThrow call = isThrowStatement call
           in case filter containsThrow childCalls of
                [] -> buildTemplateLiteral childCalls
                (c : _) -> c
        TSGN.Choice alternatives ->
          if null alternatives
            then throwEvaluationError "Cannot evaluate CHOICE node: missing alternative"
            else T.concat ["(() => { ", T.concat (buildChoiceIfElseStatements alternatives), " })()"]
        TSGN.Repeat content -> evalNodeExpr content
        TSGN.Repeat1 content -> evalNodeExpr content
        TSGN.Symbol annofield -> evalAnnoatedFieldExpr annofield
        TSGN.StringLiteral annofield -> evalAnnoatedFieldExpr annofield
        TSGN.Pattern annofield -> evalAnnoatedFieldExpr annofield
        TSGN.Blank -> "\"\""
        TSGN.Field _ content -> evalNodeExpr content
        TSGN.Alias content _ _ -> evalNodeExpr content
        TSGN.Token content -> evalNodeExpr content
        TSGN.ImmediateToken content -> evalNodeExpr content
        TSGN.Prec _ content -> evalNodeExpr content
        TSGN.PrecLeft _ content -> evalNodeExpr content
        TSGN.PrecRight _ content -> evalNodeExpr content
        TSGN.PrecDynamic _ content -> evalNodeExpr content
        TSGN.Reserved content _ -> evalNodeExpr content
        TSGN.Empty -> "\"\""

    -- Generate evaluate() for SEQ nodes: concatenate children results
    generateSeqEvaluate :: [GrammarNodeWithField] -> T.Text
    generateSeqEvaluate members =
      let expr = evalNodeExpr (TSGN.Seq members)
       in if isThrowStatement expr
            then TT.inst TTS.evaluate_method (TT.TArray [expr])
            else TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", expr, ";"]])

    -- Generate evaluate() for CHOICE nodes: handle choice resolution
    -- Uses flat if-else statements for better V8 optimization
    generateChoiceEvaluate :: [GrammarNodeWithField] -> T.Text
    generateChoiceEvaluate alternatives =
      let statements = buildChoiceIfElseStatements alternatives
       in TT.inst TTS.evaluate_method (TT.TArray statements)

    -- Generate evaluate() for REPEAT nodes: generate 0-n repetitions
    generateRepeatEvaluate :: GrammarNodeWithField -> T.Text
    generateRepeatEvaluate content =
      let expr = evalNodeExpr (TSGN.Repeat content)
       in if isThrowStatement expr
            then TT.inst TTS.evaluate_method (TT.TArray [expr])
            else TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", expr, ";"]])

    -- Generate evaluate() for REPEAT1 nodes: generate 1-n repetitions
    generateRepeat1Evaluate :: GrammarNodeWithField -> T.Text
    generateRepeat1Evaluate content =
      let expr = evalNodeExpr (TSGN.Repeat1 content)
       in if isThrowStatement expr
            then TT.inst TTS.evaluate_method (TT.TArray [expr])
            else TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", expr, ";"]])

    -- Generate evaluate() for SYMBOL nodes: delegate to referenced class
    generateSymbolEvaluate :: AnnoatedField -> T.Text
    generateSymbolEvaluate annofield =
      let expr = evalAnnoatedFieldExpr annofield
       in if isThrowStatement expr
            then TT.inst TTS.evaluate_method (TT.TArray [expr])
            else TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", expr, ";"]])

    -- Generate evaluate() for literal nodes: return value
    generateLiteralEvaluate :: AnnoatedField -> T.Text
    generateLiteralEvaluate annofield =
      let expr = evalAnnoatedFieldExpr annofield
       in if isThrowStatement expr
            then TT.inst TTS.evaluate_method (TT.TArray [expr])
            else TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", "\"", expr, "\"", ";"]])

    -- Generate evaluate() for BLANK nodes: return empty string
    generateBlankEvaluate :: T.Text
    generateBlankEvaluate =
      TT.inst TTS.evaluate_method (TT.TArray ["return \"\";"])

    -- Generate evaluate() for EMPTY nodes: return empty string
    generateEmptyEvaluate :: T.Text
    generateEmptyEvaluate =
      TT.inst TTS.evaluate_method (TT.TArray ["return \"\";"])

-- | Generate evaluate() method for leaf alternatives (STRING, PATTERN, BLANK)
-- that extend a parent CHOICE class. These have no fields, so evaluate returns
-- the literal value directly.
generateLeafAlternativeEvaluate :: TSGN.Node -> T.Text
generateLeafAlternativeEvaluate node = case node of
  TSGN.StringLiteral value ->
    TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return \"", escapeTypeScriptString value, "\";"]])
  TSGN.Pattern value ->
    TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return \"", escapeTypeScriptString value, "\";"]])
  TSGN.Blank ->
    TT.inst TTS.evaluate_method (TT.TArray ["return \"\";"])
  -- For wrapped nodes, unwrap and recurse
  TSGN.Token content -> generateLeafAlternativeEvaluate content
  TSGN.ImmediateToken content -> generateLeafAlternativeEvaluate content
  TSGN.Alias content _ _ -> generateLeafAlternativeEvaluate content
  TSGN.Prec _ content -> generateLeafAlternativeEvaluate content
  TSGN.PrecLeft _ content -> generateLeafAlternativeEvaluate content
  TSGN.PrecRight _ content -> generateLeafAlternativeEvaluate content
  TSGN.PrecDynamic _ content -> generateLeafAlternativeEvaluate content
  TSGN.Reserved content _ -> generateLeafAlternativeEvaluate content
  _ -> TT.inst TTS.evaluate_method (TT.TArray ["return \"\";"]) -- fallback
