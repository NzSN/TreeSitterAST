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
            -- Leaf rule: normally extends SyntaticLeaf with value param
            -- BUT if it's an alternative of a CHOICE, use no-arg constructor
            if isAlternativeOfChoice
              then
                ( "SyntaticLeaf", -- baseClass (will be overridden by finalBaseClass)
                  leafAlternativeConstructor, -- no-arg constructor
                  Nothing
                )
              else
                ( "SyntaticLeaf",
                  leafConstructor,
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
      -- For leaf alternatives of CHOICE, generate evaluate() that returns literal value
      evaluateMethod =
        if isLeaf rule
          then
            if isAlternativeOfChoice
              then generateLeafAlternativeEvaluate rule
              else "" -- SyntaticLeaf has its own evaluate()
          else generateEvaluateMethod grammarNodeWithField
      factoryMethod = generateFactoryMethods (isLeaf rule && not isAlternativeOfChoice) (T.unpack className) fields
      allMethods =
        [constructorDef]
          ++ (if T.null evaluateMethod then [] else [evaluateMethod])
          ++ (if T.null factoryMethod then [] else [factoryMethod])
      methods = if null allMethods then Nothing else Just $ TT.TArray allMethods
   in T.concat
        [ TT.inst TTS.export_qualifier $
            TT.inst TTS.class_declare className (Just finalBaseClass) fieldDecls methods,
          "\n"
        ]
  where
    leafConstructor :: T.Text
    leafConstructor =
      TT.inst
        TTS.const_declare
        (TT.TArray [TT.inst TTS.parameter_declare "value" "string"])
        (TT.TArray [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "value"])])

    -- Constructor for leaf alternatives (STRING, PATTERN, BLANK) that extend a parent CHOICE class
    -- No parameters, just calls super()
    leafAlternativeConstructor :: T.Text
    leafAlternativeConstructor =
      TT.inst
        TTS.const_declare
        (TT.TArray []) -- no parameters
        (TT.TArray [TT.inst TTS.function_call "super" Nothing])

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

-- | Generate static factory methods for a class
generateFactoryMethods :: Bool -> String -> [Field] -> T.Text
generateFactoryMethods isLeafNode className fields =
  if null fields && not isLeafNode
    then ""
    else
      let methodName = T.pack $ "create" ++ upper_the_first_char className
          params = generateFactoryParams isLeafNode fields
          body = generateFactoryBody isLeafNode className fields
       in TT.inst TTS.static_factory_method methodName params (T.pack className) body
  where
    generateFactoryParams :: Bool -> [Field] -> TT.TArray T.Text
    generateFactoryParams isLeaf fields'
      | isLeaf = TT.TArray [TT.inst TTS.parameter_declare "value" "string"]
      | otherwise = TT.TArray $ filter (not . T.null) $ map generateParam fields'
      where
        generateParam :: Field -> T.Text
        generateParam field =
          case field of
            Field {} -> TT.inst TTS.parameter_declare (field_name field) (evalFieldType field)
            SumField {} -> TT.inst TTS.parameter_declare (field_name field) (evalFieldType field)
            EmptyField -> ""

    generateFactoryBody :: Bool -> String -> [Field] -> T.Text
    generateFactoryBody isLeaf clsName fields'
      | isLeaf =
          let instanceVar = T.pack $ "return new " ++ clsName ++ "(value);"
           in T.concat [instanceVar, "\n"]
      | otherwise =
          let paramNames = filter (not . T.null) $ map fieldName fields'
              paramList = T.intercalate ", " paramNames
              instanceVar = T.pack $ "const instance = new " ++ clsName ++ "(" ++ T.unpack paramList ++ ");"
              returnStmt = T.pack $ "return instance;"
              stmts = [instanceVar, returnStmt]
           in T.concat $ map (\s -> T.concat [s, "\n"]) stmts
      where
        fieldName :: Field -> T.Text
        fieldName (Field name _) = name
        fieldName (SumField name _) = name
        fieldName EmptyField = ""

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

    -- Helper to generate a throw expression (IIFE) for use in ternary expressions
    throwExpression :: T.Text -> T.Text
    throwExpression msg = T.concat ["(() => { throw new Error(\"", msg, "\"); })()"]

    -- Generate expression for CHOICE node evaluation
    generateChoiceExpression :: [GrammarNodeWithField] -> T.Text
    generateChoiceExpression alternatives =
      let -- Generate conditional chain for all alternatives
          conditionalChain = buildConditionalChain alternatives
       in if T.null conditionalChain
            then throwExpression "Cannot evaluate CHOICE node: no valid alternatives"
            else T.concat ["(", conditionalChain, ")"]
      where
        -- Helper to find SYMBOL field in a node (unwrapping wrappers)
        -- Returns field name for checking, and full expression for evaluation
        findSymbolField :: GrammarNodeWithField -> Maybe (T.Text, T.Text)
        findSymbolField node = case node of
          TSGN.Symbol annofield ->
            let fieldName = annoFieldInstanceName annofield
                expr = evalNodeExpr node -- Use full node evaluation, not just field
             in Just (fieldName, expr)
          TSGN.Field _ content -> findSymbolField content -- Unwrap FIELD
          TSGN.Alias content _ _ -> findSymbolField content -- Unwrap ALIAS
          TSGN.Token content -> findSymbolField content -- Unwrap TOKEN
          TSGN.ImmediateToken content -> findSymbolField content -- Unwrap IMMEDIATE_TOKEN
          TSGN.Prec _ content -> findSymbolField content -- Unwrap PREC
          TSGN.PrecLeft _ content -> findSymbolField content -- Unwrap PREC_LEFT
          TSGN.PrecRight _ content -> findSymbolField content -- Unwrap PREC_RIGHT
          TSGN.PrecDynamic _ content -> findSymbolField content -- Unwrap PREC_DYNAMIC
          TSGN.Reserved content _ -> findSymbolField content -- Unwrap RESERVED
          TSGN.Seq members -> findSymbolFieldInSeq members
          TSGN.Choice alternatives -> L.find isJust (map findSymbolField alternatives) >>= id
          _ -> Nothing -- BLANK, REPEAT, etc.
          -- Helper to find a Symbol field in a SEQ, returning the field name but expression from the full SEQ
        findSymbolFieldInSeq :: [GrammarNodeWithField] -> Maybe (T.Text, T.Text)
        findSymbolFieldInSeq members = do
          -- Find first Symbol in members
          (fieldName, _) <- L.find isJust (map findSymbolField members) >>= id
          -- Return field name with full SEQ evaluation
          return (fieldName, evalNodeExpr (TSGN.Seq members))

        -- Build conditional chain for alternatives
        buildConditionalChain :: [GrammarNodeWithField] -> T.Text
        buildConditionalChain [] = throwExpression "No alternative matched in CHOICE node"
        buildConditionalChain [alt] =
          -- Last alternative: handle based on type
          case findSymbolField alt of
            Just (fieldName, expr) ->
              -- SYMBOL alternative: check if defined
              let check = T.concat ["this.", fieldName, " !== undefined"]
               in T.concat [check, " ? ", expr, " : ", throwExpression "No alternative matched in CHOICE node"]
            Nothing ->
              -- Non-SYMBOL alternative (BLANK, etc.): evaluate directly
              evalNodeExpr alt
        buildConditionalChain (alt : rest) =
          case findSymbolField alt of
            Just (fieldName, expr) ->
              -- Alternative with checkable field: generate conditional
              let check = T.concat ["this.", fieldName, " !== undefined"]
                  restChain = buildConditionalChain rest
               in T.concat [check, " ? ", expr, " : ", restChain]
            Nothing ->
              -- Alternative without checkable field: evaluate directly as fallback
              evalNodeExpr alt

    -- Check if a string is a throw statement (starts with "throw ")
    isThrowStatement :: T.Text -> Bool
    isThrowStatement = T.isPrefixOf "throw "

    -- Evaluate a node to an expression (or throw statement)
    evalNodeExpr :: GrammarNodeWithField -> T.Text
    evalNodeExpr n =
      case n of
        TSGN.Seq members ->
          let childCalls = map evalNodeExpr members
              containsThrow call = isThrowStatement call
           in case filter containsThrow childCalls of
                [] -> T.intercalate " + " childCalls
                (c : _) -> c
        TSGN.Choice alternatives ->
          if null alternatives
            then throwEvaluationError "Cannot evaluate CHOICE node: missing alternative"
            else generateChoiceExpression alternatives
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
    generateChoiceEvaluate :: [GrammarNodeWithField] -> T.Text
    generateChoiceEvaluate alternatives =
      let expr = evalNodeExpr (TSGN.Choice alternatives)
       in if isThrowStatement expr
            then TT.inst TTS.evaluate_method (TT.TArray [expr])
            else TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", expr, ";"]])

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
            else TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", expr, ";"]])

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
