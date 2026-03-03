{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgBuilder.ECMA.ProgBuilderForECMA where

import Data.List as L
import Data.Map qualified as Map
import Data.Text.Lazy qualified as T
import ProgBuilder.ProgBuilderDescription
  ( Property (..),
    propsOfNode, propType,
  )
import Template.Template qualified as TT
import Template.TypeScriptTemplate qualified as TTS
import TreeSitterGrammarNodes (isLeaf)
import TreeSitterGrammarNodes qualified as TSGN
import Fundamentals.Inference (trans)
import TypedASTGenerator.NodeDescriptionHelper
import Utility (upper_the_first_char)

descript :: TSGN.Grammar -> String
descript grammar =
  let processed = TSGN.convert grammar
      origGrammar = TSGN.orig processed
      transformedGrammar = trans origGrammar
      rules = TSGN.grammarNodes transformedGrammar
      builder_def = Map.foldrWithKey (\name rule acc -> T.concat [acc, build name rule]) "" rules
   in T.unpack $ T.concat [prologue, builder_def]

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

build :: String -> TSGN.Node -> T.Text
build name rule =
  let className = T.pack $ node_type_ident name
      props = propsOfNode rule
      fields = fieldsFromProperties props
      (baseClass, constructorDef, fieldDecls) =
        if isLeaf rule
          then
            ( "SyntaticLeaf",
              leafConstructor,
              Nothing
            )
          else
            ( "SyntaticInterior",
              interiorConstructor fields,
              Just $ TT.TArray $ collapse' fields
            )
      evaluateMethod = if isLeaf rule then "" else generateEvaluateMethod rule fields  -- TODO: change to fields
      factoryMethod = generateFactoryMethods (isLeaf rule) (T.unpack className) fields  -- TODO: change to fields
      allMethods = [constructorDef] ++
                   (if T.null evaluateMethod then [] else [evaluateMethod]) ++
                   (if T.null factoryMethod then [] else [factoryMethod])
      methods = if null allMethods then Nothing else Just $ TT.TArray allMethods
   in T.concat
        [ TT.inst TTS.export_qualifier $
            TT.inst TTS.class_declare className (Just baseClass) fieldDecls methods,
          "\n"
        ]
  where
    leafConstructor :: T.Text
    leafConstructor =
      TT.inst
        TTS.const_declare
        (TT.TArray [TT.inst TTS.parameter_declare "value" "string"])
        (TT.TArray [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "value"])])

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
            EmptyField -> ""  -- Skip empty fields

        generateConstructorAssignment :: Field -> T.Text
        generateConstructorAssignment field =
          case field of
            Field name _ ->
              T.concat ["this.", name, "_i = ", name, ";"]
            SumField name _ ->
              T.concat ["this.", name, "_i = ", name, ";"]
            EmptyField -> ""  -- Skip empty fields

    interiorPrologueStmts :: [T.Text]
    interiorPrologueStmts =
      [TT.inst TTS.function_call "super" Nothing]

-- | Represent a property field of a Javascript class.
data Field
  = Field {field_name :: T.Text, field_type :: T.Text}
  | SumField {field_name :: T.Text, field_types :: [T.Text]}
  | EmptyField
  deriving (Show, Eq, Ord)

eval :: Field -> T.Text
eval f
  | field@(Field _ _) <- f = evaluate field
  | field@(SumField _ _) <- f = evaluate field
  | EmptyField <- f = ""
  where
    evaluate field = T.concat [evalFieldName field, " : ", evalFieldType field]

evalFieldName :: Field -> T.Text
evalFieldName f
  | (Field f_name _) <- f = T.concat [f_name, "_i"]
  | (SumField f_name _) <- f = T.concat [f_name, "_i"]
  | EmptyField <- f = ""

evalFieldType :: Field -> T.Text
evalFieldType f
  | (Field _ f_type) <- f =
      T.pack $ upper_the_first_char (T.unpack f_type) ++ "_T"
  | field@(SumField _ _) <- f = collapseSumType field
  | EmptyField <- f = ""
  where
    collapseSumType :: Field -> T.Text
    collapseSumType (SumField _ []) = T.pack "undefined"
    collapseSumType (SumField _ types) =
      T.pack $ L.intercalate " | " $
        nub $ map typeShow types
    -- Unreachable
    collapseSumType _ = undefined

    typeShow :: T.Text -> String
    typeShow x = if isBuiltin x
                  then T.unpack x
                  else (++ "_T") . upper_the_first_char . T.unpack $ x


collapse' :: [Field] -> [T.Text]
collapse' = map (\f -> T.concat [eval f, ";\n"])


propFromTSGN :: Property -> Field
propFromTSGN x
  | (SymbolProp p_type _) <- x = Field p_type p_type
  | (StrProp _ _) <- x = EmptyField
  | (NamedProp p_name p_types _) <- x = SumField p_name $ map (asTypeStr . propType) p_types
  where
    asTypeStr :: Maybe T.Text -> T.Text
    asTypeStr x'
      | Nothing <- x' = "string"
      | Just s  <- x' = s

isStrProp :: Property -> Bool
isStrProp (StrProp _ _) = True
isStrProp _ = False

fieldsFromProperties :: [Property] -> [Field]
fieldsFromProperties props = go 0 props
  where
    go _ [] = []
    go idx (prop : rest)
      | isStrProp prop = go idx rest  -- skip string literals, no field
      | otherwise =
          let baseField = propFromTSGN prop
              suffixedField = case baseField of
                Field name ftype -> Field (T.concat [name, T.pack $ "_" ++ show idx]) ftype
                SumField name ftypes -> SumField (T.concat [name, T.pack $ "_" ++ show idx]) ftypes
                EmptyField -> EmptyField  -- shouldn't happen since StrProp already filtered
          in suffixedField : go (idx + 1) rest

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
generateEvaluateMethod :: TSGN.Node -> [Field] -> T.Text
generateEvaluateMethod node fields =
  case node of
    TSGN.Seq members -> generateSeqEvaluate members fields
    TSGN.Choice alternatives -> generateChoiceEvaluate alternatives fields
    TSGN.Repeat content -> generateRepeatEvaluate content fields
    TSGN.Repeat1 content -> generateRepeat1Evaluate content fields
    TSGN.Symbol name -> generateSymbolEvaluate name fields
    TSGN.StringLiteral _ -> generateLiteralEvaluate
    TSGN.Pattern _ -> generateLiteralEvaluate
    TSGN.Blank -> generateBlankEvaluate
    TSGN.Field _ content -> generateEvaluateMethod content fields
    TSGN.Alias content _ _ -> generateEvaluateMethod content fields
    TSGN.Token content -> generateEvaluateMethod content fields
    TSGN.ImmediateToken content -> generateEvaluateMethod content fields
    TSGN.Prec _ content -> generateEvaluateMethod content fields
    TSGN.PrecLeft _ content -> generateEvaluateMethod content fields
    TSGN.PrecRight _ content -> generateEvaluateMethod content fields
    TSGN.PrecDynamic _ content -> generateEvaluateMethod content fields
    TSGN.Reserved content _ -> generateEvaluateMethod content fields
    TSGN.Empty -> generateEmptyEvaluate
  where
    -- Generate evaluate() for SEQ nodes: concatenate children results
    generateSeqEvaluate :: [TSGN.Node] -> [Field] -> T.Text
    generateSeqEvaluate members fields =
      let childCalls = map (generateMemberCall fields) (zip [0..] members)
          joinedCalls = T.intercalate " + " childCalls
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", joinedCalls, ";"]])
      where
        -- Escape a string for use in TypeScript double-quoted string literal
        escapeTypeScriptString :: T.Text -> T.Text
        escapeTypeScriptString s = T.concatMap escapeChar s
          where
            escapeChar '"' = "\\\""
            escapeChar '\\' = "\\\\"
            escapeChar c = T.singleton c

        generateMemberCall :: [Field] -> (Int, TSGN.Node) -> T.Text
        generateMemberCall fields'' (idx, member) =
          case member of
            TSGN.StringLiteral s -> T.concat ["\"", escapeTypeScriptString s, "\""]
            TSGN.Pattern s -> T.concat ["\"", escapeTypeScriptString s, "\""]
            _ ->
              let fieldIdx = countNonStringMembersBefore idx members
              in if fieldIdx < length fields''
                 then T.concat ["this.", evalFieldName (fields'' !! fieldIdx), ".evaluate()"]
                 else T.concat ["this.unknown.evaluate()"]  -- Shouldn't happen

        getNthNonStrPropIndex :: Int -> [Property] -> Int
        getNthNonStrPropIndex n props'' =
          let nonStrIndices = map fst $ filter (\(_, p) -> not (isStrProp' p)) (zip [0..] props'')
          in if n < length nonStrIndices then nonStrIndices !! n else length props''  -- Fallback

        isStrProp' :: Property -> Bool
        isStrProp' (StrProp _ _) = True
        isStrProp' _ = False

        countNonStringMembersBefore :: Int -> [TSGN.Node] -> Int
        countNonStringMembersBefore targetIdx members' =
          length $ filter (not . isStringMember) $ take targetIdx members'

        isStringMember :: TSGN.Node -> Bool
        isStringMember (TSGN.StringLiteral _) = True
        isStringMember (TSGN.Pattern _) = True
        isStringMember _ = False

    -- Generate evaluate() for CHOICE nodes: need to handle choice resolution
    generateChoiceEvaluate :: [TSGN.Node] -> [Field] -> T.Text
    generateChoiceEvaluate alternatives fields' =
      let -- For now, just evaluate the first alternative
          childCall = if not (null fields')
                      then T.concat ["this.", evalFieldName (head fields'), ".evaluate()"]
                      else T.concat ["this.unknown.evaluate()"]
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", childCall, ";"]])

    -- Generate evaluate() for REPEAT nodes: generate 0-n repetitions
    generateRepeatEvaluate :: TSGN.Node -> [Field] -> T.Text
    generateRepeatEvaluate content fields' =
      let childCall = if not (null fields')
                      then T.concat ["this.", evalFieldName (head fields'), ".evaluate()"]
                      else T.concat ["this.unknown.evaluate()"]
          -- Simple implementation: just evaluate once for now
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", childCall, ";"]])

    -- Generate evaluate() for REPEAT1 nodes: generate 1-n repetitions
    generateRepeat1Evaluate :: TSGN.Node -> [Field] -> T.Text
    generateRepeat1Evaluate content fields' =
      let childCall = if not (null fields')
                      then T.concat ["this.", evalFieldName (head fields'), ".evaluate()"]
                      else T.concat ["this.unknown.evaluate()"]
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", childCall, ";"]])

    -- Generate evaluate() for SYMBOL nodes: delegate to referenced class
    generateSymbolEvaluate :: T.Text -> [Field] -> T.Text
    generateSymbolEvaluate name fields' =
      let childCall = if not (null fields')
                      then T.concat ["this.", evalFieldName (head fields'), ".evaluate()"]
                      else T.concat ["this.unknown.evaluate()"]
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", childCall, ";"]])

    -- Generate evaluate() for literal nodes: return value
    generateLiteralEvaluate :: T.Text
    generateLiteralEvaluate =
      TT.inst TTS.evaluate_method (TT.TArray ["return this.value_;"])

    -- Generate evaluate() for BLANK nodes: return empty string
    generateBlankEvaluate :: T.Text
    generateBlankEvaluate =
      TT.inst TTS.evaluate_method (TT.TArray ["return \"\";"])

    -- Generate evaluate() for EMPTY nodes: return empty string
    generateEmptyEvaluate :: T.Text
    generateEmptyEvaluate =
      TT.inst TTS.evaluate_method (TT.TArray ["return \"\";"])

    -- Helper to get property name by index (with _i suffix)

-- | ECMA Knowledge
isBuiltin :: T.Text -> Bool
isBuiltin x
  | x == "string" = True
  | otherwise = False

-- | Get TypeScript type string for a Property
propertyTypeStr :: Property -> T.Text
propertyTypeStr prop = case prop of
  SymbolProp name _ ->
    T.pack $ upper_the_first_char (T.unpack name) ++ "_T"
  NamedProp _ p_types _ ->
    let types = map (asTypeStr . propType) p_types
        typeStrs = nub $ map typeShow types
    in if null typeStrs
        then "undefined"
        else T.pack $ L.intercalate " | " typeStrs
  StrProp _ _ -> "string"
  where
    asTypeStr :: Maybe T.Text -> T.Text
    asTypeStr Nothing = "string"
    asTypeStr (Just s) = s

    typeShow :: T.Text -> String
    typeShow x = if isBuiltin x
                  then T.unpack x
                  else (++ "_T") . upper_the_first_char . T.unpack $ x
