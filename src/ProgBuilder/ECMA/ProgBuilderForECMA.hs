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
import Fundamentals.Generation (GenerationStrategy(..), evaluateNode, defaultContext)
import TypedASTGenerator.NodeDescriptionHelper
import Utility (upper_the_first_char)

descript :: TSGN.Grammar -> String
descript grammar =
  let processed = TSGN.convert grammar
      origGrammar = TSGN.orig processed
      transformedGrammar = trans origGrammar
      rules = TSGN.grammarNodes transformedGrammar
      builder_def = Map.foldrWithKey (\name rule acc -> T.concat [acc, build name rule]) "" rules
   in T.unpack $ T.concat [imports, prologue, builder_def]

imports :: T.Text
imports =
  T.concat [TT.inst
    TTS.import_statement
    (TT.TArray ["strict as assert"])
    "assert", "\n"]

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
      fields = propsOfNode rule
      (baseClass, constructorDef, props) =
        if isLeaf rule
          then
            ( "SyntaticLeaf",
              leafConstructor,
              Nothing
            )
          else
            ( "SyntaticInterior",
              interiorConstructor fields,
              Just $ TT.TArray $ collapse' $ propFromTSGNs fields
            )
      evaluateMethod = if isLeaf rule then "" else generateEvaluateMethod rule fields
      factoryMethod = generateFactoryMethods (isLeaf rule) (T.unpack className) fields
      allMethods = [constructorDef] ++
                   (if T.null evaluateMethod then [] else [evaluateMethod]) ++
                   (if T.null factoryMethod then [] else [factoryMethod])
      methods = if null allMethods then Nothing else Just $ TT.TArray allMethods
   in T.concat
        [ TT.inst TTS.export_qualifier $
            TT.inst TTS.class_declare className (Just baseClass) props methods,
          "\n"
        ]
  where
    leafConstructor :: T.Text
    leafConstructor =
      TT.inst
        TTS.const_declare
        (TT.TArray [TT.inst TTS.parameter_declare "value" "string"])
        (TT.TArray [TT.inst TTS.function_call "super" (Just $ TT.TArray [TT.inst TTS.var_ref "value"])])

    interiorConstructor :: [Property] -> T.Text
    interiorConstructor props =
      TT.inst
        TTS.const_declare
        (TT.TArray $ filter (not . T.null) $ map generateConstructorParam $ zip [0..] props)
        ( TT.TArray
            (interiorPrologueStmts ++ filter (not . T.null) (map generateConstructorAssignment (zip [0..] props)))
        )
      where
        generateConstructorParam :: (Int, Property) -> T.Text
        generateConstructorParam (idx, prop) =
          case prop of
            SymbolProp name _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in TT.inst TTS.parameter_declare (T.concat [name, T.pack $ "_" ++ show fieldIdx]) (T.pack $ upper_the_first_char (T.unpack name) ++ "_T")
            NamedProp name _ _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in TT.inst TTS.parameter_declare (T.concat [name, T.pack $ "_" ++ show fieldIdx]) (T.pack $ upper_the_first_char (T.unpack name) ++ "_T")
            StrProp _ _ -> ""  -- Skip string literal parameters
          where
            countNonStrPropsBefore :: Int -> [Property] -> Int
            countNonStrPropsBefore targetIdx props' =
              length $ filter (not . isStrProp) $ take targetIdx props'

            isStrProp :: Property -> Bool
            isStrProp (StrProp _ _) = True
            isStrProp _ = False

        generateConstructorAssignment :: (Int, Property) -> T.Text
        generateConstructorAssignment (idx, prop) =
          case prop of
            SymbolProp name _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in T.concat ["this.", name, T.pack $ "_" ++ show fieldIdx, "_i = ", name, T.pack $ "_" ++ show fieldIdx, ";"]
            NamedProp name _ _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in T.concat ["this.", name, T.pack $ "_" ++ show fieldIdx, "_i = ", name, T.pack $ "_" ++ show fieldIdx, ";"]
            StrProp _ _ -> ""  -- Skip string literal assignments
          where
            countNonStrPropsBefore :: Int -> [Property] -> Int
            countNonStrPropsBefore targetIdx props' =
              length $ filter (not . isStrProp) $ take targetIdx props'

            isStrProp :: Property -> Bool
            isStrProp (StrProp _ _) = True
            isStrProp _ = False

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

propFromTSGNs :: [Property] -> [Field]
propFromTSGNs = foldl (appendIDX 0) [] . (L.group . L.sort . propFromTSGNs')
  where
    appendIDX :: Int -> [Field] -> [Field] -> [Field]
    appendIDX i r (Field f_name f_type : xs) =
      Field (modifyFieldName i f_name) f_type : appendIDX (i + 1) [] xs ++ r
    appendIDX i r (SumField f_name f_types : xs) =
      SumField (modifyFieldName i f_name) f_types : appendIDX (i + 1) [] xs ++ r
    appendIDX i r (EmptyField : xs) = appendIDX i [] xs ++ r
    appendIDX _ _ [] = []

    modifyFieldName :: Int -> T.Text -> T.Text
    modifyFieldName i t = T.concat [t, T.pack . ("_" ++) . show $ i]

propFromTSGNs' :: [Property] -> [Field]
propFromTSGNs' = map propFromTSGN

propFromTSGN :: Property -> Field
propFromTSGN x
  | (SymbolProp p_type _) <- x = Field p_type p_type
  | (StrProp _ _) <- x = EmptyField
  | (NamedProp p_name p_types _) <- x = do
      SumField p_name $ map (asTypeStr . propType) p_types
  where
    asTypeStr :: Maybe T.Text -> T.Text
    asTypeStr x'
      | Nothing <- x' = "string"
      | Just s  <- x' = s

-- | Generate static factory methods for a class
generateFactoryMethods :: Bool -> String -> [Property] -> T.Text
generateFactoryMethods isLeafNode className properties =
  if null properties && not isLeafNode
    then ""
    else
      let methodName = T.pack $ "create" ++ upper_the_first_char className
          params = generateFactoryParams isLeafNode properties
          body = generateFactoryBody isLeafNode className properties
      in TT.inst TTS.static_factory_method methodName params (T.pack className) body
  where
    generateFactoryParams :: Bool -> [Property] -> TT.TArray T.Text
    generateFactoryParams isLeaf props =
      if isLeaf
        then TT.TArray [TT.inst TTS.parameter_declare "value" "string"]
        else TT.TArray $ filter (not . T.null) $ map generateParam $ zip [0..] props
      where
        generateParam :: (Int, Property) -> T.Text
        generateParam (idx, prop) =
          case prop of
            SymbolProp name _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in TT.inst TTS.parameter_declare (T.concat [name, T.pack $ "_" ++ show fieldIdx]) (T.pack $ upper_the_first_char (T.unpack name) ++ "_T")
            NamedProp name _ _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in TT.inst TTS.parameter_declare (T.concat [name, T.pack $ "_" ++ show fieldIdx]) (T.pack $ upper_the_first_char (T.unpack name) ++ "_T")
            StrProp _ _ -> ""  -- Skip string literal parameters
          where
            countNonStrPropsBefore :: Int -> [Property] -> Int
            countNonStrPropsBefore targetIdx props' =
              length $ filter (not . isStrProp) $ take targetIdx props'

            isStrProp :: Property -> Bool
            isStrProp (StrProp _ _) = True
            isStrProp _ = False

    generateFactoryBody :: Bool -> String -> [Property] -> T.Text
    generateFactoryBody isLeaf clsName props =
      if isLeaf
        then
          let instanceVar = T.pack $ "return new " ++ clsName ++ "(value);"
          in T.concat [instanceVar, "\n"]
        else
          let instanceVar = T.pack $ "const instance = new " ++ clsName ++ "();"
              assignments = filter (not . T.null) $ map generateAssignment $ zip [0..] props
              returnStmt = T.pack $ "return instance;"
              stmts = instanceVar : assignments ++ [returnStmt]
          in T.concat $ map (\s -> T.concat [s, "\n"]) stmts
      where
        generateAssignment :: (Int, Property) -> T.Text
        generateAssignment (idx, prop) =
          case prop of
            SymbolProp name _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in T.concat ["instance.", name, T.pack $ "_" ++ show fieldIdx, "_i = ", name, T.pack $ "_" ++ show fieldIdx, ";"]
            NamedProp name _ _ ->
              let fieldIdx = countNonStrPropsBefore idx props
              in T.concat ["instance.", name, T.pack $ "_" ++ show fieldIdx, "_i = ", name, T.pack $ "_" ++ show fieldIdx, ";"]
            StrProp _ _ -> ""  -- Skip string literal assignments
          where
            countNonStrPropsBefore :: Int -> [Property] -> Int
            countNonStrPropsBefore targetIdx props' =
              length $ filter (not . isStrProp) $ take targetIdx props'

            isStrProp :: Property -> Bool
            isStrProp (StrProp _ _) = True
            isStrProp _ = False

-- | Generate evaluate() method for a grammar node
generateEvaluateMethod :: TSGN.Node -> [Property] -> T.Text
generateEvaluateMethod node properties =
  case node of
    TSGN.Seq members -> generateSeqEvaluate members properties
    TSGN.Choice alternatives -> generateChoiceEvaluate alternatives properties
    TSGN.Repeat content -> generateRepeatEvaluate content properties
    TSGN.Repeat1 content -> generateRepeat1Evaluate content properties
    TSGN.Symbol name -> generateSymbolEvaluate name properties
    TSGN.StringLiteral _ -> generateLiteralEvaluate
    TSGN.Pattern _ -> generateLiteralEvaluate
    TSGN.Blank -> generateBlankEvaluate
    TSGN.Field _ content -> generateEvaluateMethod content properties
    TSGN.Alias content _ _ -> generateEvaluateMethod content properties
    TSGN.Token content -> generateEvaluateMethod content properties
    TSGN.ImmediateToken content -> generateEvaluateMethod content properties
    TSGN.Prec _ content -> generateEvaluateMethod content properties
    TSGN.PrecLeft _ content -> generateEvaluateMethod content properties
    TSGN.PrecRight _ content -> generateEvaluateMethod content properties
    TSGN.PrecDynamic _ content -> generateEvaluateMethod content properties
    TSGN.Reserved content _ -> generateEvaluateMethod content properties
    TSGN.Empty -> generateEmptyEvaluate
  where
    -- Generate evaluate() for SEQ nodes: concatenate children results
    generateSeqEvaluate :: [TSGN.Node] -> [Property] -> T.Text
    generateSeqEvaluate members props =
      let childCalls = map (generateMemberCall props) (zip [0..] members)
          joinedCalls = T.intercalate " + " childCalls
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", joinedCalls, ";"]])
      where
        generateMemberCall :: [Property] -> (Int, TSGN.Node) -> T.Text
        generateMemberCall props' (idx, member) =
          case member of
            TSGN.StringLiteral s -> T.concat ["\"", s, "\""]
            TSGN.Pattern s -> T.concat ["\"", s, "\""]
            _ ->
              let propIdx = countNonStringMembersBefore idx members
                  actualIdx = getNthNonStrPropIndex propIdx props'
              in if actualIdx < length props'
                 then T.concat ["this.", getPropertyName actualIdx props', ".evaluate()"]
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
    generateChoiceEvaluate :: [TSGN.Node] -> [Property] -> T.Text
    generateChoiceEvaluate alternatives props =
      let -- For now, just evaluate the first alternative
          childCall = T.concat ["this.", getPropertyName 0 props, ".evaluate()"]
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", childCall, ";"]])

    -- Generate evaluate() for REPEAT nodes: generate 0-n repetitions
    generateRepeatEvaluate :: TSGN.Node -> [Property] -> T.Text
    generateRepeatEvaluate content props =
      let childCall = T.concat ["this.", getPropertyName 0 props, ".evaluate()"]
          -- Simple implementation: just evaluate once for now
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", childCall, ";"]])

    -- Generate evaluate() for REPEAT1 nodes: generate 1-n repetitions
    generateRepeat1Evaluate :: TSGN.Node -> [Property] -> T.Text
    generateRepeat1Evaluate content props =
      let childCall = T.concat ["this.", getPropertyName 0 props, ".evaluate()"]
      in TT.inst TTS.evaluate_method (TT.TArray [T.concat ["return ", childCall, ";"]])

    -- Generate evaluate() for SYMBOL nodes: delegate to referenced class
    generateSymbolEvaluate :: T.Text -> [Property] -> T.Text
    generateSymbolEvaluate name props =
      let childCall = T.concat ["this.", getPropertyName 0 props, ".evaluate()"]
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
    getPropertyName :: Int -> [Property] -> T.Text
    getPropertyName idx props =
      if idx < length props
        then case props !! idx of
          SymbolProp name _ ->
            let fieldIdx = countNonStrPropsBefore idx props
            in T.concat [name, T.pack $ "_" ++ show fieldIdx, "_i"]
          NamedProp name _ _ ->
            let fieldIdx = countNonStrPropsBefore idx props
            in T.concat [name, T.pack $ "_" ++ show fieldIdx, "_i"]
          StrProp _ _ -> "value_"
        else "unknown"
      where
        countNonStrPropsBefore :: Int -> [Property] -> Int
        countNonStrPropsBefore targetIdx props' =
          length $ filter (not . isStrProp) $ take targetIdx props'

        isStrProp :: Property -> Bool
        isStrProp (StrProp _ _) = True
        isStrProp _ = False

-- | ECMA Knowledge
isBuiltin :: T.Text -> Bool
isBuiltin x
  | x == "string" = True
  | otherwise = False
