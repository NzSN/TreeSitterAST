{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeSitterGrammarNodes
  ( GrammarNode(..),
    Node,
    PrecedenceValue (..),
    Nodes,
    Grammar (..),
    Grammar' (..),
    convert,
    parseGrammarFromJSON,
    parseGrammarFromFile,
    parseNodeFromJSON,
    parseNodeFromFile,
    isLeaf,
    resolveAlias,
    trimExternals,
    mapNode,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text.Lazy (Text, fromStrict)
import GHC.Generics

-- | Precedence value can be either a named precedence (string) or numeric precedence (int).
data PrecedenceValue
  = NamedPrecedence Text
  | NumericPrecedence Int
  deriving (Show, Eq, Generic, Ord)

instance ToJSON PrecedenceValue where
  toJSON (NamedPrecedence s) = toJSON s
  toJSON (NumericPrecedence n) = toJSON n

instance FromJSON PrecedenceValue where
  parseJSON v = case v of
    String s -> return $ NamedPrecedence $ fromStrict s
    Number n -> return $ NumericPrecedence (floor n)
    _ ->
      prependFailure
        "Parsing PrecedenceValue failed"
        (typeMismatch "String or Number" v)

-- | A grammar node.
data GrammarNode a
  = Seq {members :: [GrammarNode a]}
  | Choice {members :: [GrammarNode a]}
  | Repeat {content :: GrammarNode a}
  | Repeat1 {content :: GrammarNode a}
  | Symbol {name :: a}
  | StringLiteral {value :: a}
  | Pattern {value :: a}
  | Blank
  | Field {fieldName :: a, content :: GrammarNode a}
  | Alias {content :: GrammarNode a, named :: Bool, aliasValue :: a}
  | Token {content :: GrammarNode a}
  | ImmediateToken {content :: GrammarNode a}
  | Prec {precedence :: PrecedenceValue, content :: GrammarNode a}
  | PrecLeft {precedence :: PrecedenceValue, content :: GrammarNode a}
  | PrecRight {precedence :: PrecedenceValue, content :: GrammarNode a}
  | PrecDynamic {precedence :: PrecedenceValue, content :: GrammarNode a}
  | Reserved {content :: GrammarNode a, contextName :: a}
  | Empty
  deriving (Show, Eq, Generic, Ord)
instance Functor GrammarNode where
  fmap f node = case node of
    Seq members -> Seq (fmap (fmap f) members)
    Choice members -> Choice (fmap (fmap f) members)
    Repeat content -> Repeat (fmap f content)
    Repeat1 content -> Repeat1 (fmap f content)
    Symbol name -> Symbol (f name)
    StringLiteral value -> StringLiteral (f value)
    Pattern value -> Pattern (f value)
    Blank -> Blank
    Field fieldName content -> Field (f fieldName) (fmap f content)
    Alias content _named aliasValue -> Alias (fmap f content) _named (f aliasValue)
    Token content -> Token (fmap f content)
    ImmediateToken content -> ImmediateToken (fmap f content)
    Prec precedence content -> Prec precedence (fmap f content)
    PrecLeft precedence content -> PrecLeft precedence (fmap f content)
    PrecRight precedence content -> PrecRight precedence (fmap f content)
    PrecDynamic precedence content -> PrecDynamic precedence (fmap f content)
    Reserved content contextName -> Reserved (fmap f content) (f contextName)
    Empty -> Empty

instance Foldable GrammarNode where
  foldMap f node = case node of
    Seq members -> foldMap (foldMap f) members
    Choice members -> foldMap (foldMap f) members
    Repeat content -> foldMap f content
    Repeat1 content -> foldMap f content
    Symbol name -> f name
    StringLiteral value -> f value
    Pattern value -> f value
    Blank -> mempty
    Field fieldName content -> f fieldName <> foldMap f content
    Alias content _named aliasValue -> foldMap f content <> f aliasValue
    Token content -> foldMap f content
    ImmediateToken content -> foldMap f content
    Prec _ content -> foldMap f content
    PrecLeft _ content -> foldMap f content
    PrecRight _ content -> foldMap f content
    PrecDynamic _ content -> foldMap f content
    Reserved content contextName -> foldMap f content <> f contextName
    Empty -> mempty

instance Traversable GrammarNode where
  traverse f node = case node of
    Seq members -> Seq <$> traverse (traverse f) members
    Choice members -> Choice <$> traverse (traverse f) members
    Repeat content -> Repeat <$> traverse f content
    Symbol name -> Symbol <$> f name
    StringLiteral value -> StringLiteral <$> f value
    Pattern value -> Pattern <$> f value
    Blank -> pure Blank
    Field fieldName content -> Field <$> f fieldName <*> traverse f content
    Alias content _named aliasValue -> Alias <$> traverse f content <*> pure _named <*> f aliasValue
    Token content -> Token <$> traverse f content
    ImmediateToken content -> ImmediateToken <$> traverse f content
    Prec precedence content -> Prec precedence <$> traverse f content
    PrecLeft precedence content -> PrecLeft precedence <$> traverse f content
    PrecRight precedence content -> PrecRight precedence <$> traverse f content
    PrecDynamic precedence content -> PrecDynamic precedence <$> traverse f content
    Reserved content contextName -> Reserved <$> traverse f content <*> f contextName
    Empty -> pure Empty
type Node = GrammarNode Text

mapNode :: (Node -> Node) -> Node -> Node
mapNode tr n = tr $ case n of
  Seq members -> Seq (map (mapNode tr) members)
  Choice members -> Choice (map (mapNode tr) members)
  Repeat content -> Repeat (mapNode tr content)
  Repeat1 content -> Repeat1 (mapNode tr content)
  Symbol name -> Symbol name
  StringLiteral value -> StringLiteral value
  Pattern value -> Pattern value
  Blank -> Blank
  Field fieldName content -> Field fieldName (mapNode tr content)
  Alias content named aliasValue -> Alias (mapNode tr content) named aliasValue
  Token content -> Token (mapNode tr content)
  ImmediateToken content -> ImmediateToken (mapNode tr content)
  Prec precedence content -> Prec precedence (mapNode tr content)
  PrecLeft precedence content -> PrecLeft precedence (mapNode tr content)
  PrecRight precedence content -> PrecRight precedence (mapNode tr content)
  PrecDynamic precedence content -> PrecDynamic precedence (mapNode tr content)
  Reserved content contextName -> Reserved (mapNode tr content) contextName
  Empty -> Empty

-- | Apply a node transformation to relevant fields of a grammar.
applyNodeTransform :: (Node -> Node) -> Bool -> Grammar -> Grammar
applyNodeTransform nodeTransform includeExternals grammar =
  grammar
    { grammarNodes = Map.map nodeTransform grammar.grammarNodes,
      grammarExternals = if includeExternals then fmap (map nodeTransform) (grammarExternals grammar) else grammarExternals grammar,
      grammarInline = fmap (map nodeTransform) (grammarInline grammar),
      grammarSupertypes = fmap (map nodeTransform) (grammarSupertypes grammar),
      grammarReserved = fmap (Map.map (map nodeTransform)) (grammarReserved grammar)
    }

-- | Extract identifier from a node (Symbol name, StringLiteral value, Pattern value).
nodeIdentifier :: Node -> Maybe Text
nodeIdentifier (Symbol name) = Just name
nodeIdentifier (StringLiteral value) = Just value
nodeIdentifier (Pattern value) = Just value
nodeIdentifier _ = Nothing

-- | Get the set of identifiers from grammar's external definitions.
getExternalsSet :: Grammar -> Set.Set Text
getExternalsSet grammar = Set.fromList $ maybe [] (mapMaybe nodeIdentifier) (grammarExternals grammar)

-- | Trim out all Symbol node such that it's name present
-- in external in Grammar.
trimExternals :: Grammar -> Grammar
trimExternals grammar = applyNodeTransform trimNode False grammar
  where
    externalsSet = getExternalsSet grammar
    trimNode = mapNode replaceExternal
    replaceExternal node = case node of
      Symbol name | name `Set.member` externalsSet -> Empty
      _ -> node

resolveAlias :: Grammar -> Grammar
resolveAlias grammar = applyNodeTransform resolveNode True grammar
  where
    externalsSet = getExternalsSet grammar
    resolveNode = mapNode replaceAlias
    replaceAlias (Alias content _named _aliasValue) =
      case nodeIdentifier content of
        -- \| TODO: Need to deal with external node that ref by
        -- \|       alias node.
        Just ident | ident `Set.member` externalsSet -> Empty
        _ -> content
    replaceAlias node = node

isLeaf :: Node -> Bool
isLeaf (StringLiteral _) = True
isLeaf (Pattern _) = True
isLeaf Blank = True
isLeaf (Token _) = True
isLeaf (ImmediateToken _) = True
isLeaf (Alias content _ _) = isLeaf content
isLeaf (Prec _ content) = isLeaf content
isLeaf (PrecLeft _ content) = isLeaf content
isLeaf (PrecRight _ content) = isLeaf content
isLeaf (PrecDynamic _ content) = isLeaf content
isLeaf (Reserved content _) = isLeaf content
-- Symbol could be token or non-token; assume non-token (interior)
isLeaf _ = False

instance ToJSON Node where
  toJSON Empty = Null
  toJSON (Seq members) =
    object
      [ "type" .= ("SEQ" :: Text),
        "members" .= members
      ]
  toJSON (Choice members) =
    object
      [ "type" .= ("CHOICE" :: Text),
        "members" .= members
      ]
  toJSON (Repeat content) =
    object
      [ "type" .= ("REPEAT" :: Text),
        "content" .= content
      ]
  toJSON (Repeat1 content) =
    object
      [ "type" .= ("REPEAT1" :: Text),
        "content" .= content
      ]
  toJSON (Symbol name) =
    object
      [ "type" .= ("SYMBOL" :: Text),
        "name" .= name
      ]
  toJSON (StringLiteral value) =
    object
      [ "type" .= ("STRING" :: Text),
        "value" .= value
      ]
  toJSON (Pattern value) =
    object
      [ "type" .= ("PATTERN" :: Text),
        "value" .= value
      ]
  toJSON Blank =
    object
      [ "type" .= ("BLANK" :: Text)
      ]
  toJSON (Field fieldName content) =
    object
      [ "type" .= ("FIELD" :: Text),
        "name" .= fieldName,
        "content" .= content
      ]
  toJSON (Alias content named aliasValue) =
    object
      [ "type" .= ("ALIAS" :: Text),
        "content" .= content,
        "named" .= named,
        "value" .= aliasValue
      ]
  toJSON (Token content) =
    object
      [ "type" .= ("TOKEN" :: Text),
        "content" .= content
      ]
  toJSON (ImmediateToken content) =
    object
      [ "type" .= ("IMMEDIATE_TOKEN" :: Text),
        "content" .= content
      ]
  toJSON (Prec precedence content) =
    object
      [ "type" .= ("PREC" :: Text),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (PrecLeft precedence content) =
    object
      [ "type" .= ("PREC_LEFT" :: Text),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (PrecRight precedence content) =
    object
      [ "type" .= ("PREC_RIGHT" :: Text),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (PrecDynamic precedence content) =
    object
      [ "type" .= ("PREC_DYNAMIC" :: Text),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (Reserved content contextName) =
    object
      [ "type" .= ("RESERVED" :: Text),
        "content" .= content,
        "context_name" .= contextName
      ]

instance FromJSON Node where
  parseJSON (Object v) = do
    typ <- v .: "type"
    case typ of
      "SEQ" -> Seq <$> v .: "members"
      "CHOICE" -> Choice <$> v .: "members"
      "REPEAT" -> Repeat <$> v .: "content"
      "REPEAT1" -> Repeat1 <$> v .: "content"
      "SYMBOL" -> Symbol <$> v .: "name"
      "STRING" -> StringLiteral <$> v .: "value"
      "PATTERN" -> Pattern <$> v .: "value"
      "BLANK" -> return Blank
      "FIELD" -> Field <$> v .: "name" <*> v .: "content"
      "ALIAS" -> Alias <$> v .: "content" <*> v .: "named" <*> v .: "value"
      "TOKEN" -> Token <$> v .: "content"
      "IMMEDIATE_TOKEN" -> ImmediateToken <$> v .: "content"
      "PREC" -> Prec <$> v .: "value" <*> v .: "content"
      "PREC_LEFT" -> PrecLeft <$> v .: "value" <*> v .: "content"
      "PREC_RIGHT" -> PrecRight <$> v .: "value" <*> v .: "content"
      "PREC_DYNAMIC" -> PrecDynamic <$> v .: "value" <*> v .: "content"
      "RESERVED" -> Reserved <$> v .: "content" <*> v .: "context_name"
      unknown -> fail $ "Unknown node type: " ++ unknown
  parseJSON invalid =
    prependFailure
      "Parsing Node failed"
      (typeMismatch "Object" invalid)

-- | A map from node name to node definition.
type Nodes = Map String Node

-- | A form of Grammar that all intermidiate information of
-- | nodes are resolved into lower node as possible.
newtype Grammar' = Grammar' {orig :: Grammar}

convert :: Grammar -> Grammar'
convert = Grammar' . trimExternals . resolveAlias

-- | Full grammar definition.
data Grammar = Grammar
  { grammarName :: Text,
    grammarWord :: Maybe String,
    grammarNodes :: Nodes,
    grammarExternals :: Maybe [Node],
    grammarInline :: Maybe [Node],
    grammarSupertypes :: Maybe [Node],
    grammarReserved :: Maybe (Map Text [Node])
  }
  deriving (Show, Eq, Generic)

instance ToJSON Grammar where
  toJSON (Grammar name word nodes externals inline supertypes reserved) =
    object $
      [ "name" .= name,
        "rules" .= nodes
      ]
        ++ maybe [] (\w -> ["word" .= w]) word
        ++ maybe [] (\e -> ["externals" .= e]) externals
        ++ maybe [] (\i -> ["inline" .= map (\case Symbol name_ -> name_; _ -> error "non-symbol in inline") i]) inline
        ++ maybe [] (\s -> ["supertypes" .= map (\case Symbol name_ -> name_; _ -> error "non-symbol in supertypes") s]) supertypes
        ++ maybe [] (\r -> ["reserved" .= r]) reserved

instance FromJSON Grammar where
  parseJSON (Object v) = do
    name <- v .: "name"
    word <- v .:? "word"
    nodes <- v .: "rules"
    externals <- v .:? "externals"
    reserved <- v .:? "reserved"
    inline <- fmap (fmap (map Symbol)) (v .:? "inline")
    supertypes <- fmap (fmap (map Symbol)) (v .:? "supertypes")
    return $ Grammar name word nodes externals inline supertypes reserved
  parseJSON invalid =
    prependFailure
      "Parsing Grammar failed"
      (typeMismatch "Object" invalid)

parseGrammarFromJSON :: BS.ByteString -> Maybe Grammar
parseGrammarFromJSON = decode

parseGrammarFromFile :: String -> MaybeT IO Grammar
parseGrammarFromFile path = do
  content <- lift $ BS.readFile path
  case decode content of
    Nothing -> MaybeT $ return Nothing
    Just grammar -> return grammar

parseNodeFromJSON :: String -> Maybe Node
parseNodeFromJSON jsonStr = decode $ BS.pack jsonStr

parseNodeFromFile :: String -> MaybeT IO Node
parseNodeFromFile path = do
  content <- lift $ readFile path
  case decode (BS.pack content) of
    Nothing -> MaybeT $ return Nothing
    Just node -> return node
