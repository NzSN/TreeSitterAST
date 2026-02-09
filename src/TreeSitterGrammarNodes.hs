{-# LANGUAGE OverloadedStrings #-}

module TreeSitterGrammarNodes
  ( Node (..),
    PrecedenceValue (..),
    Nodes,
    Grammar (..),
    parseGrammarFromJSON,
    parseGrammarFromFile,
    parseNodeFromJSON,
    parseNodeFromFile,
    isLeaf,
    mapNode,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text.Lazy (Text, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BS
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
data Node
  = Seq            {members :: [Node]}
  | Choice         {members :: [Node]}
  | Repeat         {content :: Node}
  | Repeat1        {content :: Node}
  | Symbol         {name :: Text}
  | StringLiteral  {value :: Text}
  | Pattern        {value :: Text}
  | Blank
  | Field          {fieldName :: Text, content :: Node}
  | Alias          {content :: Node, named :: Bool, aliasValue :: Text}
  | Token          {content :: Node}
  | ImmediateToken {content :: Node}
  | Prec           {precedence :: PrecedenceValue, content :: Node}
  | PrecLeft       {precedence :: PrecedenceValue, content :: Node}
  | PrecRight      {precedence :: PrecedenceValue, content :: Node}
  | PrecDynamic    {precedence :: PrecedenceValue, content :: Node}
  | Reserved       {content :: Node, contextName :: Text}
  | Empty
  deriving (Show, Eq, Generic, Ord)

mapNode :: (Node -> Node) -> Node -> Node
mapNode tr n = tr $ case n of
  Seq members                    -> Seq (map (mapNode tr) members)
  Choice members                 -> Choice (map (mapNode tr) members)
  Repeat content                 -> Repeat (mapNode tr content)
  Repeat1 content                -> Repeat1 (mapNode tr content)
  Symbol name                    -> Symbol name
  StringLiteral value            -> StringLiteral value
  Pattern value                  -> Pattern value
  Blank                          -> Blank
  Field fieldName content        -> Field fieldName (mapNode tr content)
  Alias content named aliasValue -> Alias (mapNode tr content) named aliasValue
  Token content                  -> Token (mapNode tr content)
  ImmediateToken content         -> ImmediateToken (mapNode tr content)
  Prec precedence content        -> Prec precedence (mapNode tr content)
  PrecLeft precedence content    -> PrecLeft precedence (mapNode tr content)
  PrecRight precedence content   -> PrecRight precedence (mapNode tr content)
  PrecDynamic precedence content -> PrecDynamic precedence (mapNode tr content)
  Reserved content contextName   -> Reserved (mapNode tr content) contextName
  Empty                          -> Empty

resolveAlias :: Grammar -> Grammar
resolveAlias = undefined

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
    object [ "type" .= ("TOKEN" :: Text),
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
      "SEQ"             -> Seq            <$> v .: "members"
      "CHOICE"          -> Choice         <$> v .: "members"
      "REPEAT"          -> Repeat         <$> v .: "content"
      "REPEAT1"         -> Repeat1        <$> v .: "content"
      "SYMBOL"          -> Symbol         <$> v .: "name"
      "STRING"          -> StringLiteral  <$> v .: "value"
      "PATTERN"         -> Pattern        <$> v .: "value"
      "BLANK"           -> return Blank
      "FIELD"           -> Field          <$> v .: "name" <*> v .: "content"
      "ALIAS"           -> Alias          <$> v .: "content" <*> v .: "named" <*> v .: "value"
      "TOKEN"           -> Token          <$> v .: "content"
      "IMMEDIATE_TOKEN" -> ImmediateToken <$> v .: "content"
      "PREC"            -> Prec           <$> v .: "value" <*> v .: "content"
      "PREC_LEFT"       -> PrecLeft       <$> v .: "value" <*> v .: "content"
      "PREC_RIGHT"      -> PrecRight      <$> v .: "value" <*> v .: "content"
      "PREC_DYNAMIC"    -> PrecDynamic    <$> v .: "value" <*> v .: "content"
      "RESERVED"        -> Reserved       <$> v .: "content" <*> v .: "context_name"
      unknown           -> fail $ "Unknown node type: " ++ unknown
  parseJSON invalid =
    prependFailure
      "Parsing Node failed"
      (typeMismatch "Object" invalid)

-- | A map from node name to node definition.
type Nodes = Map String Node

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
    name       <- v .: "name"
    word       <- v .:? "word"
    nodes      <- v .: "rules"
    externals  <- v .:? "externals"
    reserved   <- v .:? "reserved"
    inline     <- fmap (fmap (map Symbol)) (v .:? "inline")
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
    Nothing      -> MaybeT $ return Nothing
    Just grammar -> return grammar

parseNodeFromJSON :: String -> Maybe Node
parseNodeFromJSON jsonStr = decode $ BS.pack jsonStr

parseNodeFromFile :: String -> MaybeT IO Node
parseNodeFromFile path = do
  content <- lift $ readFile path
  case decode (BS.pack content) of
    Nothing   -> MaybeT $ return Nothing
    Just node -> return node
