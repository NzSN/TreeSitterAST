{-# LANGUAGE OverloadedStrings #-}

module TreeSitterGrammarNodes
  ( Node (..),
    PrecedenceValue (..),
    Nodes,
    Grammar (..),
    parseNodeFromJSON,
    parseGrammarFromJSON,
    parseGrammarFromFile,
    parseNodeFromFile,
    isLeaf,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map (Map)
import Data.Text (unpack)
import GHC.Generics

-- | Precedence value can be either a named precedence (string) or numeric precedence (int).
data PrecedenceValue
  = NamedPrecedence String
  | NumericPrecedence Int
  deriving (Show, Eq, Generic, Ord)

instance ToJSON PrecedenceValue where
  toJSON (NamedPrecedence s) = toJSON s
  toJSON (NumericPrecedence n) = toJSON n

instance FromJSON PrecedenceValue where
  parseJSON v = case v of
    String s -> return $ NamedPrecedence (unpack s)
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
  | Symbol         {name :: String}
  | StringLiteral  {value :: String}
  | Pattern        {value :: String}
  | Blank
  | Field          {fieldName :: String, content :: Node}
  | Alias          {content :: Node, named :: Bool, aliasValue :: String}
  | Token          {content :: Node}
  | ImmediateToken {content :: Node}
  | Prec           {precedence :: PrecedenceValue, content :: Node}
  | PrecLeft       {precedence :: PrecedenceValue, content :: Node}
  | PrecRight      {precedence :: PrecedenceValue, content :: Node}
  | PrecDynamic    {precedence :: PrecedenceValue, content :: Node}
  | Reserved       {content :: Node, contextName :: String}
  | Empty
  deriving (Show, Eq, Generic, Ord)

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
  toJSON (Seq members) =
    object
      [ "type" .= ("SEQ" :: String),
        "members" .= members
      ]
  toJSON (Choice members) =
    object
      [ "type" .= ("CHOICE" :: String),
        "members" .= members
      ]
  toJSON (Repeat content) =
    object
      [ "type" .= ("REPEAT" :: String),
        "content" .= content
      ]
  toJSON (Repeat1 content) =
    object
      [ "type" .= ("REPEAT1" :: String),
        "content" .= content
      ]
  toJSON (Symbol name) =
    object
      [ "type" .= ("SYMBOL" :: String),
        "name" .= name
      ]
  toJSON (StringLiteral value) =
    object
      [ "type" .= ("STRING" :: String),
        "value" .= value
      ]
  toJSON (Pattern value) =
    object
      [ "type" .= ("PATTERN" :: String),
        "value" .= value
      ]
  toJSON Blank =
    object
      [ "type" .= ("BLANK" :: String)
      ]
  toJSON (Field fieldName content) =
    object
      [ "type" .= ("FIELD" :: String),
        "name" .= fieldName,
        "content" .= content
      ]
  toJSON (Alias content named aliasValue) =
    object
      [ "type" .= ("ALIAS" :: String),
        "content" .= content,
        "named" .= named,
        "value" .= aliasValue
      ]
  toJSON (Token content) =
    object
      [ "type" .= ("TOKEN" :: String),
        "content" .= content
      ]
  toJSON (ImmediateToken content) =
    object
      [ "type" .= ("IMMEDIATE_TOKEN" :: String),
        "content" .= content
      ]
  toJSON (Prec precedence content) =
    object
      [ "type" .= ("PREC" :: String),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (PrecLeft precedence content) =
    object
      [ "type" .= ("PREC_LEFT" :: String),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (PrecRight precedence content) =
    object
      [ "type" .= ("PREC_RIGHT" :: String),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (PrecDynamic precedence content) =
    object
      [ "type" .= ("PREC_DYNAMIC" :: String),
        "value" .= precedence,
        "content" .= content
      ]
  toJSON (Reserved content contextName) =
    object
      [ "type" .= ("RESERVED" :: String),
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

-- | Full grammar definition.
data Grammar = Grammar
  { grammarName :: String,
    grammarWord :: Maybe String,
    grammarNodes :: Nodes
  }
  deriving (Show, Eq, Generic)

instance ToJSON Grammar where
  toJSON (Grammar name word nodes) =
    object $
      [ "name" .= name,
        "rules" .= nodes
      ]
        ++ maybe [] (\w -> ["word" .= w]) word

instance FromJSON Grammar where
  parseJSON (Object v) = do
    name <- v .: "name"
    word <- v .:? "word"
    nodes <- v .: "rules"
    return $ Grammar name word nodes
  parseJSON invalid =
    prependFailure
      "Parsing Grammar failed"
      (typeMismatch "Object" invalid)

-- | Parse a single node from a JSON string.
parseNodeFromJSON :: String -> Maybe Node
parseNodeFromJSON jsonStr = decode (BSL.pack jsonStr)

-- | Parse a full grammar from a JSON string.
parseGrammarFromJSON :: String -> Maybe Grammar
parseGrammarFromJSON jsonStr = decode (BSL.pack jsonStr)

-- | Parse a grammar from a file.
parseGrammarFromFile :: String -> MaybeT IO Grammar
parseGrammarFromFile path = do
  content <- lift $ readFile path
  case decode (BSL.pack content) of
    Nothing -> MaybeT $ return Nothing
    Just grammar -> return grammar

-- | Parse a single node from a file.
parseNodeFromFile :: String -> MaybeT IO Node
parseNodeFromFile path = do
  content <- lift $ readFile path
  case decode (BSL.pack content) of
    Nothing -> MaybeT $ return Nothing
    Just node -> return node
