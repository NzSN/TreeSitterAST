{-# LANGUAGE OverloadedStrings #-}

module TreeSitterGrammarNodes
  ( Rule (..),
    PrecedenceValue (..),
    Rules,
    Grammar (..),
    parseRuleFromJSON,
    parseGrammarFromJSON,
    parseGrammarFromFile,
    parseRuleFromFile,
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
  deriving (Show, Eq, Generic)

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

-- | A grammar rule.
data Rule
  = Seq {members :: [Rule]}
  | Choice {members :: [Rule]}
  | Repeat {content :: Rule}
  | Repeat1 {content :: Rule}
  | Symbol {name :: String}
  | StringLit {value :: String} -- renamed to avoid conflict with built-in String
  | Pattern {value :: String}
  | Blank
  | Field {fieldName :: String, content :: Rule}
  | Alias {content :: Rule, named :: Bool, aliasValue :: String}
  | Token {content :: Rule}
  | ImmediateToken {content :: Rule}
  | Prec {precedence :: PrecedenceValue, content :: Rule}
  | PrecLeft {precedence :: PrecedenceValue, content :: Rule}
  | PrecRight {precedence :: PrecedenceValue, content :: Rule}
  | PrecDynamic {precedence :: PrecedenceValue, content :: Rule}
  | Reserved {content :: Rule, contextName :: String}
  deriving (Show, Eq, Generic)

instance ToJSON Rule where
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
  toJSON (StringLit value) =
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

instance FromJSON Rule where
  parseJSON (Object v) = do
    typ <- v .: "type"
    case typ of
      "SEQ" -> Seq <$> v .: "members"
      "CHOICE" -> Choice <$> v .: "members"
      "REPEAT" -> Repeat <$> v .: "content"
      "REPEAT1" -> Repeat1 <$> v .: "content"
      "SYMBOL" -> Symbol <$> v .: "name"
      "STRING" -> StringLit <$> v .: "value"
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
      unknown -> fail $ "Unknown rule type: " ++ unknown
  parseJSON invalid =
    prependFailure
      "Parsing Rule failed"
      (typeMismatch "Object" invalid)

-- | A map from rule name to rule definition.
type Rules = Map String Rule

-- | Full grammar definition.
data Grammar = Grammar
  { grammarName :: String,
    grammarWord :: Maybe String,
    grammarRules :: Rules
  }
  deriving (Show, Eq, Generic)

instance ToJSON Grammar where
  toJSON (Grammar name word rules) =
    object $
      [ "name" .= name,
        "rules" .= rules
      ]
        ++ maybe [] (\w -> ["word" .= w]) word

instance FromJSON Grammar where
  parseJSON (Object v) = do
    name <- v .: "name"
    word <- v .:? "word"
    rules <- v .: "rules"
    return $ Grammar name word rules
  parseJSON invalid =
    prependFailure
      "Parsing Grammar failed"
      (typeMismatch "Object" invalid)

-- | Parse a single rule from a JSON string.
parseRuleFromJSON :: String -> Maybe Rule
parseRuleFromJSON jsonStr = decode (BSL.pack jsonStr)

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

-- | Parse a single rule from a file.
parseRuleFromFile :: String -> MaybeT IO Rule
parseRuleFromFile path = do
  content <- lift $ readFile path
  case decode (BSL.pack content) of
    Nothing -> MaybeT $ return Nothing
    Just rule -> return rule
