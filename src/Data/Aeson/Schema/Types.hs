{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Data.Aeson.Schema.Types
  ( Pattern (..)
  , mkPattern
  , SchemaType (..)
  , Schema (..)
  , Graph
  , empty
  , schemaQQ
  ) where

import           Control.Arrow                    (second)
import           Control.Monad                    (liftM)
import           Data.Aeson                       (FromJSON (..), Value (..),
                                                   (.!=), (.:?))
import           Data.Aeson.Parser                (value')
import           Data.Aeson.Schema.Choice
import           Data.Aeson.TH.Lift               ()
import           Data.Aeson.Types                 (Object, Parser, emptyArray,
                                                   emptyObject, parseEither)
import           Data.Attoparsec.ByteString.Char8 (skipSpace)
import           Data.Attoparsec.Lazy             (Result (..), parse)
import           Data.ByteString.Lazy.Char8       (pack)
import           Data.Foldable                    (Foldable (..), toList)
import           Data.Function                    (on)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as H
import qualified Data.Map                         as M
import           Data.Maybe                       (catMaybes)
import           Data.Scientific                  (Scientific)
import           Data.Text                        (Text, unpack)
import qualified Data.Vector                      as V
import           Language.Haskell.TH              (recUpdE, varE)
import           Language.Haskell.TH.Quote        (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax       (Lift (..))
import           Prelude                          hiding (foldr, length)
import           Text.Regex.PCRE                  (makeRegexM)
import           Text.Regex.PCRE.String           (Regex)

-- | Compiled regex and its source
data Pattern = Pattern { patternSource :: Text, patternCompiled :: Regex }

instance Eq Pattern where
  (==) = (==) `on` patternSource

instance Show Pattern where
  show pattern = "let Right p = mkPattern (" ++ show (patternSource pattern) ++ ") in p"

instance FromJSON Pattern where
  parseJSON (String s) = mkPattern s
  parseJSON _          = fail "only strings can be parsed as patterns"

instance Lift Pattern where
  lift (Pattern src _) = [| let Right p = mkPattern src in p |]

type Limit = (Scientific, Bool)

parseJSONLimit :: Text
               -> Object
               -> Parser (Maybe Limit)
parseJSONLimit d o = do
  r <- o .:? ("exclusive" `mappend` d)
  case r of
    Nothing -> do
      r <- o .:? d
      case r of
        Nothing -> pure Nothing
        Just n  -> pure . Just $ (n, False)
    Just n -> pure . Just $ (n, True)

-- | Compile a regex to a pattern, reporting errors with fail
mkPattern :: (Monad m) => Text -> m Pattern
mkPattern t = liftM (Pattern t) $ makeRegexM (unpack t)

-- | Primitive JSON types
data SchemaType = StringType
                | NumberType
                | IntegerType
                | BooleanType
                | ObjectType
                | ArrayType
                | NullType
                | AnyType -- ^ any of the above
                deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance FromJSON SchemaType where
  parseJSON (String t) = case t of
    "string"  -> return StringType
    "number"  -> return NumberType
    "integer" -> return IntegerType
    "boolean" -> return BooleanType
    "object"  -> return ObjectType
    "array"   -> return ArrayType
    "null"    -> return NullType
    "any"     -> return AnyType
    _         -> fail $ "not a valid type: " ++ unpack t
  parseJSON _ = fail "not a string"

instance Lift SchemaType where
  lift StringType  = [| StringType |]
  lift NumberType  = [| NumberType |]
  lift IntegerType = [| IntegerType |]
  lift BooleanType = [| BooleanType |]
  lift ObjectType  = [| ObjectType |]
  lift ArrayType   = [| ArrayType |]
  lift NullType    = [| NullType |]
  lift AnyType     = [| AnyType |]

-- | JSON Schema (Draft 7) Core Schema Definition
data Schema ref = Schema
  { schemaType                 :: [Choice2 SchemaType (Schema ref)]          -- ^ List of allowed schema types
  , schemaProperties           :: HashMap Text (Schema ref)                  -- ^ Subschemas for properties
  , schemaPatternProperties    :: [(Pattern, Schema ref)]                    -- ^ All properties that match one of the regexes must validate against the associated schema
  , schemaAdditionalProperties :: Choice2 Bool (Schema ref)                  -- ^ Whether additional properties are allowed when the instance is an object, and if so, a schema that they have to validate against
  , schemaItems                :: Maybe (Choice2 (Schema ref) [Schema ref])  -- ^ Either a schema for all array items or a different schema for each position in the array
  , schemaAdditionalItems      :: Choice2 Bool (Schema ref)                  -- ^ Whether additional items are allowed
  , schemaRequired             :: [Text]                                     -- ^ An object instance is valid against this keyword if every item in the array is the name of a property in the instance
  , schemaDependencies         :: HashMap Text (Choice2 [Text] (Schema ref)) -- ^ Map of dependencies (property a requires properties b and c, property a requires the instance to validate against another schema, etc.)
  , schemaMinimum              :: Maybe Limit                                -- ^ Minimum value when the instance is a number, whether or not it's exclusive
  , schemaMaximum              :: Maybe Limit                                -- ^ Maximum value when the instance is a number, whether or not it's exclusive
  , schemaMinItems             :: Int                                        -- ^ Minimum length for arrays
  , schemaMaxItems             :: Maybe Int                                  -- ^ Maximum length for arrays
  , schemaUniqueItems          :: Bool                                       -- ^ Whether all array items must be distinct from each other
  , schemaPattern              :: Maybe Pattern                              -- ^ Regex for validating strings
  , schemaMinLength            :: Int                                        -- ^ Minimum length for strings
  , schemaMaxLength            :: Maybe Int                                  -- ^ Maximum length for strings
  , schemaEnum                 :: Maybe [Value]                              -- ^ Allowed values for this schema
  , schemaEnumDescriptions     :: Maybe [Text]                               -- ^ Extension by Google: description for the values in schemaEnum
  , schemaDefault              :: Maybe Value                                -- ^ Default value if this schema is used in a property of another schema and the value is undefined
  , schemaTitle                :: Maybe Text                                 -- ^ Short description of the instance property
  , schemaDescription          :: Maybe Text                                 -- ^ Full description of the purpose of the instance property
  , schemaFormat               :: Maybe Text                                 -- ^ Format of strings, e.g. 'data-time', 'regex' or 'email'
  , schemaMultipleOf           :: Maybe Scientific                           -- ^ When the instance is a number, it must be multiple of this number
  , schemaDisallow             :: [Choice2 SchemaType (Schema ref)]          -- ^ List of disallowed types
  , schemaExtends              :: [Schema ref]                               -- ^ Base schema that the current schema inherits from
  , schemaId                   :: Maybe Text                                 -- ^ Identifier of the current schema
  , schemaDRef                 :: Maybe ref                                  -- ^ $ref: reference to another schema
  , schemaDSchema              :: Maybe Text                                 -- ^ $schema: URI of a schema that defines the format of the current schema
  } deriving (Eq, Show)

-- | Set of potentially mutually recursive schemas
type Graph f ref = M.Map ref (f ref)

instance Functor Schema where
  fmap f s = s
    { schemaType = mapChoice2 id (fmap f) <$> schemaType s
    , schemaProperties = fmap f <$> schemaProperties s
    , schemaPatternProperties = second (fmap f) <$> schemaPatternProperties s
    , schemaAdditionalProperties = mapChoice2 id (fmap f) (schemaAdditionalProperties s)
    , schemaItems = mapChoice2 (fmap f) (fmap $ fmap f) <$> schemaItems s
    , schemaAdditionalItems = mapChoice2 id (fmap f) (schemaAdditionalItems s)
    , schemaDependencies = mapChoice2 id (fmap f) <$> schemaDependencies s
    , schemaDisallow = mapChoice2 id (fmap f) <$> schemaDisallow s
    , schemaExtends = fmap f <$> schemaExtends s
    , schemaDRef = f <$> schemaDRef s
    }

instance Foldable Schema where
  foldr f start s = ffoldr (ffoldr f) (choice2of2s $ schemaType s)
                  . ffoldr (ffoldr f) (schemaProperties s)
                  . ffoldr (ffoldr f) (map snd $ schemaPatternProperties s)
                  . foldChoice2of2 (ffoldr f) (schemaAdditionalProperties s)
                  . ffoldr (\items -> foldChoice1of2 (ffoldr f) items . foldChoice2of2 (ffoldr $ ffoldr f) items) (schemaItems s)
                  . foldChoice2of2 (ffoldr f) (schemaAdditionalItems s)
                  . ffoldr (ffoldr f) (choice2of2s $ toList $ schemaDependencies s)
                  . ffoldr (ffoldr f) (choice2of2s $ schemaDisallow s)
                  . ffoldr (ffoldr f) (schemaExtends s)
                  . ffoldr f (schemaDRef s)
                  $ start
    where
      ffoldr :: (Foldable t) => (a -> b -> b) -> t a -> b -> b
      ffoldr g = flip $ foldr g
      foldChoice1of2 :: (a -> b -> b) -> Choice2 a x -> b -> b
      foldChoice1of2 g (Choice1of2 c) = g c
      foldChoice1of2 _ _              = id
      foldChoice2of2 :: (a -> b -> b) -> Choice2 x a -> b -> b
      foldChoice2of2 g (Choice2of2 c) = g c
      foldChoice2of2 _ _              = id

instance FromJSON ref => FromJSON (Schema ref) where
  parseJSON (Object o) = Schema
    <$> (parseSingleOrArray =<< parseFieldDefault "type" "any")
    <*> parseFieldDefault "properties" emptyObject
    <*> (parseFieldDefault "patternProperties" emptyObject >>= mapM (\(k, v) -> fmap (,v) (mkPattern k)) . H.toList)
    <*> (parseField "additionalProperties" .!= Choice1of2 True)
    <*> parseField "items"
    <*> (parseField "additionalItems" .!= Choice1of2 True)
    <*> parseFieldDefault "required" emptyArray
    <*> (traverse parseDependency =<< parseFieldDefault "dependencies" emptyObject)
    <*> parseJSONLimit "Minimum" o
    <*> parseJSONLimit "Maximum" o
    <*> parseFieldDefault "minItems" (Number 0)
    <*> parseField "maxItems"
    <*> parseFieldDefault "uniqueItems" (Bool False)
    <*> parseField "pattern"
    <*> parseFieldDefault "minLength" (Number 0)
    <*> parseField "maxLength"
    <*> parseField "enum"
    <*> parseField "enumDescriptions"
    <*> parseField "default"
    <*> parseField "title"
    <*> parseField "description"
    <*> parseField "format"
    <*> parseField "multipleOf"
    <*> (parseSingleOrArray =<< parseFieldDefault "disallow" emptyArray)
    <*> ((maybe (return Nothing) (fmap Just . parseSingleOrArray) =<< parseField "extends") .!= [])
    <*> parseField "id"
    <*> parseField "$ref"
    <*> parseField "$schema"
    where
      parseField :: (FromJSON a) => Text -> Parser (Maybe a)
      parseField name = o .:? name

      parseFieldDefault :: (FromJSON a) => Text -> Value -> Parser a
      parseFieldDefault name value = parseJSON =<< parseField name .!= value

      singleOrArray :: (Value -> Parser a) -> Value -> Parser [a]
      singleOrArray p (Array a) = mapM p (V.toList a)
      singleOrArray p v         = (:[]) <$> p v

      parseSingleOrArray :: (FromJSON a) => Value -> Parser [a]
      parseSingleOrArray = singleOrArray parseJSON

      parseDependency :: FromJSON ref => Value -> Parser (Choice2 [Text] (Schema ref))
      parseDependency (String s) = return $ Choice1of2 [s]
      parseDependency val        = parseJSON val
  parseJSON (Bool _) = pure empty -- Boolean schemas not supported yet
  parseJSON _ = fail $ "a schema must be a JSON Object or a Bool"

instance (Eq ref, Lift ref) => Lift (Schema ref) where
  lift schema = case updates of
    [] -> varE 'empty
    _  -> recUpdE (varE 'empty) updates
    where
      updates = catMaybes
        [ field 'schemaType schemaType
        , field 'schemaProperties schemaProperties
        , field 'schemaPatternProperties schemaPatternProperties
        , field 'schemaAdditionalProperties schemaAdditionalProperties
        , field 'schemaItems schemaItems
        , field 'schemaAdditionalItems schemaAdditionalItems
        , field 'schemaRequired schemaRequired
        , field 'schemaDependencies schemaDependencies
        , field 'schemaMinimum schemaMinimum
        , field 'schemaMaximum schemaMaximum
        , field 'schemaMinItems schemaMinItems
        , field 'schemaMaxItems schemaMaxItems
        , field 'schemaUniqueItems schemaUniqueItems
        , field 'schemaPattern schemaPattern
        , field 'schemaMinLength schemaMinLength
        , field 'schemaMaxLength schemaMaxLength
        , field 'schemaEnum schemaEnum
        , field 'schemaEnumDescriptions schemaEnumDescriptions
        , field 'schemaDefault schemaDefault
        , field 'schemaTitle schemaTitle
        , field 'schemaDescription schemaDescription
        , field 'schemaFormat schemaFormat
        , field 'schemaMultipleOf schemaMultipleOf
        , field 'schemaDisallow schemaDisallow
        , field 'schemaExtends schemaExtends
        , field 'schemaId schemaId
        , fmap ('schemaDRef,) . lift . Just <$> schemaDRef schema
        , field 'schemaDSchema schemaDSchema
        ]
      field name accessor = if accessor schema == accessor empty
        then Nothing
        else Just $ (name,) <$> lift (accessor schema)

-- | The empty schema accepts any JSON value.
empty :: Schema ref
empty = Schema
  { schemaType = [Choice1of2 AnyType]
  , schemaProperties = H.empty
  , schemaPatternProperties = []
  , schemaAdditionalProperties = Choice1of2 True
  , schemaItems = Nothing
  , schemaAdditionalItems = Choice1of2 True
  , schemaRequired = []
  , schemaDependencies = H.empty
  , schemaMinimum = Nothing
  , schemaMaximum = Nothing
  , schemaMinItems = 0
  , schemaMaxItems = Nothing
  , schemaUniqueItems = False
  , schemaPattern = Nothing
  , schemaMinLength = 0
  , schemaMaxLength = Nothing
  , schemaEnum = Nothing
  , schemaEnumDescriptions = Nothing
  , schemaDefault = Nothing
  , schemaTitle = Nothing
  , schemaDescription = Nothing
  , schemaFormat = Nothing
  , schemaMultipleOf = Nothing
  , schemaDisallow = []
  , schemaExtends = []
  , schemaId = Nothing
  , schemaDRef = Nothing
  , schemaDSchema = Nothing
  }

-- | QuasiQuoter for schemas in JSON (e.g. @[schemaQQ| { \"type\": \"number\", \"minimum\": 0 } |]@)
schemaQQ :: QuasiQuoter
schemaQQ = QuasiQuoter { quoteExp = quote }
  where
    quote jsonStr = case parse (skipSpace *> value' <* skipSpace) (pack jsonStr) of
      Done _ json -> case parseEither parseJSON json :: Either String (Schema Text) of
        Left e  -> fail e
        Right s -> lift s
      _ -> fail "not a valid JSON value"
