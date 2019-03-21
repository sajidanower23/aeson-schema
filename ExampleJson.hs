{-# LANGUAGE OverloadedStrings #-}

module ExampleJson where

import           Data.Aeson.Schema.Choice
import           Data.Aeson.Schema.Types  as AT
import           Data.Aeson.Types
import           Data.HashMap.Lazy        as H
import           Data.Map.Internal        as M
import           Data.Ratio
import           Data.Text                as T
import           GHC.Base
import           GHC.Classes
import           GHC.Maybe
import           GHC.Show
import           Text.Regex
import           Text.Regex.PCRE.String

graph :: AT.Graph AT.Schema Text
graph = M.fromList [("parsedSchema",
                      AT.empty{
                        schemaType = [Choice1of2 ObjectType],
                        schemaProperties = H.fromList [("firstName",
                          AT.empty{
                            schemaType = [Choice1of2 StringType],
                            schemaDescription = Just ("The person's first name.")
                          })
                        ],
                        schemaTitle = Just "Person",
                        schemaDRef = Just "parsedSchema",
                        schemaDSchema = Just "http://json-schema.org/draft-07/schema#"
                      })
                    ]

newtype ParsedSchema
  = ParsedSchema ParsedSchema
    deriving (Eq, Show)

instance FromJSON ParsedSchema
    where parseJSON = fmap ParsedSchema . parseJSON

instance ToJSON ParsedSchema
    where toJSON (ParsedSchema val) = toJSON val

