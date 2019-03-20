

module ExampleJson where

import Data.Aeson.Schema.Choice
import Data.Aeson.Schema.Types
import Data.Aeson.Types
import Data.Aeson.Types.FromJSON
import Data.Aeson.Types.ToJSON
import Data.HashMap.Lazy
import Data.Map.Internal
import Data.Ratio
import Data.Text
import GHC.Base
import GHC.Classes
import GHC.Maybe
import GHC.Show
import Text.Regex
import Text.Regex.PCRE.String

graph :: Data.Aeson.Schema.Types.Graph Data.Aeson.Schema.Types.Schema
                                       Data.Text.Text

graph = Data.Map.Internal.fromList [(Data.Text.pack ['p',
                                                     'a',
                                                     'r',
                                                     's',
                                                     'e',
                                                     'd',
                                                     'S',
                                                     'c',
                                                     'h',
                                                     'e',
                                                     'm',
                                                     'a'],
                                     Data.Aeson.Schema.Types.empty{Data.Aeson.Schema.Types.schemaType = [Data.Aeson.Schema.Choice.Choice1of2 Data.Aeson.Schema.Types.ObjectType],
                                                                   Data.Aeson.Schema.Types.schemaProperties = Data.HashMap.Lazy.fromList [(Data.Text.pack ['f',
                                                                                                                                                           'i',
                                                                                                                                                           'r',
                                                                                                                                                           's',
                                                                                                                                                           't',
                                                                                                                                                           'N',
                                                                                                                                                           'a',
                                                                                                                                                           'm',
                                                                                                                                                           'e'],
                                                                                                                                           Data.Aeson.Schema.Types.empty{Data.Aeson.Schema.Types.schemaType = [Data.Aeson.Schema.Choice.Choice1of2 Data.Aeson.Schema.Types.StringType],
                                                                                                                                                                         Data.Aeson.Schema.Types.schemaDescription = GHC.Maybe.Just (Data.Text.pack ['T',
                                                                                                                                                                                                                                                     'h',
                                                                                                                                                                                                                                                     'e',
                                                                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                                                                     'p',
                                                                                                                                                                                                                                                     'e',
                                                                                                                                                                                                                                                     'r',
                                                                                                                                                                                                                                                     's',
                                                                                                                                                                                                                                                     'o',
                                                                                                                                                                                                                                                     'n',
                                                                                                                                                                                                                                                     '\'',
                                                                                                                                                                                                                                                     's',
                                                                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                                                                     'f',
                                                                                                                                                                                                                                                     'i',
                                                                                                                                                                                                                                                     'r',
                                                                                                                                                                                                                                                     's',
                                                                                                                                                                                                                                                     't',
                                                                                                                                                                                                                                                     ' ',
                                                                                                                                                                                                                                                     'n',
                                                                                                                                                                                                                                                     'a',
                                                                                                                                                                                                                                                     'm',
                                                                                                                                                                                                                                                     'e',
                                                                                                                                                                                                                                                     '.'])})],
                                                                   Data.Aeson.Schema.Types.schemaTitle = GHC.Maybe.Just (Data.Text.pack ['P',
                                                                                                                                         'e',
                                                                                                                                         'r',
                                                                                                                                         's',
                                                                                                                                         'o',
                                                                                                                                         'n']),
                                                                   Data.Aeson.Schema.Types.schemaDRef = GHC.Maybe.Just (Data.Text.pack ['p',
                                                                                                                                        'a',
                                                                                                                                        'r',
                                                                                                                                        's',
                                                                                                                                        'e',
                                                                                                                                        'd',
                                                                                                                                        'S',
                                                                                                                                        'c',
                                                                                                                                        'h',
                                                                                                                                        'e',
                                                                                                                                        'm',
                                                                                                                                        'a']),
                                                                   Data.Aeson.Schema.Types.schemaDSchema = GHC.Maybe.Just (Data.Text.pack ['h',
                                                                                                                                           't',
                                                                                                                                           't',
                                                                                                                                           'p',
                                                                                                                                           ':',
                                                                                                                                           '/',
                                                                                                                                           '/',
                                                                                                                                           'j',
                                                                                                                                           's',
                                                                                                                                           'o',
                                                                                                                                           'n',
                                                                                                                                           '-',
                                                                                                                                           's',
                                                                                                                                           'c',
                                                                                                                                           'h',
                                                                                                                                           'e',
                                                                                                                                           'm',
                                                                                                                                           'a',
                                                                                                                                           '.',
                                                                                                                                           'o',
                                                                                                                                           'r',
                                                                                                                                           'g',
                                                                                                                                           '/',
                                                                                                                                           'd',
                                                                                                                                           'r',
                                                                                                                                           'a',
                                                                                                                                           'f',
                                                                                                                                           't',
                                                                                                                                           '-',
                                                                                                                                           '0',
                                                                                                                                           '7',
                                                                                                                                           '/',
                                                                                                                                           's',
                                                                                                                                           'c',
                                                                                                                                           'h',
                                                                                                                                           'e',
                                                                                                                                           'm',
                                                                                                                                           'a',
                                                                                                                                           '#'])})]

newtype ParsedSchema
  = ParsedSchema ParsedSchema
    deriving (GHC.Classes.Eq, GHC.Show.Show)

instance Data.Aeson.Types.FromJSON.FromJSON ParsedSchema
    where parseJSON = GHC.Base.fmap ParsedSchema GHC.Base.. Data.Aeson.Types.FromJSON.parseJSON

instance Data.Aeson.Types.ToJSON.ToJSON ParsedSchema
    where toJSON (ParsedSchema val) = Data.Aeson.Types.ToJSON.toJSON val