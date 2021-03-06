import qualified Data.Text                         as T
import           Test.Framework

import qualified Data.Aeson.Schema.CodeGen.Tests
import qualified Data.Aeson.Schema.Types.Tests
import qualified Data.Aeson.Schema.Validator.Tests
import           TestSuite.Types                   (SchemaTest (..),
                                                    readSchemaTests)

allTests :: IO [SchemaTest]
allTests = do
  requiredTests <- readSchemaTests "test/test-suite/tests/draft7"
  optionalTests <- readSchemaTests "test/test-suite/tests/draft7/optional"
  formatTests <- readSchemaTests "test/test-suite/tests/draft7/optional/format"

  return $ requiredTests ++ optionalTests ++ formatTests


main :: IO ()
main = do
  schemaTests <- allTests
  defaultMain
    [ testGroup "Data.Aeson.Schema.Types" Data.Aeson.Schema.Types.Tests.tests
    , testGroup "Data.Aeson.Schema.Validator" $ Data.Aeson.Schema.Validator.Tests.tests schemaTests
    -- , buildTest $ testGroup "Data.Aeson.Schema.CodeGen" <$> Data.Aeson.Schema.CodeGen.Tests.tests schemaTests
    -- , testGroup "Data.Aeson.Schema.Choice" Data.Aeson.Schema.Choice.Tests.tests
    ]

-- runTest :: String -> IO ()
-- runTest tc = do
--     at <- allTests
--     let schemaTests = findTest at
--     if null schemaTests
--         then fail "test not found"
--         else
--             defaultMain
--               [
--                 testGroup "Data.Aeson.Schema.Validator" $ Data.Aeson.Schema.Validator.Tests.tests schemaTests
--                 , buildTest $ testGroup "Data.Aeson.Schema.CodeGen" <$> Data.Aeson.Schema.CodeGen.Tests.tests schemaTests
--               ]


--   where
--     name = T.pack tc
--     findTest = filter (\t -> schemaTestDescription t == name)
