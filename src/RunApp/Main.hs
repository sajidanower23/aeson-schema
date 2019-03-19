{-# LANGUAGE ScopedTypeVariables #-}

module RunApp.Main (run) where

import           Data.Aeson                 hiding (defaultOptions)
import           Data.Aeson.Schema

import           Data.Aeson.Schema.CodeGen
import           Data.Aeson.Schema.CodeGenM (defaultOptions)

import           Data.Text

import qualified Data.ByteString.Lazy       as L

import qualified Data.Map                   as M

import           Language.Haskell.TH

run :: IO ()
run = do
  schemaBS <- L.readFile "example.json"
  case decode schemaBS :: Maybe Value of
        Nothing -> error "JSON syntax error"
        Just val -> case fromJSON val :: Result (Schema Text) of
          Error e -> error $ show e
          Success schema' -> do
            let schema = schema' { schemaDRef = Just "parsedSchema"}
                graph = M.singleton "parsedSchema" schema
            runQ (generate graph defaultOptions) >>= \(code, m) -> do
              print m
              print code
            -- error "lol"

