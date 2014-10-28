{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}

module Config where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Data

import Util

data Config = Config
    { configName :: String
    , configScripts :: [FilePath]
    } deriving (Show, Read, Eq, Ord, Data, Typeable)
deriveJSON defaultOptions{fieldLabelModifier=dataFieldToKeyName "config"} ''Config

parseConfig :: String -> Either String [Config]
parseConfig = eitherDecode . B.pack
