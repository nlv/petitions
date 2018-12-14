{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Config (
  ConfigApp(..)
, ConfigTls(..)
, ConfigServer(..)
, loadConfig
)  
where

import Dhall

data ConfigApp = ConfigApp {
  tls       :: ConfigTls
, server    :: ConfigServer
, dbconnect :: String
} deriving (Generic, Show)
instance Interpret ConfigApp

data ConfigTls = ConfigTls {
  crtFile :: String
, keyFile :: String
} deriving (Generic, Show)
instance Interpret ConfigTls

data ConfigServer = ConfigServer {
  url  :: String
, port :: Natural
} deriving (Generic, Show)
instance Interpret ConfigServer

loadConfig :: Text -> IO (ConfigApp)
loadConfig = input auto