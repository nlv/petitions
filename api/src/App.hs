{-# LANGUAGE OverloadedStrings #-}

module App where

import Database.Beam
import Control.Monad.Trans.Except
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.IO
import Data
import Api
import qualified Base as B
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg

api :: Proxy Api
api = Proxy

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ simpleCors (serve api server)

server :: Server Api
server =
  getPetitionByCode

getPetitionByCode :: Text -> Maybe Text -> Handler Petition
getPetitionByCode code locale = do
  p' <- liftIO $ do
    conn <- liftIO $ Pg.connectPostgreSQL "dbname=petitions" 
    -- runBeamPostgresDebug putStrLn conn (B.getPetitionByCode code locale)
    runBeamPostgres conn (B.getPetitionByCode code locale)
  case p' of
    Just p -> pure p
    _      -> throwE err404

