{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.IO
import Data
import Api
import qualified Base as B
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

getPetitionByCode :: Text -> Maybe Text -> Handler PetitionExt
getPetitionByCode code locale = do
  conn <- liftIO $ Pg.connectPostgreSQL "dbname=petitions" 
  p' <- liftIO $ B.getPetitionByCode conn code locale
  case p' of
    Just (Petition (PetitionId a) b c d e) -> pure $ (Petition a b c d e)
    _      -> throwE err404

