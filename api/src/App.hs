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
  getPetitionById 

getPetitionById :: Text -> Maybe Text -> Handler Petition
getPetitionById i l = case (i, l) of
  ("zerro", Just "ru") -> return example0Ru
  ("zerro", Just "en") -> return example0En
  ("one",   Nothing)   -> return example1
  _ -> throwE err404

example0Ru :: Petition
example0Ru = Petition 0 "zerro" "Ноль" "Длииииииииинно" "ru" 

example0En :: Petition
example0En = Petition 0 "zerro" "Zerro" "Veeeeeeeryyyyyy Looooooong" "en" 

example1 :: Petition
example1 = Petition 1 "one" "One" "One petition of the first petition" "en"
