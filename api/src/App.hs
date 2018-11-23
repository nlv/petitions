{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Monad.Trans.Except
import           Data.Text
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Data

api :: Proxy Api
api = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve api server

server :: Server Api
server =
  getPetitionById 


getPetitionById :: Integer -> Maybe Text -> Handler Petition
getPetitionById i l = case (i, l) of
  (0, Just "ru") -> return example0Ru
  (0, Just "en") -> return example0En
  (1, Nothing)   -> return example1
  _ -> throwE err404

example0Ru :: Petition
example0Ru = Petition 0 "Ноль" "Очень кратко" "Длииииииииинно" "ru"

example0En :: Petition
example0En = Petition 0 "Zerro" "Very short" "Veeeeeeeryyyyyy Looooooong" "en"

example1 :: Petition
example1 = Petition 1 "One" "One petition" "One petition of the first petition" "en"
