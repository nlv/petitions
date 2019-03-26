{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Text
import Data.Text.Lazy (fromStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import Network.Wai.Handler.WarpTLS
import Servant
import System.IO
import Data
import Api
import qualified Config as Cfg
import qualified Base as B
import qualified Database.PostgreSQL.Simple as Pg
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import qualified Text.Blaze.Html5.Attributes   as A
import Text.Markdown

api :: Proxy Api
api = Proxy

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy _ = Just $ simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type"]
  , corsMethods = HTTP.methodOptions : HTTP.methodPut : corsMethods simpleCorsResourcePolicy 
  }

run :: IO ()
run = do
  opts <- Cfg.getOpts
  config <- Cfg.loadConfig (Cfg.configFile opts) 
  let serverCfg = Cfg.server config
  let url = pack $ Cfg.url serverCfg
  let port = fromIntegral (Cfg.port serverCfg)
  let crtFile = Cfg.crtFile $ Cfg.tls config
  let keyFile = Cfg.keyFile $ Cfg.tls config
  let dbconnect = BS.pack $ Cfg.dbconnect config
      warpOpts =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
      tlsOpts = tlsSettings crtFile keyFile
  runTLS tlsOpts warpOpts =<< mkApp url port dbconnect
  -- runServer warpOpts =<< mkApp

mkApp :: Text -> Int -> ByteString -> IO Application
-- mkApp = return $ simpleCors (serve api server)
mkApp url port dbconnect = 
  return $ (cors corsPolicy) $ logStdoutDev $ static $ (serve api $ server url port dbconnect)

server :: Text -> Int -> ByteString -> Server Api
server url port db = ((getPetition db) :<|> postSigner db) 
       :<|> (getPetitionWidget url port)
       :<|> (getPetitionFullText db)

getPetition :: ByteString -> Text -> Maybe Text -> Handler (Petition, Int)
getPetition dbconnect code locale = do
  conn <- liftIO $ Pg.connectPostgreSQL dbconnect
  p' <- liftIO $ B.getPetitionByCode conn code locale
  case p' of
    Just p -> do
      cnt <- liftIO $ B.getSignersCount conn (_petitionId p)
      pure (p, cnt)
    -- Just (Petition (PetitionId a) b c d e) -> pure $ (Petition a b c d e)
    _      -> throwE err404

getPetitionWidget :: Text -> Int -> Text -> Maybe Text -> Maybe Text -> Handler H.Html
getPetitionWidget url port code locale widget = do
  let widget' = maybe ("default"::Text) id widget
      locale' = maybe ("default"::Text) id locale
      url' = url `append` ":" `append` (pack $ show port)
      widgetDir = url' `append` "/static/widgets/" `append` widget' `append` "/"
      widgetFile = (widgetDir `append`)
  pure $ H.docTypeHtml H.! A.lang (H.textValue $ maybe "en" id locale) $ do
    H.head $ do
      H.meta H.! A.charset "UTF-8"
      H.title "Petition"
      H.script H.! A.type_ "text/javascript" H.! A.src (H.textValue $ widgetFile "petition-widget.js") $ mempty
      H.link H.! A.rel "stylesheet" H.! A.href (H.textValue $ url' `append` "/static/reset.css") 
      H.link H.! A.rel "stylesheet" H.! A.href (H.textValue $ widgetFile "petition.css") 
      H.body $ do
        H.div H.! A.id "petition" $ mempty
        H.script $ do
          H.toHtml ("window.onload = function () { \
                  \    setTimeout (function() { var app = Elm.Main.init({ \
                  \      node: document.getElementById('petition'), \
                  \      flags: {url: '" `append` url' `append` "', code: '" `append` code `append` "', locale: '" `append` locale' `append` ("'} \
                  \                  }); \
                  \    }, 100); \
                  \ };" :: Text)) 

getPetitionFullText :: ByteString -> Text -> Maybe Text -> Handler H.Html
getPetitionFullText dbconnect code locale = do
  conn <- liftIO $ Pg.connectPostgreSQL dbconnect
  p' <- liftIO $ B.getPetitionByCode conn code locale
  case p' of
    Just p -> do
      pure $ H.docTypeHtml $ do
        H.head $ do
          H.meta H.! A.charset "UTF-8"
          H.link H.! A.rel "stylesheet" H.! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" 
          H.script H.! A.type_ "text/javascript" H.! A.src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" $ mempty
          H.title $ H.text (_petitionName p)
          H.body $ do
            H.div H.! A.class_ "container" $ do
              H.h1 $ H.text (_petitionName p)
              markdown def (fromStrict $ _petitionContent p)
    _      -> throwE err404

postSigner :: ByteString -> Text -> SignerForm -> Handler Int
postSigner dbconnect code signerForm = do
  conn <- liftIO $ Pg.connectPostgreSQL dbconnect
  inserted <- liftIO $ B.insertSigner conn code signerForm
  case inserted of 
    Just cnt -> pure cnt
    Nothing -> throwE err404

