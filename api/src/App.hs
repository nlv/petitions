{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Text
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
import qualified Base as B
import qualified Database.PostgreSQL.Simple as Pg
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import qualified Text.Blaze.Html5.Attributes   as A

api :: Proxy Api
api = Proxy

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy _ = Just $ simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type"]
  , corsMethods = HTTP.methodOptions : HTTP.methodPut : corsMethods simpleCorsResourcePolicy 
  }

run :: IO ()
run = do
  let port = 8080
  -- let port = 80
      warpOpts =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
      -- tlsOpts = tlsSettings "/etc/ssl/certs/ssl-cert-snakeoil.pem" "/etc/ssl/private/ssl-cert-snakeoil.key"
      tlsOpts = tlsSettings "crt.txt" "private-key.txt"
  runTLS tlsOpts warpOpts =<< mkApp
  -- runServer warpOpts =<< mkApp

mkApp :: IO Application
-- mkApp = return $ simpleCors (serve api server)
mkApp = return $ (cors corsPolicy) $ logStdoutDev $ static $ (serve api server)

server :: Server Api
server = (getPetitionByCode :<|> postSigner) :<|> getHtmlPetitionByCode 


getPetitionByCode :: Text -> Maybe Text -> Handler Petition
getPetitionByCode code locale = do
  conn <- liftIO $ Pg.connectPostgreSQL "dbname=petitions user=nlv" 
  p' <- liftIO $ B.getPetitionByCode conn code locale
  case p' of
    Just p -> pure p
    -- Just (Petition (PetitionId a) b c d e) -> pure $ (Petition a b c d e)
    _      -> throwE err404

getHtmlPetitionByCode :: Text -> Maybe Text -> Handler H.Html
getHtmlPetitionByCode code locale = do
  let locale' = maybe ("default"::Text) id locale
  pure $ H.docTypeHtml $ do
    H.head $ do
      H.meta H.! A.charset "UTF-8"
      H.title "Petition"
      H.script H.! A.type_ "text/javascript" H.! A.src "https://petitions.nika.news:8080/static/petition-widget.js" $ mempty
      H.link H.! A.rel "stylesheet" H.! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" 
      H.body $ do
        H.div H.! A.id "petition" $ mempty
        H.script $ do
          H.toHtml ("var app = Elm.Main.init({ \
                  \  node: document.getElementById('petition'), \
                  \  flags: {url: 'https://petitions.nika.news:8080', code: '" `append` code `append` "', locale: '" `append` locale' `append` ("'} \
                  \              });" :: Text))
      H.script H.! A.type_ "text/javascript" H.! A.src "https://code.jquery.com/jquery-3.3.1.slim.min.js" $ mempty
      H.script H.! A.type_ "text/javascript" H.! A.src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" $ mempty
      H.script H.! A.type_ "text/javascript" H.! A.src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" $ mempty

postSigner :: Text -> SignerForm -> Handler ()
postSigner code signerForm = do
  conn <- liftIO $ Pg.connectPostgreSQL "dbname=petitions user=nlv" 
  inserted <- liftIO $ B.insertSigner conn code signerForm
  case inserted of 
    True -> pure ()
    False -> throwE err404

