{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api,
  RestApi
  )  where

import Data.Text
import Servant
import Data

import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H

type Api = RestApi :<|> HtmlApi

type RestApi = 
       "petition" :> Capture "code" Text :> QueryParam "locale" Text :> Get '[JSON] (Petition, Int)
  :<|> "petition" :> Capture "code" Text :> "signer" :> ReqBody '[JSON] SignerForm :> Post '[JSON] Int

type HtmlApi = 
       "petition.html" :> Capture "code" Text 
       :> QueryParam "locale" Text :> QueryParam "widget" Text :> Get '[HTML] H.Html
  :<|> "petitionText.html" :> Capture "code" Text :> QueryParam "locale" Text :> Get '[HTML] H.Html 



