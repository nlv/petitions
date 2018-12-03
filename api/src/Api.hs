{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api
  )  where

import Data.Text
import Servant
import Data

type Api = PetitionApi -- :<|> SingerApi

type PetitionApi = 
       "petition" :> Capture "code" Text :> QueryParam "locale" Text :> Get '[JSON] Petition 
  :<|> "petition" :> Capture "code" Text :> "signer" :> ReqBody '[JSON] SignerForm :> Post '[JSON] ()


