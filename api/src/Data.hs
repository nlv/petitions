{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data (
  Api,
  Petition(..)
  )  where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant

type Api = PetitionApi -- :<|> SingerApi

type PetitionApi = 
  "petition" :> Capture "id" Integer :> QueryParam "locale" Text :> Get '[JSON] Petition

data Petition
  = Petition {
    petitionId                :: Integer,
    petitionName              :: Text,
    petitionShortDescription  :: Text,
    petitionDescription       :: Text,
    petitionLocale            :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Petition
instance FromJSON Petition

{-
data Signer
  = Signer {
    petitionFirstName :: String,
    petitionLastName  :: String,
    petitionCountry :: String,
    petitionOrganization :: Maybe Integer,
    petitionEmail :: Integer,
    petitionPhone :: Integer,
    petitionBirthYear :: Integer,
    petitionGender :: Integer,
    petitionNotifiesEnabled :: Integer,
    petition :: Integer,
    petition :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Signer
instance FromJSON Signer
-}

