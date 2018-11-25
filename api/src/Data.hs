{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data (
  Api,
  Petition(..)
  )  where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant
import Database.Beam

data Petition2T f
  = Petition2 {
    _petitionId                :: Columnar f Int,
    _petitionName              :: Columnar f Text,
    _petitionShortDescription  :: Columnar f Text,
    _petitionDescription       :: Columnar f Text,
    _petitionLocale            :: Columnar f Text
  }

type Petition2 = Petition2T Identity
type Petition2Id = PrimaryKey Petition2T Identity

deriving instance Show Petition2
deriving instance Eq Petition2

type Api = PetitionApi -- :<|> SingerApi

type PetitionApi = 
  "petition" :> Capture "code" Text :> QueryParam "locale" Text :> Get '[JSON] Petition

data Petition
  = Petition {
    petitionId                :: Int,
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

