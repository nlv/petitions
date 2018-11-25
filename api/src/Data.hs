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
  -- Petition(..)
  Petition,
  PetitionT(..)
  )  where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant
import Database.Beam

data PetitionT f
  = Petition {
    _petitionId                :: Columnar f Int,
    _petitionName              :: Columnar f Text,
    _petitionShortDescription  :: Columnar f Text,
    _petitionDescription       :: Columnar f Text,
    _petitionLocale            :: Columnar f Text
  }
  deriving Generic

type Petition = PetitionT Identity
type PetitionId = PrimaryKey PetitionT Identity

deriving instance Show Petition
deriving instance Eq Petition

type Api = PetitionApi -- :<|> SingerApi

type PetitionApi = 
  "petition" :> Capture "code" Text :> QueryParam "locale" Text :> Get '[JSON] Petition

-- data Petition
--   = Petition {
--     petitionId                :: Int,
--     petitionName              :: Text,
--     petitionShortDescription  :: Text,
--     petitionDescription       :: Text,
--     petitionLocale            :: Text
--   }
--   deriving (Eq, Show, Generic)

instance ToJSON (PetitionT Identity)
instance FromJSON (PetitionT Identity)


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

