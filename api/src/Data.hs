{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data (
  Petition,
  PetitionT(..)
  )  where

import Data.Aeson
import Data.Text
import GHC.Generics
import Database.Beam

-- ** Petition

data PetitionT f
  = Petition {
    _petitionId                :: Columnar f Int,
    _petitionCode              :: Columnar f Text,
    _petitionName              :: Columnar f Text,
    _petitionShortDescription  :: Columnar f Text,
    _petitionDescription       :: Columnar f Text,
    _petitionLocale            :: Columnar f Text
  }
  deriving Generic

type Petition = PetitionT Identity

deriving instance Show Petition
deriving instance Eq Petition

instance ToJSON (PetitionT Identity)
instance FromJSON (PetitionT Identity)

-- ** PetitionLocale

-- CREATE TABLE petitions_locale (
--      id          SERIAL                PRIMARY KEY
--     ,petition_id INTEGER      NOT NULL REFERENCES petitions (id)
--     ,locale      VARCHAR(10)  NOT NULL REFERENCES locales (code)
--     ,name        VARCHAR(255) NOT NULL 
--     ,description TEXT         NOT NULL
--     ,insdate     TIMESTAMP    NOT NULL DEFAULT NOW()
--     ,UNIQUE (petition_id, locale)
-- );

-- data PetitionLocaleT f
--   = PetitionLocale {
--     _petitionLocaleId                :: Columnar f Int,
--     _petitionLocalePetitionId        :: Columnar f Int,
--     _petitionLocaleLocale            :: Columnar f Text
--     _petitionLocaleName              :: Columnar f Text,
--     _petitionLocaleShortDescription  :: Columnar f Text,
--     _petitionLocaleDescription       :: Columnar f Text,
--   }
--   deriving Generic

-- type Petition = PetitionT Identity

-- deriving instance Show Petition
-- deriving instance Eq Petition

-- instance ToJSON (PetitionT Identity)
-- instance FromJSON (PetitionT Identity)

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
