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

deriving instance Show Petition
deriving instance Eq Petition

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
