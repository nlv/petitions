{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data (
  Petition,
  PetitionT(..),
  PetitionId,
  PetitionLocale,
  PetitionLocaleT(..),
  PetitionLocaleId
  )  where

import GHC.Generics
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
    _petitionDescription       :: Columnar f Text,
    _petitionLocale            :: Columnar f Text
  }
  deriving Generic

type Petition = PetitionT Identity

deriving instance Show Petition
deriving instance Eq Petition

instance ToJSON (PetitionT Identity)
instance FromJSON (PetitionT Identity)

type PetitionId = PrimaryKey PetitionT Identity

-- instance Generic (PrimaryKey PetitionT Identity)
-- instance ToJSON (PrimaryKey PetitionT Identity)

data PetitionLocaleT f
  = PetitionLocale {
    _petitionLocaleId          :: Columnar f Int,
    _petitionLocaleCode        :: Columnar f Text,
    _petitionLocaleName        :: Columnar f Text,
    _petitionLocaleDescription :: Columnar f Text,
    _petitionLocaleLocale      :: Columnar f Text,

    _petitionLocalePetitionId  :: PrimaryKey PetitionT f
  }
  deriving Generic

type PetitionLocale = PetitionLocaleT Identity

type PetitionLocaleId = PrimaryKey PetitionLocaleT Identity
-- deriving instance Show (PrimaryKey PetitionT Identity)
-- deriving instance Show PetitionLocale

-- deriving instance Eq PetitionLocale

-- instance ToJSON (PetitionLocaleT Identity)
-- instance FromJSON (PetitionLocaleT Identity)


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
