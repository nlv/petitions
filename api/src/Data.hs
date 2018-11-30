{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data (
  Petition(..),
  Petition'(..),
  PetitionField
  -- PetitionT(..),
  -- PetitionId,
  -- PetitionLocale,
  -- PetitionLocaleT(..),
  -- PetitionLocaleId
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text

import Opaleye (Field, SqlInt4, SqlText)

-- ** Petition

data Petition' a b c d e = Petition' { 
    _petitionId                :: a,
    _petitionCode              :: b,
    _petitionName              :: c,
    _petitionDescription       :: d,
    _petitionLocale            :: e
} deriving Generic
type Petition = Petition' Int Text Text Text Text
type PetitionField = Petition' 
                      (Field SqlInt4)
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText)

deriving instance Show Petition
deriving instance Eq Petition

instance ToJSON   Petition
instance FromJSON Petition

-- instance ToJSON   (Petition' Int Text Text Text Text)
-- instance FromJSON (Petition' Int Text Text Text Text)

-- data PetitionLocaleT f
--   = PetitionLocale {
--     _petitionLocaleId          :: Columnar f Int,
--     _petitionLocaleName        :: Columnar f Text,
--     _petitionLocaleDescription :: Columnar f Text,
--     _petitionLocaleLocale      :: Columnar f Text,

--     _petitionLocalePetitionId  :: PrimaryKey PetitionT f
--   }
--   deriving Generic



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
