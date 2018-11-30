{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Data (
  Petition,
  PetitionExt,
  Petition'(..),
  PetitionField,
  PetitionId'(..),
  pPetitionId
  -- PetitionT(..),
  -- PetitionId,
  -- PetitionLocale,
  -- PetitionLocaleT(..),
  -- PetitionLocaleId
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Opaleye (Field, SqlInt4, SqlText)

-- ** Petition

newtype PetitionId' a = PetitionId a deriving (Show, Eq, Generic)
$(makeAdaptorAndInstance "pPetitionId" ''PetitionId')
type PetitionIdField = PetitionId' (Field SqlInt4)
type PetitionId = PetitionId' Int 
instance ToJSON   PetitionId
instance FromJSON PetitionId

data Petition' a b c d e = Petition { 
    _petitionId                :: a,
    _petitionCode              :: b,
    _petitionName              :: c,
    _petitionDescription       :: d,
    _petitionLocale            :: e
} deriving Generic
type Petition = Petition' PetitionId Text Text Text Text
type PetitionExt = Petition' Int Text Text Text Text

type PetitionField = Petition' 
                      PetitionIdField
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText)

deriving instance Show Petition
deriving instance Eq Petition

instance ToJSON   PetitionExt
instance FromJSON PetitionExt

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
