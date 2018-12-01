{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Data (
  Petition,
  Petition'(..),
  PetitionField,
  -- PetitionId'(..),
  -- PetitionId,
  -- pPetitionId,

  PetitionLocale,
  PetitionLocale'(..),
  PetitionLocaleField,
  -- PetitionLocaleId'(..),
  -- PetitionLocaleId,
  -- pPetitionLocaleId
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Opaleye (Field, SqlInt4, SqlText)

-- ** Petition

-- newtype PetitionId' a = PetitionId a deriving (Show, Eq, Generic)
-- $(makeAdaptorAndInstance "pPetitionId" ''PetitionId')
-- type PetitionIdField = PetitionId' (Field SqlInt4)
-- type PetitionId = PetitionId' Int 
-- instance ToJSON   PetitionId
-- instance FromJSON PetitionId

data Petition' a b c d e = Petition { 
    _petitionId                :: a,
    _petitionCode              :: b,
    _petitionName              :: c,
    _petitionDescription       :: d,
    _petitionLocale            :: e
} deriving Generic
-- type Petition = Petition' PetitionId Text Text Text Text
type Petition = Petition' Int Text Text Text Text

type PetitionField = Petition' 
                      -- PetitionIdField
                      (Field SqlInt4) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText)

deriving instance Show Petition
deriving instance Eq Petition

instance ToJSON   Petition
instance FromJSON Petition

-- ** PetitionLocale

-- newtype PetitionLocaleId' a = PetitionLocaleId a deriving (Show, Eq, Generic)
-- $(makeAdaptorAndInstance "pPetitionLocaleId" ''PetitionLocaleId')
-- type PetitionLocaleIdField = PetitionLocaleId' (Field SqlInt4)
-- type PetitionLocaleId = PetitionLocaleId' Int 
-- instance ToJSON   PetitionLocaleId
-- instance FromJSON PetitionLocaleId

data PetitionLocale' a b c d e = PetitionLocale { 
    _petitionLocaleId          :: a,
    _petitionLocaleName        :: b,
    _petitionLocaleDescription :: c,
    _petitionLocaleLocale      :: d,
    _petitionLocalePetitionId  :: e
} deriving Generic
-- type PetitionLocale = PetitionLocale' PetitionLocaleId Text Text Text PetitionId
type PetitionLocale = PetitionLocale' Int Text Text Text Int

type PetitionLocaleField = PetitionLocale' 
                            -- PetitionLocaleIdField
                            (Field SqlInt4) 
                            (Field SqlText) 
                            (Field SqlText) 
                            (Field SqlText) 
                            -- PetitionIdField
                            (Field SqlInt4) 

deriving instance Show PetitionLocale
deriving instance Eq PetitionLocale

instance ToJSON   PetitionLocale
instance FromJSON PetitionLocale




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
