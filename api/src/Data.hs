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

  Signer,
  Signer'(..),
  SignerField,
  SignerFieldMod,
  SignerForm(..)
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Opaleye (Field, SqlInt4, SqlText, SqlBool)

-- ** Petition

-- newtype PetitionId' a = PetitionId a deriving (Show, Eq, Generic)
-- $(makeAdaptorAndInstance "pPetitionId" ''PetitionId')
-- type PetitionIdField = PetitionId' (Field SqlInt4)
-- type PetitionId = PetitionId' Int 
-- instance ToJSON   PetitionId
-- instance FromJSON PetitionId

data Petition' a b c d e f = Petition { 
    _petitionId          :: a,
    _petitionCode        :: b,
    _petitionName        :: c,
    _petitionDescription :: d,
    _petitionContent     :: e,
    _petitionLocale      :: f
} deriving Generic
-- type Petition = Petition' PetitionId Text Text Text Text
type Petition = Petition' Int Text Text Text Text Text

type PetitionField = Petition' 
                      -- PetitionIdField
                      (Field SqlInt4) 
                      (Field SqlText) 
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

data PetitionLocale' a b c d e f = PetitionLocale { 
    _petitionLocaleId          :: a,
    _petitionLocaleName        :: b,
    _petitionLocaleDescription :: c,
    _petitionLocaleContent     :: d,
    _petitionLocaleLocale      :: e,
    _petitionLocalePetitionId  :: f
} deriving Generic
-- type PetitionLocale = PetitionLocale' PetitionLocaleId Text Text Text PetitionId
type PetitionLocale = PetitionLocale' Int Text Text Text Text Int

type PetitionLocaleField = PetitionLocale' 
                            -- PetitionLocaleIdField
                            (Field SqlInt4) 
                            (Field SqlText) 
                            (Field SqlText) 
                            (Field SqlText) 
                            (Field SqlText) 
                            -- PetitionIdField
                            (Field SqlInt4) 

deriving instance Show PetitionLocale
deriving instance Eq PetitionLocale

instance ToJSON   PetitionLocale
instance FromJSON PetitionLocale

-- ** Signer

data Signer' a b c d e f g h k l m n = Signer { 
    _signerId              :: a,
    _signerPetitionId      :: b,
    _signerFirstName       :: c,
    _signerLastName        :: d,
    _signerCountry         :: e,
    _signerCity            :: f, 
    _signerOrganization    :: g,
    _signerEmail           :: h,
    _signerPhone           :: k,
    _signerBirthYear       :: l,
    _signerGender          :: m,
    _signerNotifiesEnabled :: n
} deriving Generic
type Signer = Signer' Int Int Text Text Text Text Text Text Text Int Text Bool

type SignerField = Signer' 
                      (Field SqlInt4) 
                      (Field SqlInt4) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlInt4) 
                      (Field SqlText) 
                      (Field SqlBool) 

type SignerFieldMod = Signer' 
                      (Maybe (Field SqlInt4))
                      (Field SqlInt4) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlText) 
                      (Field SqlInt4) 
                      (Field SqlText) 
                      (Field SqlBool) 

deriving instance Show Signer
deriving instance Eq Signer

instance ToJSON   Signer
instance FromJSON Signer

-- ** SignerForm

data SignerForm = SignerForm { 
    _signerFormFirstName       :: Text,
    _signerFormLastName        :: Text,
    _signerFormCountry         :: Text,
    _signerFormCity            :: Text,
    _signerFormOrganization    :: Text,
    _signerFormEmail           :: Text,
    _signerFormPhone           :: Text,
    _signerFormBirthYear       :: Int,
    _signerFormGender          :: Text,
    _signerFormNotifiesEnabled :: Bool
} deriving Generic

instance ToJSON   SignerForm
instance FromJSON SignerForm
