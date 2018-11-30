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
  PetitionLocaleId,
  Signer,
  SignerT(..),
  SignerId,
  SignerForm
  )  where

import GHC.Generics
import Data.Aeson
import Data.Text
-- import GHC.Generics
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


data SignerT f
  = Signer {
    _signerId                 :: Columnar f Int--,
    -- _singerFirstName          :: Columnar f Text--,
    -- _singerLastName           :: Columnar f Text,
    -- _signerCountry            :: Columnar f Text,
    -- _signerOrganization       :: Columnar f Text,
    -- _signerEmail              :: Columnar f Text,
    -- _signerPhone              :: Columnar f Text,
    -- _signerBirthYear          :: Columnar f Int,
    -- _signerGender             :: Columnar f Int,
    -- _signerNotifiesEnabled    :: Columnar f Bool --,

    -- _signerPetitionId :: PrimaryKey PetitionT f
  }
  deriving Generic

type Signer = SignerT Identity

-- deriving instance Show Signer
-- deriving instance Eq Signer

-- instance ToJSON (SignerT Identity)
-- instance FromJSON (SignerT Identity)

type SignerId = PrimaryKey SignerT Identity

data SignerForm
  = SignerForm {
    _singerFormFirstName          :: Text,
    _singerFormLastName           :: Text,
    _signerFormCountry            :: Text,
    _signerFormOrganization       :: Text,
    _signerFormEmail              :: Text,
    _signerFormPhone              :: Text,
    _signerFormBirthYear          :: Int,
    _signerFormGender             :: Int,
    _signerFormNotifiesEnabled    :: Bool
  }
  deriving Generic


instance ToJSON (SignerForm)
instance FromJSON (SignerForm)