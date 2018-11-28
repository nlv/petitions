{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Base (
  getPetitionByCode
  )  where

import GHC.Generics
import Database.Beam
import Data.Text
import Lens.Micro
import Data

import System.IO

instance Beamable PetitionT

instance Table PetitionT where
  data PrimaryKey PetitionT f = PetitionId (Columnar f Int) deriving Generic
  primaryKey = PetitionId . _petitionId
instance Beamable (PrimaryKey PetitionT)

instance Beamable PetitionLocaleT

instance Table PetitionLocaleT where
  data PrimaryKey PetitionLocaleT f = PetitionLocaleId (Columnar f Int) deriving Generic
  primaryKey = PetitionLocaleId . _petitionLocaleId
instance Beamable (PrimaryKey PetitionLocaleT)

data PetitionDb f = PetitionDb
                      { _petitions       :: f (TableEntity PetitionT) 
                      , _petitionsLocale :: f (TableEntity PetitionLocaleT)
                      }
                        deriving Generic

instance Database be PetitionDb

petitionDb :: DatabaseSettings be PetitionDb
petitionDb = defaultDbSettings `withDbModification`
  dbModification {
    _petitionsLocale =
      modifyTable (\_ -> "petitions_locale") $ 
      tableModification {
         _petitionLocaleId          = fieldNamed "id",
         _petitionLocaleName        = fieldNamed "name",
         _petitionLocaleDescription = fieldNamed "description",
         _petitionLocaleLocale      = fieldNamed "locale",
         _petitionLocalePetitionId  = PetitionId "petition_id"
      }
  }

Petition 
  (LensFor petitionId)    
  (LensFor petitionCode)    
  (LensFor petitionName)
  (LensFor petitionDescription)
  (LensFor petitionLocale) = tableLenses

PetitionLocale
  (LensFor petitionLocaleId)    
  (LensFor petitionLocaleName)
  (LensFor petitionLocaleDescription)
  (LensFor petitionLocaleLocale)
  (PetitionId (LensFor petitionLocalePetitionId)) = tableLenses

PetitionDb 
  (TableLens petitions) 
  (TableLens petitionsLocale) = dbLenses

getPetitionByCode code locale = do
  ps <- runSelectReturningList $ select $ do
    case locale of 
      (Just locale') -> do
        p <- all_ (petitionDb ^. petitions)
        l <- leftJoin_ (all_ (petitionDb ^. petitionsLocale))
                       (\l' -> (_petitionLocalePetitionId l') `references_` p &&. 
                        l' ^. petitionLocaleLocale ==. (val_ locale')) 
        guard_ (p ^. petitionCode ==. val_ code)
        pure (p, l)
      Nothing -> do
        p <- all_ (petitionDb ^. petitions)
        guard_ (p ^. petitionCode ==. val_ code)
        pure (p, nothing_)
  pure $ 
    case ps of
      (p, Nothing):_ -> Just p
      (p, Just l''):_ -> Just $
        Petition 
          (p ^. petitionId) 
          (p ^. petitionCode) 
          (l'' ^. petitionLocaleName) 
          (l'' ^. petitionLocaleDescription) 
          (l'' ^. petitionLocaleLocale) 
      _   -> Nothing
