{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE ImpredicativeTypes #-}
{-#  LANGUAGE AllowAmbiguousTypes #-}
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
         _petitionLocaleLocale      = fieldNamed "locale",
         _petitionLocalePetitionId = PetitionId "petition_id"
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
  (LensFor petitionLocaleCode)    
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
        l <- all_ (petitionDb ^. petitionsLocale)
        -- l <- leftJoin_ (all_ (petitionDb ^. petitionsLocale))
                      --  (\l' -> _petitionLocalePetitionId l' `references_` p)
        guard_ (_petitionLocalePetitionId l `references_` p)
        guard_ (p ^. petitionCode ==. val_ code)
        guard_ (l ^. petitionLocaleLocale ==. val_ locale')
        pure p
        -- pure (p, l)
        -- pure $ case l of
        --   Just l' -> undefined
        --     -- Petition 
        --     --   p ^. petitionId
        --     --   code
        --     --   l' ^. petitionLocaleName
        --     --   l' ^. petitionLocaleDescription
        --     --   l' ^. petitionLocaleLocale
        --   _ -> p
      Nothing -> do
        p <- all_ (petitionDb ^. petitions)
        guard_ (p ^. petitionCode ==. val_ code)
        pure p
        -- pure (p, undefined)
  pure $ 
    case ps of
      p:_ -> Just p
      _   -> Nothing
