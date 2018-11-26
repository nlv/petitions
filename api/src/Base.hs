{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE ImpredicativeTypes #-}

module Base (
  getPetitionByCode
  )  where

import GHC.Generics
import Database.Beam
import Data.Text
import Lens.Micro
import Data

type PetitionId = PrimaryKey PetitionT Identity

instance Beamable PetitionT

instance Table PetitionT where
  data PrimaryKey PetitionT f = PetitionId (Columnar f Int) deriving Generic
  primaryKey = PetitionId . _petitionId
instance Beamable (PrimaryKey PetitionT)

data PetitionDb f = PetitionDb
                      { _petitions :: f (TableEntity PetitionT) }
                        deriving Generic

instance Database be PetitionDb

petitionDb :: DatabaseSettings be PetitionDb
petitionDb = defaultDbSettings

Petition 
  (LensFor petitionId)    
  (LensFor petitionCode)    
  (LensFor petitionName)
  (LensFor petitionDescription)
  (LensFor petitionLocale) = tableLenses

PetitionDb 
  (TableLens petitions) = dbLenses

getPetitionByCode code = do
  ps <- runSelectReturningList $ select $ do
    p <- all_ (petitionDb ^. petitions)
    guard_ (p ^. petitionCode ==. val_ code )
    pure p
  pure $ 
    case ps of
      p:_ -> Just p
      _   -> Nothing
