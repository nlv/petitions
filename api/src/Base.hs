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
import Data.Maybe
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


instance Table SignerT where
  data PrimaryKey SignerT f = SignerId (Columnar f Int) deriving Generic
  primaryKey = SignerId . _signerId

instance Beamable SignerT
instance Beamable (PrimaryKey SignerT)

data PetitionDb f = PetitionDb
                      { _petitions       :: f (TableEntity PetitionT) 
                      , _petitionsLocale :: f (TableEntity PetitionLocaleT)
                      , _signers         :: f (TableEntity SignerT)
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

Signer 
  (LensFor singerId) = tableLenses
  -- (LensFor petitionFirstName) = tableLenses    
  -- (LensFor signerLastName)
  -- (LensFor signerCountry)
  -- (LensFor signerOrganization)
  -- (LensFor signerEmail)
  -- (LensFor signerPhone)
  -- (LensFor signerBirthYear)
  -- (LensFor signerGender)
  -- (LensFor signerNotifiesEnabled) = tableLenses
  -- (PetitionId (LensFor signerId)) = tableLenses

PetitionDb 
  (TableLens petitions) 
  (TableLens petitionsLocale) 
  (TableLens signers) = dbLenses

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

-- insertSigner :: SignerForm -> Text -> IO Int
-- insertSigner signerForm code = do
--   conn <- Pg.connectPostgreSQL "dbname=petitions" 
--   -- runBeamPostgresDebug putStrLn conn (B.getPetitionByCode code locale)
--   runBeamPostgres conn $ do
--     -- petition' <- getPetitionByCode code Nothing
--     let petition' = Nothing
--     x <- case petition' of
--       Just petition -> do 
--         let sx = [ (Signer default_)
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- undefined
--                         -- (val_ "_signerFormFirstName signerForm")
--                         -- (val_ $ "_signerFormLastName signerForm")
--                         -- (val_ $ "_signerFormCountry signerForm")
--                         -- (val_ $ "_signerFormOrganization signerForm")
--                         -- (val_ $ "_signerFormEmail signerForm")
--                         -- (val_ $ "_signerFormPhone signerForm")
--                         -- (val_ $ 2) -- "_signerFormBirthYear signerForm")
--                         -- (val_ $ 3) -- "_signerFormGender signerForm")
--                         -- (val_ $ "_signerFormNotifiesEnabled signerForm")
--                         -- -- (pk petition)
--                       ] :: (QExpr (Sql92InsertValuesExpressionSyntax Sql92InsertValuesSyntax _ _))
--                       --s')-- :: [QGenExpr _ _ _ _] -- ctxt0 expr0 s0 a0]
--         runInsert $
--           insert (petitionDb ^. signers) $
--           insertExpressions sx
--     pure 2
--   pure 0
  


