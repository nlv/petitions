{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Base (
  getPetitionByCode,
  insertSigner,
  getSignersCount
  )  where

import GHC.Generics
import Data.Text
import Data
import System.IO
import qualified Database.PostgreSQL.Simple as Pg

import           Opaleye (Field, FieldNullable, matchNullable, isNull,
                         Table, table, tableField, selectTable,
                         Insert(..),
                         Update(..),
                         Delete(..),
                         rCount,
                         rReturning,                         
                         Select, SelectArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, sqlStrictText, aggregate, groupBy, sqlInt4, sqlBool,
                         count, avg, sum, leftJoin, runSelect, runInsert_,
                         showSqlForPostgres, Unpackspec,
                         SqlInt4, SqlText, SqlDate, SqlFloat8, SqlBool)

import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Arrow (returnA)

import GHC.Int (Int64)

-- ** Petition

$(makeAdaptorAndInstance "pPetition" ''Petition')

petitionTable :: Table PetitionField PetitionField
petitionTable = table "petitions"
                  (pPetition Petition {
                    -- _petitionId          = pPetitionId (PetitionId (tableField "id")),
                    _petitionId          = tableField "id",
                    _petitionCode        = tableField "code",
                    _petitionName        = tableField "name",
                    _petitionDescription = tableField "description",
                    _petitionContent     = tableField "content",
                    _petitionLocale      = tableField "locale"
                  })

-- type PetitionIdNullableField = PetitionId' (FieldNullable SqlInt4)

-- ** PetitionLocale

$(makeAdaptorAndInstance "pPetitionLocale" ''PetitionLocale')

petitionLocaleTable :: Table PetitionLocaleField PetitionLocaleField
petitionLocaleTable = table "petitions_locale"
                  (pPetitionLocale PetitionLocale {
                    -- _petitionLocaleId          = pPetitionLocaleId (PetitionLocaleId (tableField "id")),
                    _petitionLocaleId          = tableField "id",
                    _petitionLocaleName        = tableField "name",
                    _petitionLocaleDescription = tableField "description",
                    _petitionLocaleContent     = tableField "content",
                    _petitionLocaleLocale      = tableField "locale",
                    -- _petitionLocalePetitionId  = pPetitionId (PetitionId (tableField "petition_id"))
                    _petitionLocalePetitionId  = (tableField "petition_id")
                  })

-- type PetitionLocaleIdNullableField = PetitionLocaleId' (FieldNullable SqlInt4)

-- type PetitionLocaleNullableField = PetitionLocale' 
--                             PetitionLocaleIdNullableField
--                             (FieldNullable SqlText) 
--                             (FieldNullable SqlText) 
--                             (FieldNullable SqlText) 
--                             PetitionIdNullableField

-- Signer

$(makeAdaptorAndInstance "pSigner" ''Signer')

signerTable :: Table SignerFieldMod SignerField
signerTable = table "signers"
                  (pSigner Signer {
                    _signerId              = tableField "id",
                    _signerPetitionId      = tableField "petition_id",
                    _signerFirstName       = tableField "first_name",
                    _signerLastName        = tableField "last_name",
                    _signerCountry         = tableField "country",
                    _signerCity            = tableField "city",
                    _signerOrganization    = tableField "organization",
                    _signerEmail           = tableField "email",
                    _signerPhone           = tableField "phone",
                    _signerBirthYear       = tableField "birth_year",
                    _signerGender          = tableField "gender",
                    _signerNotifiesEnabled = tableField "notifies_enabled"
                  })


-- a :: Text -> Text -> Select Petition
-- a :: Text -> Text -> Select Text
getPetitionByCode :: Pg.Connection -> Text -> Maybe Text -> IO (Maybe Petition)
getPetitionByCode conn code locale = do
  ps <- runSelect conn $ proc () -> do
    petition <- selectTable petitionTable -< ()
    restrict -< _petitionCode petition .== sqlStrictText code
    returnA -< petition 
  case ps of
          p:ps' -> case locale of
                      Just l -> do
                          located <- getPetitionLocale conn (_petitionId p) l
                          case located of 
                            Just l' -> pure $ Just $ locatedPetition p l'
                            Nothing -> pure $ Just p
                        -- Just <$> (locatedPetition p) <$> getPetitionLocale conn (_petitionId p) l
                      _      -> pure (Just p)
          _     -> pure (Nothing :: Maybe Petition)
  where 
    locatedPetition :: Petition -> PetitionLocale -> Petition
    locatedPetition p'' l'' = 
      Petition 
        (_petitionId p'')
        (_petitionCode p'')
        (_petitionLocaleName l'')
        (_petitionLocaleDescription l'')
        (_petitionLocaleContent l'')
        (_petitionLocaleLocale l'')

getPetitionLocale :: Pg.Connection -> Int -> Text -> IO (Maybe PetitionLocale)
getPetitionLocale conn pid locale = do
  ps <- runSelect conn $ proc () -> do
    located <- selectTable petitionLocaleTable -< ()
    restrict -< _petitionLocaleLocale located .== sqlStrictText locale
    restrict -< _petitionLocalePetitionId located .== sqlInt4 pid
    returnA -< located 
  pure $ case ps of
            p:ps' -> Just p
            _         -> Nothing

insertSigner :: Pg.Connection -> Text -> SignerForm -> IO Bool
insertSigner conn code signerForm = do
  petition' <- getPetitionByCode conn code Nothing
  case petition' of
    Just petition -> do
      let signer = signerForm2Mod (_petitionId petition) signerForm
      runInsert_ conn Insert
          { iTable      = signerTable
          , iRows       = [signer]
          , iReturning  = rCount
          , iOnConflict = Nothing
          }
      pure True
    _ -> pure False

getSignersCount :: Pg.Connection -> Int -> IO Int
getSignersCount conn pid = do
  res <- (Pg.query 
            conn 
            "SELECT COUNT(*) cnt FROM signers WHERE petition_id = ?" 
            (Pg.Only pid)) :: IO [Pg.Only Int]
  case res of
     (Pg.Only x) : _ -> pure x
     _               -> pure 0

signerForm2Mod :: Int -> SignerForm -> SignerFieldMod
signerForm2Mod pid form = 
  Signer
    Nothing
    (sqlInt4 pid)
    (sqlStrictText $ _signerFormFirstName form)
    (sqlStrictText $ _signerFormLastName form)
    (sqlStrictText $ _signerFormCountry form)
    (sqlStrictText $ _signerFormCity form)
    (sqlStrictText $ _signerFormOrganization form)
    (sqlStrictText $ _signerFormEmail form)
    (sqlStrictText $ _signerFormPhone form)
    (sqlInt4 $ _signerFormBirthYear form)
    (sqlStrictText $ _signerFormGender form)
    (sqlBool $ _signerFormNotifiesEnabled form)


-- insertSigner :: SignerFieldMod -> Insert Int64
-- insertSigner signer = Insert
--   { iTable      = signerTable
--   , iRows       = [signer]
--   , iReturning  = rCount
--   , iOnConflict = Nothing
-- }

-- petitionLocaleLeftJoin :: Select (PetitionField, PetitionLocaleNullableField)                            
-- petitionLocaleLeftJoin = leftJoin selectPetition selectPetitionLocale eqId 
--   where 
--     selectPetition = selectTable petitionTable
--     selectPetitionLocale = selectTable petitionLocaleTable
--     eqId (p, pl) = _petitionId p .=== _petitionLocalePetitionId pl
  -- ps <- runSelect conn $ proc () -> do
  --   petition@(origin, located) <- petitionLocaleLeftJoin -< ()
  --   restrict -< _petitionCode origin .== sqlStrictText code
  --   -- returnA -< matchNullable origin (\_ -> origin) located
  --   returnA -< origin 
  -- pure $ case ps of
  --           p:ps' -> Just p
  --           _         -> Nothing
