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
  getPetitionByCode
  )  where

import GHC.Generics
import Data.Text
import Data
import System.IO

import           Opaleye (Field, FieldNullable, matchNullable, isNull,
                         Table, table, tableField, selectTable,
                         Select, SelectArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, sqlStrictText, aggregate, groupBy, 
                         count, avg, sum, leftJoin, runSelect,
                         showSqlForPostgres, Unpackspec,
                         SqlInt4, SqlText, SqlDate, SqlFloat8, SqlBool)

import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Arrow (returnA)

$(makeAdaptorAndInstance "pPetition" ''Petition')

petitionTable :: Table PetitionField PetitionField
petitionTable = table "petitions"
                  (pPetition Petition {
                    _petitionId          = pPetitionId (PetitionId (tableField "id")),
                    _petitionCode        = tableField "code",
                    _petitionName        = tableField "name",
                    _petitionDescription = tableField "description",
                    _petitionLocale      = tableField "locale"
                  })

-- a :: Text -> Text -> Select Petition
-- a :: Text -> Text -> Select Text
getPetitionByCode conn code locale = do
  ps <- runSelect conn $ proc () -> do
    petition <- selectTable petitionTable -< ()
    restrict -< _petitionCode petition .== sqlStrictText code
    returnA -< petition
  pure $ case ps of
            p:ps' -> Just p
            _     -> Nothing

getPetitionByCode' :: Text -> Text -> Select Petition
getPetitionByCode' code locale = undefined
  -- ps <- runSelectReturningList $ select $ do
  --   case locale of 
  --     (Just locale') -> do
  --       p <- all_ (petitionDb ^. petitions)
  --       l <- leftJoin_ (all_ (petitionDb ^. petitionsLocale))
  --                      (\l' -> (_petitionLocalePetitionId l') `references_` p &&. 
  --                       l' ^. petitionLocaleLocale ==. (val_ locale')) 
  --       guard_ (p ^. petitionCode ==. val_ code)
  --       pure (p, l)
  --     Nothing -> do
  --       p <- all_ (petitionDb ^. petitions)
  --       guard_ (p ^. petitionCode ==. val_ code)
  --       pure (p, nothing_)
  -- pure $ 
  --   case ps of
  --     (p, Nothing):_ -> Just p
  --     (p, Just l''):_ -> Just $
  --       Petition 
  --         (p ^. petitionId) 
  --         (p ^. petitionCode) 
  --         (l'' ^. petitionLocaleName) 
  --         (l'' ^. petitionLocaleDescription) 
  --         (l'' ^. petitionLocaleLocale) 
  --     _   -> Nothing
