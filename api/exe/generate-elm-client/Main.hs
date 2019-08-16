{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Elm (Spec (Spec), specsToDir, toElmDecoderSource, toElmEncoderSource, toElmTypeSource, ElmType)
import Servant.Elm -- (ElmType, Proxy (Proxy), defElmImports, generateElmForAPI)
import Data
import Api


-- instance ElmType PetitionId 
-- where
--     toElmType (PetitionId x) = ElmPrimitive EInt _
instance ElmType Petition
instance ElmType SignerForm

spec :: Spec
spec = Spec ["Generated", "Api"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Petition)
             : toElmDecoderSource (Proxy :: Proxy Petition)
             : toElmTypeSource    (Proxy :: Proxy SignerForm)
             : toElmDecoderSource (Proxy :: Proxy SignerForm)
             : toElmEncoderSource  (Proxy :: Proxy SignerForm)
            --  : toElmTypeSource    (Proxy :: Proxy PetitionId)
            --  : toElmDecoderSource (Proxy :: Proxy PetitionId)
             : generateElmForAPI  (Proxy :: Proxy RestApi))


main :: IO ()
main = specsToDir [spec] "../client/src"
