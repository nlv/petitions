{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Elm (Spec (Spec), specsToDir, toElmDecoderSource, toElmTypeSource)
import Servant.Elm  (ElmType, Proxy (Proxy), defElmImports, generateElmForAPI)
import Data
import Api

instance ElmType PetitionExt

spec :: Spec
spec = Spec ["Generated", "Api"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy PetitionExt)
             : toElmDecoderSource (Proxy :: Proxy PetitionExt)
             : generateElmForAPI  (Proxy :: Proxy Api))


main :: IO ()
main = specsToDir [spec] "../client/src"
