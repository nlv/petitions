{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Elm (Spec (Spec), specsToDir, toElmDecoderSource, toElmTypeSource)
import Servant.Elm  (ElmType, Proxy (Proxy), defElmImports, generateElmForAPI)
import Data
import Api

instance ElmType Petition

spec :: Spec
spec = Spec ["Generated", "Api"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Petition)
             : toElmDecoderSource (Proxy :: Proxy Petition)
             : generateElmForAPI  (Proxy :: Proxy Api))


main :: IO ()
main = specsToDir [spec] "../client/src"
