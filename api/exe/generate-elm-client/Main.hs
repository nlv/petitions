{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Data.Text
-- import           Servant
import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmTypeSource)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI)
-- import           System.IO
import           Data

instance ElmType Petition

spec :: Spec
spec = Spec ["Generated", "Api"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Petition)
             : toElmDecoderSource (Proxy :: Proxy Petition)
             : generateElmForAPI  (Proxy :: Proxy Api))


main :: IO ()
main = specsToDir [spec] "../client"
