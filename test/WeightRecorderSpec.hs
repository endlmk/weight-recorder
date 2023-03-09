{-# LANGUAGE OverloadedStrings #-}

module WeightRecorderSpec (spec) where

import Data.ByteString.Lazy.UTF8 qualified as LBS
import Network.Wai.Test
import Paths_weight_recorder (getDataDir)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Wai
import Web.Core (WRConfig (WRConfig, wrcTplRoots))
import Web.Spock (spockAsApp)
import Web.WeightRecorder (weightRecorderMiddleware)

spec :: Spec
spec =
  do
    describe "GET /"
      $ with
        ( do
            dataDir <- getDataDir
            let cfg = WRConfig {wrcTplRoots = [dataDir </> "templates"]}
            spockAsApp (weightRecorderMiddleware cfg)
        )
      $ do
        it "serves the home page" $
          get "/" `shouldRespondWith` 200
    describe "GET with Network.WAi.Test" $
      it "Get home" $
        do
          dataDir <- getDataDir
          let cfg = WRConfig {wrcTplRoots = [dataDir </> "templates"]}
              m = weightRecorderMiddleware cfg
          appl <- spockAsApp $ m
          flip runSession appl $
            do
              response <- Network.Wai.Test.request (setPath defaultRequest "/")
              assertStatus 200 response
              assertBodyContains (LBS.fromString "ユーザー登録") response
              assertBodyContains (LBS.fromString "ログイン") response
    describe "POST"
      $ with
        ( do
            dataDir <- getDataDir
            let cfg = WRConfig {wrcTplRoots = [dataDir </> "templates"]}
            spockAsApp (weightRecorderMiddleware cfg)
        )
      $ do
        it "can register user" $
          post "/register" "name=hoge&password=hage" `shouldRespondWith` 200
