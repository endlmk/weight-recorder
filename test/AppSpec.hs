{-# LANGUAGE OverloadedStrings #-}

module AppSpec (spec) where

-- import App (app)

import qualified Data.ByteString.Lazy.UTF8 as LBS
import Network.Wai.Test
import Paths_weight_recorder (getDataDir)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Wai
import Web.Core (WRConfig (WRConfig, wrcTplRoots))
import Web.Spock (spockAsApp)
import Web.WeightRecorder (weightRecorderMiddleware)

-- main :: IO ()
-- main = hspec spec

spec :: Spec
spec =
  --   m = weightRecorderMiddleware cfg
  do
    describe "GET /" $
      -- with (spockAsApp $ weightRecorderMiddleware cfg) $
      --   do
      --     it "serves the home page" $
      --       get "/" `shouldRespondWith` 200
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
