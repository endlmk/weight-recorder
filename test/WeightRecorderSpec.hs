{-# LANGUAGE OverloadedStrings #-}

module WeightRecorderSpec (spec) where

import Data.ByteString.Lazy.UTF8 qualified as LBS
import Network.Wai (Application)
import Network.Wai.Test
import Paths_weight_recorder (getDataDir)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempFile)
import System.Process (callCommand)
import Test.Hspec
import Test.Hspec.Wai
import Web.Core (WRConfig (..))
import Web.Spock (spockAsApp)
import Web.WeightRecorder (weightRecorderMiddleware)

sqlFile :: IO FilePath
sqlFile = do
  datadir <- getDataDir
  return $ datadir </> "data" </> "schema.sql"

getAppWithConfig :: IO Application
getAppWithConfig = withSystemTempFile "test.db" $
  \path _ ->
    do
      sql <- sqlFile
      callCommand $ "sqlite3 " ++ path ++ " < " ++ sql
      dataDir <- getDataDir
      let cfg = WRConfig {wrcDBPath = (dataDir </> "data" </> "schema.sql"), wrcTplRoots = [dataDir </> "templates"]}
          m = weightRecorderMiddleware cfg
      spockAsApp m

spec :: Spec
spec =
  do
    describe "GET /" $
      with getAppWithConfig $
        do
          it "serves the home page" $
            get "/" `shouldRespondWith` 200
    describe "GET with Network.WAi.Test" $
      it "Get home" $
        do
          appl <- getAppWithConfig
          flip runSession appl $
            do
              response <- Network.Wai.Test.request (setPath defaultRequest "/")
              assertStatus 200 response
              assertBodyContains (LBS.fromString "ユーザー登録") response
              assertBodyContains (LBS.fromString "ログイン") response
    describe "POST" $
      with getAppWithConfig $
        do
          it "can register user" $
            postHtmlForm "/register" [("name", "hoge"), ("password", "hage")] `shouldRespondWith` 200
