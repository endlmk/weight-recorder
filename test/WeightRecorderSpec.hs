{-# LANGUAGE OverloadedStrings #-}

module WeightRecorderSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Network.HTTP.Types (hContentType, methodGet)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai (Application, Request (requestHeaders, requestMethod))
import Network.Wai.Test
import Paths_weight_recorder (getDataDir)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Process (callCommand)
import Test.Hspec
import Web.Core (WRConfig (..))
import Web.Spock (spockAsApp)
import Web.WeightRecorder (weightRecorderMiddleware)

sqlFile :: IO FilePath
sqlFile = do
  datadir <- getDataDir
  return $ datadir </> "data" </> "schema.sql"

dbFile :: IO FilePath
dbFile = do
  tempDir <- getCanonicalTemporaryDirectory
  return $ tempDir </> "test.db"

createDB :: IO ()
createDB = do
  sql <- sqlFile
  path <- dbFile
  callCommand $ "sqlite3 " ++ path ++ " < " ++ sql

deleteDB :: IO ()
deleteDB = do
  path <- dbFile
  removeFile path

getAppWithConfig :: IO Application
getAppWithConfig =
  do
    dbPath <- dbFile
    dataDir <- getDataDir
    let cfg = WRConfig {wrcDBPath = dbPath, wrcTplRoots = [dataDir </> "templates"]}
        m = weightRecorderMiddleware cfg
    spockAsApp m

get :: BS.ByteString -> Session SResponse
get url = request $ setPath defaultRequest {requestMethod = methodGet} url

post :: BS.ByteString -> LBS.ByteString -> Session SResponse
post url body = srequest $ SRequest req body
  where
    req =
      setPath
        defaultRequest
          { requestMethod = methodPost,
            requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
          }
        url

bodyContains :: String -> SResponse -> Session ()
bodyContains = assertBodyContains . LBS.fromString

spec :: Spec
spec =
  before_ createDB $
    after_ deleteDB $
      do
        describe "GET with Network.WAi.Test" $
          it "Get home" $
            do
              appl <- getAppWithConfig
              flip runSession appl $
                do
                  response <- get "/"
                  assertStatus 200 response
                  bodyContains "ユーザー登録" response
                  bodyContains "ログイン" response
                  assertBodyContains (LBS.fromString "ログイン") response
        describe "Post with Network.WAi.Test" $
          it "Post Register user and Login" $
            do
              appl <- getAppWithConfig
              flip runSession appl $
                do
                  response <- post "/register" "name=hoge&password=hage"
                  assertStatus 200 response
                  bodyContains "登録しました。ログインしてください。" response
                  response2 <- post "/login" "name=hoge&password=hage"
                  assertStatus 200 response2
