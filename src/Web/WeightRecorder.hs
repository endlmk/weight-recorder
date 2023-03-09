{-# LANGUAGE OverloadedStrings #-}

module Web.WeightRecorder (weightRecorderMiddleware, runWeightRecorder) where

import Data.Pool (Pool, createPool)
import Database.HDBC (IConnection (disconnect))
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Network.Wai
import Web.Action.Register (registerAction)
import Web.Core (WRAction, WRApp, WRConfig (..), WRContext, WRSession, WRState (WRState, wrstStartTemplate), emptyContext, emptySession)
import Web.Spock (get, getContext, post, prehook, root, runSpock, spock)
import Web.Spock.Config (PoolOrConn (PCNoDatabase, PCPool), SpockCfg (SpockCfg), defaultSpockCfg)
import Web.View.Start (loadStartTemplate, startView)

spockApp :: WRApp () ()
spockApp =
  prehook
    (return emptyContext)
    $ do
      get root $ startView Nothing
      post "register" registerAction

weightRecorderMiddleware :: WRConfig -> IO Middleware
weightRecorderMiddleware cfg = do
  starttpl <- loadStartTemplate cfg
  let state = WRState {wrstStartTemplate = starttpl}
  pool <- sqlitePool $ wrcDBPath cfg
  spCfg <- defaultSpockCfg emptySession (PCPool pool) state
  spock spCfg spockApp

runWeightRecorder :: WRConfig -> IO ()
runWeightRecorder cfg = runSpock 8080 (weightRecorderMiddleware cfg)

sqlitePool :: FilePath -> IO (Pool Connection)
sqlitePool dbPath = createPool (connectSqlite3 dbPath) disconnect 1 60 5