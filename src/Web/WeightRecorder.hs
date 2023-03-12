{-# LANGUAGE OverloadedStrings #-}

module Web.WeightRecorder (weightRecorderMiddleware, runWeightRecorder) where

import Data.Pool (Pool, createPool)
import Database.HDBC (IConnection (disconnect))
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Network.Wai
import Web.Action.Login (loginAction)
import Web.Action.Register (registerAction)
import Web.Core (WRAction, WRApp, WRConfig (..), WRContext (wrconUser), WRSession (wrsesUser), WRState (WRState, wrstMainTemplate, wrstStartTemplate), emptyContext, emptySession)
import Web.Spock (get, getContext, html, post, prehook, readSession, root, runSpock, spock)
import Web.Spock.Config (PoolOrConn (PCPool), defaultSpockCfg)
import Web.View.Main (loadMainTemplate, mainView)
import Web.View.Start (loadStartTemplate, startView)

spockApp :: WRApp () ()
spockApp =
  prehook (return emptyContext) $
    do
      prehook authHook $
        do get root $ mainView Nothing
      post "register" registerAction
      post "login" loginAction

weightRecorderMiddleware :: WRConfig -> IO Middleware
weightRecorderMiddleware cfg = do
  starttpl <- loadStartTemplate cfg
  maintpl <- loadMainTemplate cfg
  let state = WRState {wrstStartTemplate = starttpl, wrstMainTemplate = maintpl}
  pool <- sqlitePool $ wrcDBPath cfg
  spCfg <- defaultSpockCfg emptySession (PCPool pool) state
  spock spCfg spockApp

runWeightRecorder :: WRConfig -> IO ()
runWeightRecorder cfg = runSpock 8080 (weightRecorderMiddleware cfg)

sqlitePool :: FilePath -> IO (Pool Connection)
sqlitePool dbPath = createPool (connectSqlite3 dbPath) disconnect 1 60 5

authHook :: WRAction WRContext
authHook = do
  ctx <- getContext
  mUser <- fmap wrsesUser readSession
  case mUser of
    Nothing -> startView Nothing
    Just user -> return $ ctx {wrconUser = Just user}
