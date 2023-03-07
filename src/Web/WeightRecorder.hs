{-# LANGUAGE OverloadedStrings #-}

module Web.WeightRecorder (weightRecorderMiddleware, runWeightRecorder) where

import Network.Wai
import Web.Core (WRAction, WRApp, WRConfig, WRContext, WRSession, WRState (WRState, wrstStartTemplate), emptyContext, emptySession)
import Web.Spock (get, getContext, prehook, root, runSpock, spock)
import Web.Spock.Config (PoolOrConn (PCNoDatabase), SpockCfg (SpockCfg), defaultSpockCfg)
import Web.View.Start (loadStartTemplate, startView)

spockApp :: WRApp () ()
spockApp =
  prehook
    (return emptyContext)
    $ do
      get root $ startView Nothing

weightRecorderMiddleware :: WRConfig -> IO Middleware
weightRecorderMiddleware cfg = do
  starttpl <- loadStartTemplate cfg
  let state = WRState {wrstStartTemplate = starttpl}
  spCfg <- defaultSpockCfg emptySession PCNoDatabase state
  spock spCfg spockApp

runWeightRecorder :: WRConfig -> IO ()
runWeightRecorder cfg = runSpock 8080 (weightRecorderMiddleware cfg)