{-# LANGUAGE OverloadedStrings #-}

module App (app, runApp) where

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import Data.Text qualified as T
import Web.Spock
import Web.Spock.Config
import Network.Wai (Middleware)

data MySession = EmptySession

data MyAppState = DummyAppState (IORef Int)

runApp :: IO()
runApp = runSpock 8080 app

app :: IO Middleware
app =
  do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    spock spockCfg routes

routes :: SpockM () MySession MyAppState ()
routes =
  do
    get root $
      text "Hello World!"
    get ("hello" <//> var) $ \name ->
      do
        (DummyAppState ref) <- getState
        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i + 1, i + 1)
        text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))