module Web.Core
  ( WRApp,
    WRAction,
    WRConfig (WRConfig, wrcDBPath, wrcTplRoots),
    WRState (WRState, wrstStartTemplate),
    WRContext (WRContext, wrconUser),
    emptyContext,
    WRSession (WRSession, wrsesUser),
    emptySession,
    runSqlite,
  )
where

import Control.Monad.IO.Class (liftIO)
import Database.HDBC.Sqlite3 (Connection)
import Entity.User qualified as User
import Text.Mustache (Template)
import Web.Spock (SpockActionCtx, SpockCtxM, runQuery)

-- TODO Add WRConnection
type WRApp ctx = SpockCtxM ctx WRConnection WRSession WRState

-- TODO Add WRConnection
type WRAction = SpockActionCtx WRContext WRConnection WRSession WRState

data WRConfig = WRConfig
  { wrcDBPath :: !FilePath,
    wrcTplRoots :: ![FilePath]
  }

data WRState = WRState
  {wrstStartTemplate :: !Template}

newtype WRContext = WRContext
  {wrconUser :: Maybe User.User}

emptyContext :: WRContext
emptyContext = WRContext Nothing

type WRConnection = Connection

newtype WRSession = WRSession
  { wrsesUser :: Maybe User.User
  }

emptySession :: WRSession
emptySession = WRSession Nothing

runSqlite :: (Connection -> IO m) -> WRAction m
runSqlite f = runQuery $ liftIO . f