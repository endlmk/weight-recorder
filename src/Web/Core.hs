module Web.Core
  ( WRApp,
    WRAction,
    WRConfig (WRConfig, wrcTplRoots),
    WRState (WRState, wrstStartTemplate),
    WRContext (WRContext, wrconUser),
    emptyContext,
    WRSession (WRSession, wrsesUser),
    emptySession,
  )
where

import Database.HDBC.Sqlite3 (Connection)
import Entity.User qualified as User
import Text.Mustache (Template)
import Web.Spock (SpockActionCtx, SpockCtxM)

-- TODO Add WRConnection
type WRApp ctx = SpockCtxM ctx () WRSession WRState

-- TODO Add WRConnection
type WRAction = SpockActionCtx WRContext () WRSession WRState

data WRConfig = WRConfig
  {wrcTplRoots :: ![FilePath]}

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