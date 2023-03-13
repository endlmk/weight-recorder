{-# LANGUAGE OverloadedStrings #-}

module Web.View.Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as TXT
import Data.Time qualified as TM
import Entity.User qualified as User
import Entity.WeightRecord qualified as WRecord
import Model.WeightRecord (selectWRecord)
import Text.Mustache (Template, automaticCompile, object, substitute, (~>))
import Text.Mustache.Types (Value)
import Web.Core (WRAction, WRConfig (wrcTplRoots), WRState (wrstMainTemplate), runSqlite, wrconUser)
import Web.Spock (getContext, getState, html)

loadMainTemplate :: WRConfig -> IO Template
loadMainTemplate cfg = do
  compiled <- automaticCompile (wrcTplRoots cfg) "main.mustache"
  case compiled of
    Left err -> error (show err)
    Right template -> return template

mainView :: Maybe TXT.Text -> WRAction a
mainView mMes =
  do
    u <- wrconUser <$> getContext
    case u of
      Just user -> do
        uv <- userValue user
        tpl <- wrstMainTemplate <$> getState
        rs <- runSqlite $ selectWRecord (User.id user)
        rvs <- mapM weightRecordValue rs
        let v =
              object $
                appendMessage
                  mMes
                  ["user" ~> uv, "records" ~> rvs]
        html $ substitute tpl v
        where
          appendMessage (Just mes) ps = "message" ~> mes : ps
          appendMessage Nothing ps = ps
      _ -> error "No user data in context"

userValue :: User.User -> WRAction Value
userValue u = return $ object ["id" ~> User.id u, "name" ~> User.name u]

weightRecordValue :: WRecord.WeightRecord -> WRAction Value
weightRecordValue wr = do
  ztime <- liftIO $ toZonedTime $ WRecord.time wr
  return $ object ["time" ~> show ztime, "weight" ~> WRecord.weight wr]

toZonedTime :: TM.LocalTime -> IO TM.ZonedTime
toZonedTime = TM.utcToLocalZonedTime . TM.localTimeToUTC TM.utc