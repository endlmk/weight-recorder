{-# LANGUAGE OverloadedStrings #-}

module Web.View.Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Text qualified as TXT
import Data.Time qualified as TM
import Data.Time.Format.ISO8601 (zonedTimeFormat)
import Entity.User qualified as User
import Entity.WeightRecord qualified as WRecord
import Model.WeightRecord (selectWRecord)
import Text.Mustache (Template, automaticCompile, object, substitute, (~>))
import Text.Mustache.Types (Value)
import Web.Core (WRAction, WRConfig (wrcTplRoots), WRSession (wrsesUser), WRState (wrstMainTemplate), runSqlite, wrconUser)
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
        rs <- runSqlite $ selectWRecord (User.id user)
        rvs <- mapM weightRecordValue rs
        wgv <- weightGraphValues rs
        tpl <- wrstMainTemplate <$> getState
        let v =
              object $
                appendMessage
                  mMes
                  ["user" ~> uv, "records" ~> rvs, "graphs" ~> wgv]
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

weightGraphValues :: [WRecord.WeightRecord] -> WRAction [Value]
weightGraphValues wrs = do
  flatWrs <- liftIO $ mapM flat wrs
  let wrss = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ flatWrs
  return $ map groupToValue wrss
  where
    flat wr = do
      ztime <- toZonedTime $ WRecord.time wr
      let ztimeStr = TM.formatTime TM.defaultTimeLocale "%m/%d" ztime
          w = WRecord.weight wr
      return (ztimeStr, w)
    groupToValue gr =
      let dt = head $ map fst gr
          wt = avg $ map snd gr
          avg xs = sum xs / fromIntegral (length xs)
       in object ["day" ~> dt, "weight" ~> wt]
