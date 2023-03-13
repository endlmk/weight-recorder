{-# LANGUAGE OverloadedStrings #-}

module Web.Action.NewRecord (newRecordAction) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Entity.User qualified as User
import Model.WeightRecord (NewWRecord (NewWRecord), insertNewWRecord)
import Web.Core (WRAction, WRContext (wrconUser), runSqlite)
import Web.Spock (getContext, param, redirect)
import Web.View.Main (mainView)

newRecordAction :: WRAction a
newRecordAction = do
  mWeight <- param "weight"
  case mWeight of
    Nothing -> mainView (Just "入力されていない項目があります")
    Just weight -> do
      u <- wrconUser <$> getContext
      case u of
        Nothing -> error "No user data in context"
        Just user -> do
          now <- liftIO (utcToLocalTime utc <$> getCurrentTime)
          let r = NewWRecord (User.id user) now weight
          n <- runSqlite $ insertNewWRecord r
          when (n == 0) $ mainView (Just "記録に失敗しました")
          redirect "/"
