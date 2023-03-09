{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Register (registerAction) where

import Web.Core (WRAction)
import Web.View.Start (startView)

registerAction :: WRAction a
registerAction = do
  startView (Just "登録しました。ログインしてください。")